# MetaImpact Server #
#-------------------#

#----------------#
# load libraries #
#----------------#
library(tidyverse)
library(metafor)
library(ggplot2)
library(tidyr)
library(tidybayes)
library(dplyr)
library(glue)
library(forcats)
library(rstan)
library(MetaStan)
library(purrr)


#----------------#
# Server Content #
#----------------#
function(input, output, session) {
  
  
  data <- dataPageServer("Data")

  
  optionsReactives <- optionsPanelServer("optionsPanel",data)
  
  Pair_ctrl=optionsReactives$Pair_ctrl
  Pair_trt=optionsReactives$Pair_trt
  FixRand=optionsReactives$FixRand
  OutcomeCont=optionsReactives$OutcomeCont
  OutcomeBina=optionsReactives$OutcomeBina
  prior=optionsReactives$prior
  chains=optionsReactives$chains
  iter=optionsReactives$iter
  burn=optionsReactives$burn
  
  ContBin <- reactive({           # automatically detect if continuous or binary
    if (max(grepl("^Mean", names(data()$data)))==TRUE) {
      return('continuous')
    } else if (max(grepl("^R", names(data()$data)))==TRUE) {
      return ('binary')
    }
  })
  output$ContBin <- renderText({
    ContBin()
  })
  outputOptions(output, "ContBin", suspendWhenHidden=FALSE) #needed for UI options, but doesn't need displaying itself
  
  outcome <- reactive({                  # different outcome variables if continuous or binary
    if (ContBin()=='continuous') {
      OutcomeCont()
    } else {
      OutcomeBina()
    }
  })
  
  
  
  
  ### Summary sentence of meta-analysis ###
  #-----------------------------------#
  
  FreqSummaryText <- eventReactive( input$FreqRun, {
    paste("Results for ", strong(FixRand()), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology,
    with reference treatment ", strong(Pair_ctrl()), ".", sep="")
  })
  output$SynthesisSummaryFreq <- renderText({FreqSummaryText()})
  BayesSummaryText <- eventReactive( input$BayesRun, {
    paste("Results for ", strong(FixRand()), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(prior()), " and
    reference treatment ", strong(Pair_ctrl()), ".", sep="")
  })
  output$SynthesisSummaryBayes <- renderText({BayesSummaryText()})
  
  
  
  
  
  
  ### Run frequentist Pairwise MA ###
  #-----------------------------#
  
  WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
    SwapTrt(CONBI=ContBin(), data=Long2Wide(data=data()$data), trt=Pair_trt())
  })
  
  observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
    updateCollapse(session=session, id="FreqID", open="Frequentist Analysis")
  })
  
  PairwiseSummary_functionF <- function(outcome, MA.Model) {
    sum <- summary(MA.Model)
    line0<-paste(strong("Results"))
    line1<-paste("Number of studies: ", sum$k, sep="")
    if (outcome=="OR") {
      line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), "); p-value: ", round(sum$pval, 3), sep="")
      line4<-paste("Between study standard-deviation (log-odds scale): ")
    } else if (outcome=="RR") {
      line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), "); p-value: ", round(sum$pval, 3), sep="")
      line4<-paste("Between study standard-deviation (log-probability scale): ")
    } else {
      line2<-paste("Pooled estimate: ", round(sum$b,2), " (95% CI: ", round(sum$ci.lb,2), " to ", round(sum$ci.ub,2), "); p-value: ", round(sum$pval, 3), sep="")
      line4<-paste("Between study standard-deviation: ")
    }
    line3<-paste(strong("Heterogeneity results"))
    line4<-paste(line4, round(sqrt(sum$tau2),3), "; I-squared: ", round(sum$I2,1), "%; P-value for testing heterogeneity: ", round(sum$QEp,3), sep="")
    HTML(paste(line0,line1, line2, line3, line4, sep = '<br/>'))
  }
  PairwiseModelFit_functionF <- function(MA.Model) {
    sum <- summary(MA.Model)
    HTML(paste("AIC: ", round(sum$fit.stats[3,1],2), "; BIC: ", round(sum$fit.stats[4,1],2), sep=""))
  }
  
  freqpair <- eventReactive( input$FreqRun, {         # run frequentist pairwise MA and obtain plots etc.
    information <- list()
    information$MA <- FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin())
    if (FixRand()=='fixed') {                   # Forest plot
      if (outcome()=='OR' | outcome()=='RR') {
        information$Forest <- {
          metafor::forest(information$MA$MA.Fixed, atransf=exp)
          title("Forest plot of studies with overall estimate from fixed-effects model")}
      } else {
        information$Forest <- {
          metafor::forest(information$MA$MA.Fixed)
          title("Forest plot of studies with overall estimate from fixed-effects model")}
      }
      information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Fixed)
      information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Fixed)
    } else if (FixRand()=='random') {
      if (outcome()=='OR' | outcome()=='RR') {
        information$Forest <- {
          metafor::forest(information$MA$MA.Random, atransf=exp)
          title("Forest plot of studies with overall estimate from random-effects model")}
      } else {
        information$Forest <- {
          metafor::forest(information$MA$MA.Random)
          title("Forest plot of studies with overall estimate from random-effects model")}
      }
      information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Random)
      information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Random)
    }
    information
  })
  
  output$ForestPlotPairF <- renderPlot({      # Forest plot
    freqpair()$Forest
  })
  
  output$forestpairF_download <- downloadHandler(
    filename = function() {
      paste0("PairwiseAnalysis.", input$forestpairF_choice)
    },
    content = function(file) {
      if (input$forestpairF_choice=='pdf') {pdf(file=file)}
      else {png(file=file)}
      if (FixRand()=='fixed') { 
        if (outcome()=='OR' | outcome()=='RR') {
          forest(freqpair()$MA$MA.Fixed, atransf=exp)
          title("Forest plot of studies with overall estimate from fixed-effects model")
        } else {
          forest(freqpair()$MA$MA.Fixed)
          title("Forest plot of studies with overall estimate from fixed-effects model")
        }
      } else {
        if (outcome()=='OR' | outcome()=='RR') {
          forest(freqpair()$MA$MA.Random, atransf=exp)
          title("Forest plot of studies with overall estimate from random-effects model")
        } else {
          forest(freqpair()$MA$MA.Random)
          title("Forest plot of studies with overall estimate from random-effects model")
        }
      }
      dev.off()
    }
  )
  
  output$SummaryTableF <- renderUI({          # Summary table
    freqpair()$Summary
  })
  
  output$ModelFitF <- renderUI({              # Model fit statistics
    freqpair()$ModelFit
  })
  
  
  
  
  
  
  ### Run Bayesian Pairwise MA ###
  #--------------------------#
  
  LongData <- reactive({               # convert wide format to long if need be
    Wide2Long(data=data()$data)
  })
  
  observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
    #NoBayesian()
    updateCollapse(session=session, id="BayesID", open="Bayesian Analysis")
  })
  
  
  PairwiseSummary_functionB <- function(outcome, MA.Model, model) {   # MA.Model has to have MAData, MA.Fixed and MA.Random
    line0<-paste(strong("Results"))
    line1<-paste("Number of studies: ", nrow(MA.Model$MA.Fixed$data_wide), sep="") # same for fixed or random
    if (model=='random') {
      line2<-paste("Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','est'],2), " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','lci'],2), " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','uci'],2), ")", sep="") # already exponentiated where needed within BayesPair function
      if (outcome=='OR') {
        line3<-paste("Between study standard-deviation (log-odds scale): ")
      } else if (outcome=='RR') {
        line3<-paste("Between study standard-deviation (log-probability scale): ")
      } else {
        line3<-paste("Between study standard-deviation: ")
      }
      line3<-paste(line3, round(MA.Model$MA.Random$fit_sum['tau[1]',1],3), " (95% CI: ", round(MA.Model$MA.Random$fit_sum['tau[1]',4],3), " to ", round(MA.Model$MA.Random$fit_sum['tau[1]',8],3), ")", sep="")
    } else {
      line2<-paste("Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','est'],2), " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','lci'],2), " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','uci'],2), ")", sep="") # already exponentiated where needed within BayesPair function
      line3<-paste("For fixed models, between study standard-deviation is set to 0.")
    }
    HTML(paste(line0,line1, line2, line3, sep = '<br/>'))
  }
  PairwiseModelFit_functionB <- function(MA.Model) {
    HTML(paste("Rhat: ", round(MA.Model$Rhat.max,2), sep=""))
  }
  
  bayespair <- eventReactive( input$BayesRun, {         # run Bayesian pairwise MA and obtain plots etc.
    #NoBayesian()
    information <- list()
    information$MA <- BayesPair(CONBI=ContBin(), data=WideData(), trt=Pair_trt(), ctrl=Pair_ctrl(), outcome=outcome(), chains=chains(), iter=iter(), warmup=burn(), model='both', prior=prior())
    if (FixRand()=='fixed') {
      information$Forest <- {
        g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='fixed')
        g + ggtitle("Forest plot of studies with overall estimate from fixed-effects model") +
          theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'))
      }
      information$Summary <- PairwiseSummary_functionB(outcome(),information$MA,'fixed')
      information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Fixed)
      information$Trace <- {
        g <- stan_trace(information$MA$MA.Fixed$fit, pars="theta")
        g + theme(legend.position='none', aspect.ratio = 0.45, axis.title=element_text(size=10,face="bold")) +
          labs(y="Pooled estimate", x="Iteration")
      }
    } else if (FixRand()=='random') {
      information$Forest <- {
        g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='random')
        g + ggtitle("Forest plot of studies with overall estimate from random-effects model") +
          theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'))
      }
      information$Summary <- PairwiseSummary_functionB(outcome(),information$MA,'random')
      information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Random)
      information$Trace <- {
        g <- stan_trace(information$MA$MA.Random$fit, pars=c("theta","tau"))
        g + theme(legend.position='none', strip.placement = "outside", aspect.ratio=0.3, axis.title=element_text(size=10,face="bold")) +
          labs(x="Iteration") +
          facet_wrap(~parameter, strip.position='left', nrow=2, scales='free', labeller=as_labeller(c(theta = "Pooled estimate", 'tau[1]' = "Between-study SD") ) )
      }
    }
    information
  })
  
  
  output$ForestPlotPairB <- renderPlot({      # Forest plot
    bayespair()$Forest
  })
  
  output$forestpairB_download <- downloadHandler(
    filename = function() {
      paste0("PairwiseAnalysis.", input$forestpairB_choice)
    },
    content = function(file) {
      plot <- bayespair()$Forest
      if (input$forestpairB_choice=='png') {
        ggsave(file, plot, height=7, width=12, units="in", device="png")
      } else {
        ggsave(file, plot, height=7, width=12, units="in", device="pdf")
      }
    }
  )
  
  output$SummaryTableB <- renderUI({          # Summary table
    bayespair()$Summary
  })
  
  output$ModelFitB <- renderUI({              # Model fit statistic
    bayespair()$ModelFit
  })
  
  output$TracePlot <- renderPlot({            # Trace plot
    bayespair()$Trace
  })
  
  output$tracepair_download <- downloadHandler(
    filename = function() {
      paste0("PairwiseTrace.", input$tracepair_choice)
    },
    content = function(file) {
      plot <- bayespair()$Trace
      if (input$forestpairB_choice=='png') {
        ggsave(file, plot, height=7, width=12, units="in", device="png")
      } else {
        ggsave(file, plot, height=7, width=12, units="in", device="pdf")
      }
    }
  )
}
