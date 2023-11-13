# MetaImpact Server #
#-------------------#

#----------------#
# load libraries #
#----------------#
library(netmeta)
library(sjlabelled)
library(gemtc)
library(tidyverse)
library(metafor)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidybayes)
library(dplyr)
library(ggridges)
library(glue)
library(forcats)
library(rstan)
library(MetaStan)
library(purrr)
library(splines2)

#-----------------------------#
# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE)
#source("SampleSizeFunctions.R", local=TRUE)
source("ForestFunctions.R", local=TRUE)
#source("LanganPlots.R", local=TRUE)


#----------------#
# Server Content #
#----------------#
function(input, output, session) {
  source("DownloadButtons.R", local=TRUE)  # needs to be within server
  
  #------------------#
  # Warning messages #
  #------------------#
  BadSampleSizes <- function(){
    showModal(modalDialog(
      title = "Unsuitable Sample Sizes",
      easyClose = FALSE,
      p("The total sample size is assuming two arms of equal size. Therefore, please enter ", tags$strong("even integers.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  NoBayesian <- function(){
    showModal(modalDialog(
      title = "Feature not yet available",
      easyClose = FALSE,
      p("Calculating the power of new studies with set sample size(s) is not ready yet within the Bayesian framework. Please ", tags$strong("choose frequentist.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  NoNMA <- function(){
    showModal(modalDialog(
      title = "Feature not yet available",
      easyClose = FALSE,
      p("Synthesising evidence with an NMA is not quite ready yet. Please ", tags$strong("choose pairwise.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  NoRandomContours <- function(){
    showModal(modalDialog(
      title = "Feature not yet available",
      easyClose = FALSE,
      p("Drawing the significance contours for a random-effects meta-analysis is not quite ready yet. Please either ", tags$strong("choose fixed-effects"), " or ", tags$strong("uncheck contours option.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  FixedPredInt <- function(){
    showModal(modalDialog(
      title = "Option combination not applicable",
      easyClose = FALSE,
      p("Within a fixed-effects model, the between-study heterogeneity is set to zero, therefore a 95% predictive interval would be equivalent to the 95% confidence interval (represented by the width of the diamond) and is not a plottable option."),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  SigContourOnly <- function(){
    showModal(modalDialog(
      title = "Feature not yet available",
      easyClose = FALSE,
      p("Currently, the contours on the extended funnel plot are only for when the desired impact of the new evidence base is related to levels of p-values."),
      p("Therefore, if you wish to plot the simulated 'new trials' from the power calculations, please either: ", tags$ol(tags$li("ensure that the ", strong("type of impact"), " is set to ", strong("Significant p-value"), ", or"), tags$li("uncheck the ", strong("contours"), " option"))),
      p("If you do not wish to plot the simulated 'new trials', please ", strong("uncheck"), " the plot simulated trials option."),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  DiffSigValues <- function(){
    showModal(modalDialog(
      title = "Significance values don't match",
      easyClose = FALSE,
      p("If you wish to plot the simulated 'new trials' on top of the extended funnel plot with contours, then the significance level/cut-off value needs to match."),
      p("Please ensure that the ", strong("Sig. level for contours"), " plot option is set to the same value as the ", strong("cut-off calculator"), " option."),
      p("If you do not wish to plot the simulated 'new trials' with the contour option of the extended funnel plot, please ", strong("uncheck"), " the plot simulated trials and/or contour option."),
      br(),
      modalButton("Close warning"),
      footer = "If this error appears after changing the impact type to 'significant p-value' due to a previous warning message, and your significance levels/cut-off values match, please ignore."
    ))
  }
  NoPlotMultipleSampleSizes <- function(){
    showModal(modalDialog(
      title = "Feature not yet available",
      easyClose = FALSE,
      p("Plotting the simulated 'new trials' of multiple sample sizes is not yet available. Please either ", tags$strong("uncheck 'plot simulated trials'"), " option or ", tags$strong("specify one sample size.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  
  
  
  ### Load and present Data ###
  #-----------------------#
  
  data <- reactive({                     # Read in user or default data
    file <- input$data
    if (is.null(file)) {
      if (input$ChooseExample=='continuousEx') {
        data <- read.csv("./AntiVEGF_Continuous_Pairwise.csv")
      } else {
        data <- read.csv("./AntiVEGF_Binary_Pairwise.csv")
      }
    } else {
      data <- read.table(file = file$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"")
    }
    levels <- levels(as_vector(lapply(data[grep("^T", names(data), value=TRUE)], factor)))  # extract treatment names/levels
    return(list(data=data, levels=levels))
  })
  
  # reference <- reactive({               # Select default reference treatment
  #    file <- input$data
  #    if (is.null(file)) {
  #      return("laser")
  #    } else {
  #      return(data()$levels[1])
  #    }
  #  })
  pairwise_ref <- function(trt_ctrl) {   # pairwise options
    if (trt_ctrl=='trt') {
      ref <- reactive({
        file <- input$data
        if (is.null(file)) {
          return("BEVA")
        } else {
          return(data()$levels[1])
        }
      })
    } else {
      ref <- reactive({
        file <- input$data
        if (is.null(file)) {
          return("RANI")
        } else {
          return(data()$levels[2])
        }
      })
    }
    return(ref())
  }
  
  #  observe({                              # populating reference treatment options
  #    updateSelectInput(session = session, inputId = "Reference", choices = data()$levels, selected = reference())
  #  })
  observe({
    updateSelectInput(session=session, inputId = "Pair_Trt", choices = data()$levels, selected = pairwise_ref(trt_ctrl='trt'))
  })
  observe({
    updateSelectInput(session=session, inputId = "Pair_Ctrl", choices = data()$levels, selected = pairwise_ref(trt_ctrl='ctrl'))
  })
  
  output$data <- renderTable({           # Create a table which displays the raw data just uploaded by the user
    data()$data
  })
  
  
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
      input$OutcomeCont
    } else {
      input$OutcomeBina
    }
  })
  
  
  
  
  ### Summary sentence of meta-analysis ###
  #-----------------------------------#
  
  FreqSummaryText <- eventReactive( input$FreqRun, {
    paste("Results for ", strong(input$FixRand), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology,
    with reference treatment ", strong(input$Pair_Ctrl), ".", sep="")
  })
  output$SynthesisSummaryFreq <- renderText({FreqSummaryText()})
  BayesSummaryText <- eventReactive( input$BayesRun, {
    paste("Results for ", strong(input$FixRand), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and
    reference treatment ", strong(input$Pair_Ctrl), ".", sep="")
  })
  output$SynthesisSummaryBayes <- renderText({BayesSummaryText()})
  
  
  ### Run frequentist Pairwise MA ###
  #-----------------------------#
  
  WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
    SwapTrt(CONBI=ContBin(), data=Long2Wide(data=data()$data), trt=input$Pair_Trt)
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
    if (input$FixRand=='fixed') {                   # Forest plot
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
    } else if (input$FixRand=='random') {
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
  
  output$SummaryTableF <- renderUI({          # Summary table
    freqpair()$Summary
  })
  
  output$ModelFitF <- renderUI({              # Model fit statistics
    freqpair()$ModelFit
  })
  
  
  
  
  
  ### Run frequentist NMA ###
  #---------------------#
  
  #freqnma <- eventReactive( input$FreqRun, {                   # Run frequentist NMA
  #  if (input$Pairwise_NMA==FALSE) {
  #    NoNMA()
  #    #FreqNMA(data=WideData(), outcome=outcome(), CONBI=ContBin(), model=input$FixRand, ref=input$Reference)
  #  }
  #})
  #output$NetworkPlotF <- renderPlot({   # Network plot
  #  netgraph(freqnma()$MAObject, thickness = "number.of.studies", number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=3, col.points=1, col="gray80", pos.number.of.studies=0.43,
  #           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "black", offset=0.03)
  #  title("Network plot of all studies")
  #})
  #output$ForestPlotNMAF <- renderPlot({    # Forest plot
  #  FreqNMAForest(NMA=freqnma()$MAObject, model=input$FixRand, ref=input$Reference)
  #  title("Forest plot of outcomes")
  #})
  
  ## Double zero arms are not included in analysis - need to add warning
  
  
  
  
  
  
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
    information$MA <- BayesPair(CONBI=ContBin(), data=WideData(), trt=input$Pair_Trt, ctrl=input$Pair_Ctrl, outcome=outcome(), chains=input$chains, iter=input$iter, warmup=input$burn, model='both', prior=input$prior)
    if (input$FixRand=='fixed') {
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
    } else if (input$FixRand=='random') {
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
  
  output$SummaryTableB <- renderUI({          # Summary table
    bayespair()$Summary
  })
  
  output$ModelFitB <- renderUI({              # Model fit statistic
    bayespair()$ModelFit
  })
  
  output$TracePlot <- renderPlot({            # Trace plot
    bayespair()$Trace
  })
  
  
  
  
  ### Run Bayesian NMA ###
  #------------------#
  
  #Bayes <- eventReactive( input$BayesRun & input$Pairwise_NMA=='FALSE', {                 # Run Bayesian NMA
  #  NoNMA()
  #BayesMA(data=LongData(), CONBI=ContBin(), outcome=outcome(), model=input$FixRand, ref=input$Reference, prior=input$prior)
  #})
  
  
  #output$NetworkPlotB <- renderPlot({  # Network plot
  #  plot(Bayes()$Network)
  #  title("Network plot of all studies")
  #})
  
  #output$ForestPlotB <- renderPlot({   # Forest plot
  #  forest(Bayes()$RelEffects, digits=3)
  #  title("Forest plot of outcomes")
  #})
  
  #output$TauB <- renderText({          # Between-study standard deviation
  #  TauDesc(ResultsSum=Bayes()$ResultsSum, outcome=outcome(), model=input$FixRand)
  #})
  
  #output$DICB <- renderTable({         # DIC
  #  Bayes()$DIC
  #}, digits=3, rownames=TRUE, colnames=FALSE)
  
  
  
  

  ### Links ###
  #-------#
  
  #observeEvent(input$link_to_tabpanel_evsynth, {
  #  updateTabsetPanel(session, "MetaImpact", "Evidence Synthesis")
  #})
  
  
  
  
  
}
