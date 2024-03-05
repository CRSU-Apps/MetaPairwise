bayesAnalysisUI <- function(id) {
  ns=NS(id)
  div(
    fluidRow(align="center",
             br(),
             actionButton(ns("BayesRun"), "Run Bayesian meta-analysis", class="btn-primary btn-lg")
    ),
    conditionalPanel(condition = "input.BayesRun!=0",
                     ns=ns,
                     fluidRow(p(htmlOutput(ns("SynthesisSummaryBayes"))),
                              p("To change the model options, please adjust synthesis options and re-run analysis."),
                              fluidRow(align='center', withSpinner(htmlOutput(ns("SummaryTableB")))),   # Summary table
                              fluidRow(align='center', div(style="display: inline-block;", p(strong("Model assessment"))),
                                       div(style="display: inline-block;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                    h6("Model assessment"),
                                                                                    p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
                                                                                    p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")
                                                                            )
                                      )
                              )
                     ),
                     htmlOutput(ns("ModelFitB"), align='center'),
                     h3("Trace Plot"),
                     fluidRow(column(10,withSpinner(plotOutput(ns("TracePlot")))
                     ),                            # Trace plot
                     column(2,radioButtons(ns('tracepair_choice'), "Download trace plot as:", c('pdf','png')),
                            downloadButton(ns('tracepair_download'), "Download trace plot")
                     )
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                     h3("Forest plot"),
                     fluidRow(column(10,withSpinner(plotOutput(ns("ForestPlotPairB"))),   # Forest plot)
                     ),                            
                     column(2,radioButtons(ns('forestpairB_choice'), "Download forest plot as:", c('pdf','png')),
                            downloadButton(ns('forestpairB_download'), "Download forest plot")
                     )
                     )
                     
    )
  )
}

bayesAnalysisServer <- function(id, data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl, prior, iter, chains, burn) {
  moduleServer(
    id,
    function(input, output, session) {
      

      ### Summary sentence of meta-analysis ###
      #-----------------------------------#
      
      
      BayesSummaryText <- eventReactive( input$BayesRun, {
        paste("Results for ", strong(FixRand()), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(prior()), " and
    reference treatment ", strong(Pair_ctrl()), ".", sep="")
      })
      output$SynthesisSummaryBayes <- renderText({BayesSummaryText()})
      
      
      
      ### Run Bayesian Pairwise MA ###
      #--------------------------#
      
      LongData <- reactive({               # convert wide format to long if need be
        Wide2Long(data=data())
      })
      
      WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
        SwapTrt(CONBI=ContBin(), data=Long2Wide(data=data()), trt=Pair_trt())
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
  )
}