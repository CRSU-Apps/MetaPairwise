freqAnalysisUI <- function(id) {
  ns=NS(id)
  div(fluidRow(align="center",
                        br(),
                        actionButton(ns("FreqRun"), "Run frequentist meta-analysis", class="btn-primary btn-lg")
      ),
      conditionalPanel(condition = "input.FreqRun!=0",
                       ns=ns,
                       fluidRow(p(htmlOutput(ns("SynthesisSummaryFreq"))),
                                p("To change the model options, please adjust synthesis options and re-run analysis."),
                                fluidRow(align='center', withSpinner(htmlOutput(ns("SummaryTableF")))
                                ),
                                fluidRow(align='center', div(style="display: inline-block;", p(strong("Model fit statistics"))),
                                                         div(style="display: inline-block;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                                      h6("Model fit statistics"),
                                                                                                      p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
                                                                                                      p("The smaller the AIC or BIC, the 'better' the model. Values are best interpreted between models rather than alone.")
                                                                                              )
                                                             )
                                )
                                ),
                        htmlOutput(ns("ModelFitF"), align='center'),
                        fluidRow(column(10, withSpinner(plotOutput(ns("ForestPlotPairF")))),
                                 column(2, radioButtons(ns('forestpairF_choice'), "Download forest plot as:", c('pdf','png')),
                                           downloadButton(ns('forestpairF_download'), "Download forest plot"),    #Forest plot
                                 )
                        ),
                       fluidRow(align='center', downloadButton(ns("FreqReport"), "Generate and Download Analysis Report")),
      )
  )
}

freqAnalysisServer <- function(id, data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ### Summary sentence of meta-analysis ###
      #-----------------------------------#
      
      FreqSummaryText <- eventReactive( input$FreqRun, {
        paste("Results for ", strong(FixRand()), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology,
    with reference treatment ", strong(Pair_ctrl()), ".", sep="")
      })
      output$SynthesisSummaryFreq <- renderText({FreqSummaryText()})
      
      
      
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
      
      ## Reporter Function ##
      
      output$FreqReport <- downloadHandler(
        filename = "Analysis Report.html",
        content = function(file) {
          
          withProgress(message = 'Generating file...', value = 0.5, {
          
          if (FixRand()=='fixed') {
            file.copy(reporter(freqpair()$MA$MA.Fixed, open = FALSE, dir = "~"), file)
          } else {
            file.copy(reporter(freqpair()$MA$MA.Random, open = FALSE, dir = "~"), file)
          }
            
            setProgress(message = 'File generated', value = 1)
            Sys.sleep(1)
          })
          
            
        }
      )
      
    }
  )
}