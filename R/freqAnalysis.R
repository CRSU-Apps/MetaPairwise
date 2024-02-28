freqAnalysisUI <- function(id) {
  ns=NS(id)
  div(
    fluidRow(
      align = "center",
      br(),
      actionButton(
        inputId = ns("FreqRun"),
        label = "Run frequentist meta-analysis",
        class = "btn-primary btn-lg"
      )
    ),
    conditionalPanel(
      condition = "input.FreqRun != 0",
      ns = ns,
      fluidRow(
        p(htmlOutput(outputId = ns("SynthesisSummaryFreq"))),
        p("To change the model options, please adjust synthesis options and re-run analysis."),
        fluidRow(
          align = 'center',
          withSpinner(
            htmlOutput(outputId = ns("SummaryTableF"))
          )
        ),
        fluidRow(
          align = 'center',
          div(
            style = "display: inline-block;",
            p(strong("Model fit statistics"))
          ),
          div(
            style = "display: inline-block;",
            dropMenu(
              dropdownButton(
                size = 'xs',
                icon = icon('info')
              ),
              align = 'left',
              h6("Model fit statistics"),
              p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
              p("The smaller the AIC or BIC, the 'better' the model. Values are best interpreted between models rather than alone.")
            )
          )
        )
      ),
      htmlOutput(
        outputId = ns("ModelFitF"),
        align = 'center'
      ),
      fluidRow(
        column(
          width = 10,
          withSpinner(
            plotOutput(outputId = ns("ForestPlotPairF"))
          )
        ),
        column(
          width = 2,
          radioButtons(
            inputId = ns('forestpairF_choice'),
            label = "Download forest plot as:",
            choices = c('pdf','png')
          ),
          downloadButton(
            outputId = ns('forestpairF_download'),
            label = "Download forest plot"
          )
        )
      ),
      fluidRow(
        align = 'center',
        downloadButton(
          outputId = ns("FreqReport"),
          label = "Generate and Download Analysis Report"
        )
      )
    )
  )
}

freqAnalysisServer <- function(id, data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ### Summary sentence of meta-analysis ###
      #-----------------------------------#
      
      FreqSummaryText <- eventReactive(
        input$FreqRun,
        {
          paste0(
            "Results for ",
            strong(FixRand()),
            "-effects ",
            strong("Pairwise"),
            " meta-analysis of ",
            strong(outcome()),
            "s using ",
            strong("frequentist"),
            " methodology, with reference treatment ",
            strong(Pair_ctrl()),
            "."
          )
        }
      )
      output$SynthesisSummaryFreq <- renderText({
        FreqSummaryText()
      })
      
      
      
      ### Run frequentist Pairwise MA ###
      #-----------------------------#
      
      # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
      WideData <- reactive({
        SwapTrt(
          CONBI = ContBin(),
          data = Long2Wide(data = data()$data),
          trt = Pair_trt()
        )
      })
      
      observeEvent(
        input$FreqRun,
        {
          # reopen panel when a user re-runs analysis
          updateCollapse(session = session, id = "FreqID", open = "Frequentist Analysis")
        }
      )
      
      PairwiseSummary_functionF <- function(outcome, MA.Model) {
        sum <- summary(MA.Model)
        line0 <- paste(strong("Results"))
        line1 <- paste("Number of studies: ", sum$k, sep = "")
        if (outcome == "OR") {
          line2 <- paste0(
            "Pooled estimate: ",
            round(exp(sum$b), 2),
            " (95% CI: ",
            round(exp(sum$ci.lb), 2),
            " to ",
            round(exp(sum$ci.ub), 2),
            "); p-value: ",
            round(sum$pval, 3)
          )
          line4 <- "Between study standard-deviation (log-odds scale): "
        } else if (outcome == "RR") {
          line2 <- paste0(
            "Pooled estimate: ",
            round(exp(sum$b), 2),
            " (95% CI: ",
            round(exp(sum$ci.lb), 2),
            " to ",
            round(exp(sum$ci.ub), 2),
            "); p-value: ",
            round(sum$pval, 3)
          )
          line4 <- "Between study standard-deviation (log-probability scale): "
        } else {
          line2 <- paste(
            "Pooled estimate: ",
            round(sum$b, 2),
            " (95% CI: ",
            round(sum$ci.lb, 2),
            " to ",
            round(sum$ci.ub, 2),
            "); p-value: ",
            round(sum$pval, 3)
          )
          line4 <- "Between study standard-deviation: "
        }
        line3 <- strong("Heterogeneity results")
        line4 <- paste0(
          line4,
          round(sqrt(sum$tau2), 3),
          "; I-squared: ",
          round(sum$I2, 1),
          "%; P-value for testing heterogeneity: ",
          round(sum$QEp, 3)
        )
        HTML(paste(line0, line1, line2, line3, line4, sep = '<br/>'))
      }
      
      PairwiseModelFit_functionF <- function(MA.Model) {
        sum <- summary(MA.Model)
        HTML(paste0("AIC: ", round(sum$fit.stats[3, 1], 2), "; BIC: ", round(sum$fit.stats[4, 1], 2)))
      }
      
      freqpair <- eventReactive(
        input$FreqRun,
        {         # run frequentist pairwise MA and obtain plots etc.
          information <- list()
          information$MA <- FreqPair(data = WideData(), outcome = outcome(), model = 'both', CONBI = ContBin())
          if (FixRand() == 'fixed') {                   # Forest plot
            if (outcome() == 'OR' || outcome() == 'RR') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  atransf = exp,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            } else if (outcome() == 'RD') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")}
            } else {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  ilab = cbind(Mean.1, SD.1, Mean.2, SD.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Mean", "SD", "Mean", "SD","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            }
            
            information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Fixed)
            information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Fixed)
            
          } else if (FixRand() == 'random') {
            if (outcome() == 'OR' || outcome() == 'RR') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  atransf = exp,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from random-effects model")
              }
            }
            else if (outcome() == 'RD') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2,round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from random-effects model")
              }
            } else {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  ilab = cbind(Mean.1, SD.1, Mean.2, SD.2, round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Mean", "SD", "Mean", "SD","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            }
            
            information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Random)
            information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Random)
          }
          information
        }
      )
      
      output$ForestPlotPairF <- renderPlot({
        freqpair()$Forest
      })
      
      
      ## Forest Plot Download ##
      
      output$forestpairF_download <- downloadHandler(
        filename = function() {
          paste0("PairwiseAnalysis.", input$forestpairF_choice)
        },
        content = function(file) {
          information <- list()
          information$MA <- freqpair()$MA
          if (input$forestpairF_choice == 'pdf') {
            pdf(file = file, width = 15)
          } else {
            png(file = file, width = 1000)
          }
          
          if (FixRand() == 'fixed') { 
            if (outcome() == 'OR' || outcome() == 'RR') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  atransf = exp,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            } else if (outcome() == 'RD') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-", "Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            } else {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Fixed,
                  ilab = cbind(Mean.1, SD.1, Mean.2, SD.2, round(weights(information$MA$MA.Fixed), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Fixed$k + 2,
                  labels = c("Mean", "SD", "Mean", "SD","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Fixed$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            }
          } else {
            if (outcome() == 'OR' || outcome() == 'RR') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  atransf = exp,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from random-effects model")
              }
            } else if (outcome() == 'RD') {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1]+forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3]+forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from random-effects model")
              }
            } else {
              information$Forest <- {
                forestTemp <- metafor::forest(
                  information$MA$MA.Random,
                  ilab = cbind(Mean.1, SD.1, Mean.2, SD.2, round(weights(information$MA$MA.Random), 2))
                )
                
                text(
                  x = forestTemp$ilab.xpos,
                  y = information$MA$MA.Random$k + 2,
                  labels = c("Mean", "SD", "Mean", "SD","Weights(%)"),
                  font = 2
                )
                text(
                  x = c(
                    (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
                    (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
                  ),
                  y = information$MA$MA.Random$k + 3, 
                  labels = c(WideData()$T.1[1], WideData()$T.2[1])
                )
                title("Forest plot of studies with overall estimate from fixed-effects model")
              }
            }
          }
          dev.off()
        }
      )
      
      output$SummaryTableF <- renderUI({
        freqpair()$Summary
      })
      
      output$ModelFitF <- renderUI({
        freqpair()$ModelFit
      })
      
      ## Reporter Function ##
      
      output$FreqReport <- downloadHandler(
        filename = "Analysis Report.html",
        content = function(file) {
          
          withProgress(
            message = 'Generating file...',
            value = 0.5,
            {
              if (FixRand() == 'fixed') {
                file.copy(reporter(freqpair()$MA$MA.Fixed, filename = "report", open = FALSE), file)
              } else {
                file.copy(reporter(freqpair()$MA$MA.Random, filename= "report", open = FALSE), file)
              }
              
              setProgress(message = 'File generated', value = 1)
            }
          )
        }
      )
    }
  )
}