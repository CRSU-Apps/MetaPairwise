freqAnalysisUI <- function(id) {
  ns = NS(id)
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
      br(),
      fluidRow(
        column(
          width = 10,
          withSpinner(
            plotOutput(outputId = ns("LabbePlotPairF"))
          )
        ),
        column(
          width = 2,
          radioButtons(
            inputId = ns('labbepairF_choice'),
            label = "Download L'abbé plot as:",
            choices = c('pdf','png')
          ),
          downloadButton(
            outputId = ns('labbepairF_download'),
            label = "Download L'abbé plot"
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
      
      freqpair <- eventReactive(
        input$FreqRun,
        {
          information <- list()
          information$MA <- FreqPair(data = WideData(), outcome = outcome(), model = 'both', CONBI = ContBin())
          if (FixRand() == 'fixed') {
            information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Fixed)
            information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Fixed)
          } else if (FixRand() == 'random') {
            information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Random)
            information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Random)
          }
          return(information)
        }
      )
      
      output$ForestPlotPairF <- renderPlot({
        CreatePairwiseForestPlot(
          reference = Pair_ctrl(),
          intervention = Pair_trt(),
          meta_analysis = freqpair()$MA,
          model_effects = FixRand(),
          outcome_measure = outcome()
        )
      })
      
      
      ## Forest Plot Download ##
      
      output$forestpairF_download <- downloadHandler(
        filename = function() {
          paste0("forest_plot.", input$forestpairF_choice)
        },
        content = function(file) {
          if (input$forestpairF_choice == 'pdf') {
            pdf(file = file, width = 15)
          } else if (input$forestpairF_choice == 'png') {
            png(file = file, width = 1000)
          } else {
            stop("Only 'pdf' and 'png' file types are supported")
          }
          
          CreatePairwiseForestPlot(
            reference = Pair_ctrl(),
            intervention = Pair_trt(),
            meta_analysis = freqpair()$MA,
            model_effects = FixRand(),
            outcome_measure = outcome()
          )
          
          dev.off()
        }
      )
      
      output$SummaryTableF <- renderUI({
        freqpair()$Summary
      })
      
      output$ModelFitF <- renderUI({
        freqpair()$ModelFit
      })
      
      output$LabbePlotPairF <- renderPlot({
        if (FixRand() == 'fixed') {
          meta_analysis <- freqpair()$MA$MA.Fixed
        } else if (FixRand() == 'random') {
          meta_analysis <- freqpair()$MA$MA.Random
        } else {
          stop("Models effects should be 'fixed' or 'random'")
        }
        
        metafor::labbe(meta_analysis)
      })
      
      output$LabbepairF_download <- downloadHandler(
        filename = function() {
          paste0("labbePlot", input$forestpairF_choice)
        },
        content = function(file) {
          if (input$labbepairF_choice == 'pdf') {
            pdf(file = file)
          } else if (input$forestpairF_choice == 'png') {
            png(file = file)
          } else {
            stop("Only 'pdf' and 'png' file types are supported")
          }
          
          if (FixRand() == 'fixed') {
            meta_analysis <- freqpair()$MA$MA.Fixed
          } else if (FixRand() == 'random') {
            meta_analysis <- freqpair()$MA$MA.Random
          } else {
            stop("Models effects should be 'fixed' or 'random'")
          }
          
          metafor::labbe(meta_analysis)
          dev.off()
        }
      )
      
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