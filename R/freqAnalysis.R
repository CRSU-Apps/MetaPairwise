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
      condition = "output.analysis_up_to_date",
      ns = ns,
      fluidRow(
        p(htmlOutput(outputId = ns("SynthesisSummaryFreq"))),
        p("To change the model options, please adjust synthesis options and re-run analysis."),
        fluidRow(
          align = 'center',
          htmlOutput(outputId = ns("SummaryTableF"))
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
        align = "center",
        column(
          width = 6,
          downloadButton(
            outputId = ns("results_export"),
            label = "Download results JSON"
          )
        ),
        column(
          width = 6,
          ScriptDownloadPanel(id = ns("summary_script"), script_title = "summary and fit")
        )
      ),
      fluidRow(
        withSpinner(
          plotOutput(outputId = ns("ForestPlotPairF"))
        )
      ),
      fluidRow(
        align = 'center',
        column(
          width = 6,
          div(
            radioButtons(
              inputId = ns('forestpairF_choice'),
              label = "Download forest plot as:",
              choices = c(
                "PDF" = "pdf",
                "PNG" = "png"
              )
            ),
            downloadButton(
              outputId = ns('forestpairF_download'),
              label = "Download forest plot"
            )
          )
        ),
        column(
          width = 6,
          ScriptDownloadPanel(id = ns("forest_script"), script_title = "forest plot")
        )
      ),
      br(),
      conditionalPanel(
        condition = "output.labbe_available",
        ns = ns,
        fluidRow(
          withSpinner(
            plotOutput(outputId = ns("LabbePlotPairF"))
          )
        ),
        fluidRow(
          align = "center",
          column(
            width = 6,
            div(
              radioButtons(
                inputId = ns('labbepairF_choice'),
                label = "Download L'abbé plot as:",
                choices = c(
                  "PDF" = "pdf",
                  "PNG" = "png"
                )
              ),
              downloadButton(
                outputId = ns('labbepairF_download'),
                label = "Download L'abbé plot"
              )
            )
          ),
          column(
            width = 6,
            ScriptDownloadPanel(id = ns("labbe_script"), script_title = "L'abbé plot")
          )
        )
      ),
      hr(),
      fluidRow(
        align = "center",
        style = "margin-bottom: 16pt;",
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
      
      analysis_up_to_date <- reactiveVal(FALSE)
      output$analysis_up_to_date <- reactive({
        return(analysis_up_to_date())
      })
      outputOptions(output, "analysis_up_to_date", suspendWhenHidden = FALSE)
      
      # Validate output when meta-analysis run
      observe({
        analysis_up_to_date(TRUE)
      }) %>% bindEvent(input$FreqRun)
      
      # Clear output when options change
      observe({
        analysis_up_to_date(FALSE)
      }) %>% bindEvent(data(), outcome(), Pair_trt(), Pair_ctrl())
      
      
      ### Summary sentence of meta-analysis ###
      #-----------------------------------#
      
      output$SynthesisSummaryFreq <- renderText({
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
      })
      
      
      
      ### Run frequentist Pairwise MA ###
      #-----------------------------#
      
      # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
      WideData <- shinymeta::metaReactive({
        SwapTrt(
          CONBI = shinymeta::..(ContBin()),
          data = Long2Wide(data = shinymeta::..(data())),
          trt = shinymeta::..(Pair_trt())
        )
      })
      
      # This is the `shinymeta` equivalent to `shiny::eventReactive({})`
      freqpair <- shinymeta::metaReactive2({
        req(input$FreqRun)
        isolate(
          shinymeta::metaExpr({
            FreqPair(
              data = shinymeta::..(WideData()),
              outcome = shinymeta::..(outcome()),
              model = 'both',
              CONBI = shinymeta::..(ContBin())
            )
          })
        )
      })
      
      summary_lines <- shinymeta::metaReactive({
        if (shinymeta::..(FixRand()) == "fixed") {
          PairwiseSummary_functionF(shinymeta::..(outcome()), shinymeta::..(freqpair())$MA.Fixed)
        } else if (shinymeta::..(FixRand()) == "random") {
          PairwiseSummary_functionF(shinymeta::..(outcome()), shinymeta::..(freqpair())$MA.Random)
        }
      })
      
      output$SummaryTableF <- renderUI({
        lines = summary_lines()
        HTML(paste(strong(lines[1]), lines[2], strong(lines[3]), lines[4], lines[5], sep = "<br/>"))
      })
      
      fit_sentence <- shinymeta::metaReactive({
        if (shinymeta::..(FixRand()) == "fixed") {
          PairwiseModelFit_functionF(shinymeta::..(freqpair())$MA.Fixed)
        } else if (shinymeta::..(FixRand()) == 'random') {
          PairwiseModelFit_functionF(shinymeta::..(freqpair())$MA.Random)
        }
      })
      
      output$results_export <- downloadHandler(
        filename = "frequentist_results.json",
        content = function(file) {
          json <- ExportFrequentistJson(
            meta_analysis = freqpair(),
            model_effects = FixRand(),
            outcome_measure = outcome()
          )
          write_file(x = json, file = file)
        }
      )
      
      output$ModelFitF <- renderUI({
        HTML(fit_sentence())
      })
      
      output$ForestPlotPairF <- shinymeta::metaRender(
        renderFunc = renderPlot,
        expr = {
          CreatePairwiseForestPlot(
            reference = shinymeta::..(Pair_ctrl()),
            intervention = shinymeta::..(Pair_trt()),
            meta_analysis = shinymeta::..(freqpair()),
            model_effects = shinymeta::..(FixRand()),
            outcome_measure = shinymeta::..(outcome())
          )
        }
      )
      
      ScriptDownloadServer(
        id = "summary_script",
        output_to_reproduce = shinymeta::metaAction({
          for (line in shinymeta::..(summary_lines())) {
            print(line)
          }
          print("Model fit statistics:")
          print(shinymeta::..(fit_sentence()))
        }),
        script_name = "frequentist_summary",
        required_meta_actions = list(
          meta_data_wrangling_functions,
          meta_freq_analysis_functions,
          meta_freq_summary_functions
        )
      )
      
      ## Forest Plot Download ##
      
      output$forestpairF_download <- downloadHandler(
        filename = function() {
          paste0("frequentist_forest_plot.", input$forestpairF_choice)
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
            meta_analysis = freqpair(),
            model_effects = FixRand(),
            outcome_measure = outcome()
          )
          
          dev.off()
        }
      )
      
      ScriptDownloadServer(
        id = "forest_script",
        output_to_reproduce = output$ForestPlotPairF,
        script_name = "frequentist_forest_plot",
        required_meta_actions = list(
          meta_data_wrangling_functions,
          meta_freq_analysis_functions,
          meta_freq_forest_plot_functions
        )
      )
      
      output$labbe_available <- reactive({
        return(ContBin() == "binary")
      })
      outputOptions(output, "labbe_available", suspendWhenHidden = FALSE)
      
      output$LabbePlotPairF <- shinymeta::metaRender(
        renderFunc = renderPlot,
        expr = {
          if (shinymeta::..(FixRand()) == 'fixed') {
            meta_analysis <- shinymeta::..(freqpair())$MA.Fixed
          } else if (shinymeta::..(FixRand()) == 'random') {
            meta_analysis <- shinymeta::..(freqpair())$MA.Random
          } else {
            stop("Models effects should be 'fixed' or 'random'")
          }
          
          metafor::labbe(meta_analysis)
          title(paste0("L'abbé plot from ", shinymeta::..(FixRand()), "-effects model"))
        }
      )
      
      output$labbepairF_download <- downloadHandler(
        filename = function() {
          paste0("labbePlot.", input$labbepairF_choice)
        },
        content = function(file) {
          if (input$labbepairF_choice == 'pdf') {
            pdf(file = file)
          } else if (input$labbepairF_choice == 'png') {
            png(file = file)
          } else {
            stop("Only 'pdf' and 'png' file types are supported")
          }
          
          if (FixRand() == 'fixed') {
            meta_analysis <- freqpair()$MA.Fixed
          } else if (FixRand() == 'random') {
            meta_analysis <- freqpair()$MA.Random
          } else {
            stop("Models effects should be 'fixed' or 'random'")
          }
          
          metafor::labbe(meta_analysis)
          title(glue::glue("L'abbé plot from {FixRand()}-effects model"))
          
          dev.off()
        }
      )
      
      ScriptDownloadServer(
        id = "labbe_script",
        output_to_reproduce = output$LabbePlotPairF,
        script_name = "frequentist_labbe_plot",
        required_meta_actions = list(
          meta_data_wrangling_functions,
          meta_freq_analysis_functions
        )
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
                file.copy(reporter(freqpair()$MA.Fixed, filename = "report", open = FALSE), file)
              } else {
                file.copy(reporter(freqpair()$MA.Random, filename= "report", open = FALSE), file)
              }
              
              setProgress(message = 'File generated', value = 1)
            }
          )
        }
      )
    }
  )
}