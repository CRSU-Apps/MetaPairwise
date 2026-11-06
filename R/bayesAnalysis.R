bayesAnalysisUI <- function(id) {
  ns = NS(id)
  div(
    fluidRow(
      align="center",
      br(),
      actionButton(
        inputId = ns("BayesRun"),
        label = "Run Bayesian meta-analysis",
        class = "btn-primary btn-lg"
      ),
      div(
        id=ns("bayes_warning"),
        "Bayesian analysis has been temporarily disabled for this outcome due to a code issue.",
        span(
          "See the ",
          tags$a(
            href="https://github.com/CRSU-Apps/MetaPairwise/issues/22",
            target="_blank",
            "GitHub issue"
          ),
          " for details."
        ),
        style = "display: inline-block; color: red; font-style: italic; font-weight: bold; padding-right: 20pt;"
      )
    ),
    conditionalPanel(
      condition = "output.analysis_up_to_date",
      ns = ns,
      fluidRow(
        p(
          htmlOutput(outputId = ns("SynthesisSummaryBayes"))
        ),
        p("To change the model options, please adjust synthesis options and re-run analysis."),
        fluidRow(
          align = "center",
          htmlOutput(outputId = ns("SummaryTableB"))
        ),
        # Summary table
        fluidRow(
          align = "center",
          div(
            style = "display: inline-block;",
            p(strong("Model assessment"))
          ),
          div(
            style = "display: inline-block;",
            dropMenu(
              dropdownButton(
                size = "xs",
                icon = icon("info")
              ),
              align = "left",
              h6("Model assessment"),
              p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
              p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")
            )
          )
        )
      ),
      div(
        textOutput(outputId = ns("ModelFitB")),
        style = "text-align: center;"
      ),
      fluidRow(
        align = "center",
        downloadButton(
          outputId = ns("results_export"),
          label = "Download results JSON"
        )
      ),
      h3("Trace Plot"),
      fluidRow(
        withSpinner(
          plotOutput(outputId = ns("TracePlot"))
        )
      ),
      fluidRow(
        align = "center",
        div(
          radioButtons(
            inputId = ns("tracepair_choice"),
            label = "Download trace plot as:",
            choices = c(
              "PDF" = "pdf",
              "PNG" = "png"
            )
          ),
          downloadButton(
            outputId = ns("tracepair_download"),
            label = "Download trace plot"
          )
        )
      ),
      h3("Forest plot"),
      fluidRow(
        withSpinner(
          plotOutput(outputId = ns("ForestPlotPairB"))
        )
      ),
      fluidRow(
        align = "center",
        div(
          radioButtons(
            inputId = ns("forestpairB_choice"),
            label = "Download forest plot as:",
            choices = c(
              "PDF" = "pdf",
              "PNG" = "png"
            )
          ),
          downloadButton(
            outputId = ns("forestpairB_download"),
            label = "Download forest plot"
          )
        )
      ),
      hr(),
      fluidRow(
        align = "center",
        style = "margin-bottom: 16pt;",
        downloadButton(
          outputId = ns("BayesReport"),
          label = "Generate and Download Analysis Report"
        ),
        downloadButton(
          outputId = ns("BayesRmdReport"),
          label = "Download Reproducible R Markdown Report",
          title = "Download a zip file containing an R Markdown report, which can be knitted to re-run this analysis"
        )
      )
    )
  )
}

bayesAnalysisServer <- function(id, data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl, prior, iter, chains, burn, seed) {
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
      }) %>% bindEvent(input$BayesRun)
      
      # Clear output when options change
      observe({
        analysis_up_to_date(FALSE)
      }) %>% bindEvent(data(), outcome(), Pair_trt(), Pair_ctrl(), prior(), iter(), chains(), burn(), seed())
      
      ### Summary sentence of meta-analysis ###
      #-----------------------------------#
      
      output$SynthesisSummaryBayes <- renderText({
        paste0(
          "Results for ",
          strong(FixRand()),
          "-effects ",
          strong("Pairwise"),
          " meta-analysis of ",
          strong(outcome()),
          "s using ",
          strong("Bayesian"),
          " methodology, with vague prior ",
          strong(prior()),
          " and reference treatment ",
          strong(Pair_ctrl()),
          "."
        )
      })
      
      ### Run Bayesian Pairwise MA ###
      #--------------------------#
      
      WideData <- shinymeta::metaReactive({
        "# convert long format to wide if need be (and ensure trt and ctrl are the right way round)"
        SwapTrt(
          CONBI = shinymeta::..(ContBin()),
          data = Long2Wide(data = shinymeta::..(data())),
          trt = shinymeta::..(Pair_trt())
        )
      })
      
      observe({
        unsupported_outcome <- outcome() == "RR" || outcome() == "RD"
        invalid_seed <- !is.numeric(seed()) || is.na(seed()) || seed() != round(seed())

        if (unsupported_outcome || invalid_seed) {
          shinyjs::disable("BayesRun")
        } else {
          shinyjs::enable("BayesRun")
        }

        if (unsupported_outcome) {
          shinyjs::show("bayes_warning")
        } else {
          shinyjs::hide("bayes_warning")
        }
      })
      
      # This is the `shinymeta` equivalent to `shiny::eventReactive({})`
      bayespair <- shinymeta::metaReactive2({
        req(input$BayesRun)
        isolate(
          shinymeta::metaExpr({
            BayesPair(
              CONBI = shinymeta::..(ContBin()),
              data = shinymeta::..(WideData()),
              trt = shinymeta::..(Pair_trt()),
              ctrl = shinymeta::..(Pair_ctrl()),
              outcome = shinymeta::..(outcome()),
              chains = shinymeta::..(chains()),
              iter = shinymeta::..(iter()),
              warmup = shinymeta::..(burn()),
              model = "both",
              prior = shinymeta::..(prior()),
              seed = shinymeta::..(seed())
            )
          })
        )
      })
      
      bayes_forest <- shinymeta::metaReactive({
        BayesPairForest(shinymeta::..(bayespair())$MAdata, outcome = shinymeta::..(outcome()), model = shinymeta::..(FixRand())) +
          ggtitle(paste0("Forest plot of studies with overall estimate from ", shinymeta::..(FixRand()), "-effects model")) +
          theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))
      })
      
      bayes_trace <- shinymeta::metaReactive({
        if (shinymeta::..(FixRand()) == "fixed") {
          stan_trace(shinymeta::..(bayespair())$MA.Fixed$fit, pars = "theta") +
            theme(legend.position = "none", aspect.ratio = 0.45, axis.title = element_text(size = 10, face = "bold")) +
            labs(y = "Pooled estimate", x = "Iteration")
        } else if (shinymeta::..(FixRand()) == "random") {
          stan_trace(shinymeta::..(bayespair())$MA.Random$fit, pars = c("theta","tau")) +
            theme(
              legend.position = "none",
              strip.placement = "outside",
              aspect.ratio = 0.3,
              axis.title = element_text(size = 10, face = "bold")
            ) +
            labs(x = "Iteration") +
            facet_wrap(
              ~parameter,
              strip.position = "left",
              nrow = 2,
              scales = "free",
              labeller = as_labeller(
                c(
                  theta = "Pooled estimate",
                  "tau[1]" = "Between-study SD"
                )
              )
            )
        }
      })
      
      summary_table <- shinymeta::metaReactive({
        PairwiseSummary_functionB(shinymeta::..(outcome()), shinymeta::..(bayespair()), shinymeta::..(FixRand()))
      })
      
      output$SummaryTableB <- renderUI({
        summary_table()
      })
      
      rhat_sentence <- shinymeta::metaReactive({
        if (shinymeta::..(FixRand()) == "fixed") {
          paste0("Rhat: ", round(shinymeta::..(bayespair())$MA.Fixed$Rhat.max, 2))
        } else if (shinymeta::..(FixRand()) == "random") {
          paste0("Rhat: ", round(shinymeta::..(bayespair())$MA.Random$Rhat.max, 2))
        }
      })
      
      output$ModelFitB <- renderText({
        rhat_sentence()
      })
      
      output$results_export <- downloadHandler(
        filename = "bayesian_results.json",
        content = function(file) {
          json <- ExportBayesianJson(
            meta_analysis = bayespair(),
            model_effects = FixRand(),
            outcome_measure = outcome()
          )
          write_file(x = json, file = file)
        }
      )
      
      output$TracePlot <- renderPlot({
        bayes_trace()
      })
      
      output$tracepair_download <- downloadHandler(
        filename = function() {
          paste0("trace_plot.", input$tracepair_choice)
        },
        content = function(file) {
          plot <- bayes_trace()
          if (input$tracepair_choice == "png") {
            ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
          } else if (input$tracepair_choice == "pdf") {
            ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
          } else {
            stop("Only 'pdf' and 'png' file types are supported")
          }
        }
      )
      
      output$ForestPlotPairB <- renderPlot({
        bayes_forest()
      })
      
      output$forestpairB_download <- downloadHandler(
        filename = function() {
          paste0("bayesian_forest_plot.", input$forestpairB_choice)
        },
        content = function(file) {
          plot <- bayes_forest()
          if (input$forestpairB_choice == "png") {
            ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
          } else if (input$forestpairB_choice == "pdf") {
            ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
          } else {
            stop("Only 'pdf' and 'png' file types are supported")
          }
        }
      )

      ## Reporter Function ##

      output$BayesReport <- downloadHandler(
        filename = "Bayesian_Analysis_Report.html",
        content = function(file) {
          shiny::withProgress(
            message = "Generating file...",
            value = 0.5,
            {
              CreateBayesianReport(
                output_file = file,
                meta_analysis = bayespair(),
                model_effects = FixRand(),
                outcome_measure = outcome(),
                data_type = ContBin(),
                intervention = Pair_trt(),
                reference = Pair_ctrl(),
                prior = prior(),
                chains = chains(),
                iter = iter(),
                burn = burn(),
                seed = seed(),
                trace_plot = bayes_trace(),
                forest_plot = bayes_forest()
              )

              shiny::setProgress(message = "File generated", value = 1)
            }
          )
        }
      )
      
      output$BayesRmdReport <- downloadHandler(
        filename = "bayesian_report.zip",
        content = function(file) {
          shiny::withProgress(
            message = "Generating file...",
            value = 0.5,
            {
              sections <- list(
                list(
                  heading = "Setup",
                  text = "Load the required packages, and the functions from the `R` directory.",
                  code = function(ec) shinymeta::expandChain(MetaLoadLibraries(), .expansionContext = ec)
                ),
                list(
                  heading = "Meta-analysis",
                  text = glue::glue(
                    "Run the fixed- and random-effects meta-analyses using MCMC sampling with {chains()} chains of {iter()} iterations each. ",
                    "The random seed is set to {seed()}, so the results will match those in the app."
                  ),
                  code = function(ec) {
                    shinymeta::expandChain(
                      shinymeta::withMetaMode(shinymeta::metaExpr({
                        shinymeta::..(summary_table())
                      })),
                      .expansionContext = ec
                    )
                  }
                ),
                list(
                  heading = "Model assessment",
                  text = "If a model has converged, Rhat should be smaller than 1.01 and the trace plot should be 'spiky' and show no signs of distinct patterns.",
                  chunk_options = "fig.width = 12, fig.height = 7",
                  code = function(ec) {
                    shinymeta::expandChain(
                      shinymeta::withMetaMode(shinymeta::metaExpr({
                        cat(shinymeta::..(rhat_sentence()), sep = "\n")
                        shinymeta::..(bayes_trace())
                      })),
                      .expansionContext = ec
                    )
                  }
                ),
                list(
                  heading = "Forest plot",
                  chunk_options = "fig.width = 12, fig.height = 7",
                  code = function(ec) {
                    shinymeta::expandChain(
                      shinymeta::withMetaMode(shinymeta::metaExpr({
                        shinymeta::..(bayes_forest())
                      })),
                      .expansionContext = ec
                    )
                  }
                )
              )
              
              rmd_content <- BuildMetaPairwiseRmd(
                title = "MetaPairwise Bayesian Analysis Report",
                introduction = glue::glue(
                  "{tools::toTitleCase(FixRand())}-effects pairwise meta-analysis of {outcome()}s using Bayesian methodology, ",
                  "comparing {Pair_trt()} to reference treatment {Pair_ctrl()}, with vague prior {prior()}."
                ),
                sections = sections
              )
              
              ExportMetaPairwiseReport(
                output_file_name = file,
                report_name = "bayesian_report",
                prerequisite_definitions = list(
                  meta_data_wrangling_functions,
                  meta_bayes_analysis_functions,
                  meta_bayes_summary_functions,
                  meta_bayes_forest_plot_functions
                ),
                rmd_content = rmd_content
              )
              
              shiny::setProgress(message = "File generated", value = 1)
            }
          )
        }
      )
    }
  )
}