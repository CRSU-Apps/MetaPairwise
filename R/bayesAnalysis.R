bayesAnalysisUI <- function(id) {
  ns = NS(id)
  div(
    fluidRow(
      align="center",
      actionButton(
        inputId = ns("BayesRun"),
        label = "Run Bayesian meta-analysis",
        class = "btn-primary btn-lg"
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
            choices = c("pdf", "png")
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
            choices = c("pdf", "png")
          ),
          downloadButton(
            outputId = ns("forestpairB_download"),
            label = "Download forest plot"
          )
        )
      )
    )
  )
}

bayesAnalysisServer <- function(id, data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl, prior, iter, chains, burn) {
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
      }) %>% bindEvent(data(), outcome(), Pair_trt(), Pair_ctrl(), prior(), iter(), chains(), burn())
      
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
      
      LongData <- reactive({
        # convert wide format to long if need be
        Wide2Long(data = data()$data)
      })
      
      WideData <- reactive({
        # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
        SwapTrt(
          CONBI = ContBin(),
          data = Long2Wide(data = data()),
          trt = Pair_trt()
        )
      })
      
      bayespair <- eventReactive(
        input$BayesRun,
        {
          # run Bayesian pairwise MA
          return(
            BayesPair(
              CONBI = ContBin(),
              data = WideData(),
              trt = Pair_trt(),
              ctrl = Pair_ctrl(),
              outcome = outcome(),
              chains = chains(),
              iter = iter(),
              warmup = burn(),
              model = "both",
              prior = prior()
            )
          )
        }
      )
      
      bayes_forest <- reactive({
        return(
          BayesPairForest(bayespair()$MAdata, outcome = outcome(), model = FixRand()) +
            ggtitle(glue::glue("Forest plot of studies with overall estimate from {FixRand()}-effects model")) +
            theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))
        )
      })
      
      bayes_trace <- reactive({
        if (FixRand() == "fixed") {
          return(
            stan_trace(bayespair()$MA.Fixed$fit, pars = "theta") +
              theme(legend.position = "none", aspect.ratio = 0.45, axis.title = element_text(size = 10, face = "bold")) +
              labs(y = "Pooled estimate", x = "Iteration")
          )
        } else if (FixRand() == "random") {
          return(
            stan_trace(bayespair()$MA.Random$fit, pars = c("theta","tau")) +
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
          )
        }
      })
      
      output$SummaryTableB <- renderUI({
        PairwiseSummary_functionB(outcome(), bayespair(), FixRand())
      })
      
      output$ModelFitB <- renderText({
        if (FixRand() == "fixed") {
          return(paste0("Rhat: ", round(bayespair()$MA.Fixed$Rhat.max, 2)))
        } else if (FixRand() == "random") {
          return(paste0("Rhat: ", round(bayespair()$MA.Random$Rhat.max, 2)))
        }
      })
      
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
    }
  )
}