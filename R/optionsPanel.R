optionsPanelUI <- function(id) {
  ns = NS(id)
  div(
    h4("Synthesis Options"),
    treatment_selection_panel_ui(id = ns("treatment_selection")),
    radioButtons(
      inputId = ns("FixRand"),
      label = "Model selection:",
      choices = c(
        "Fixed-effects model (FE)" = "fixed",
        "Random-effects model (RE)" = "random"
      )
    ),
    # Intentionally not namespaced in order to capture values from global environment.
    # This is not the right way to do this. It should be fixed in future
    conditionalPanel(
      condition = "output.ContBin == 'continuous'",
      radioButtons(
        inputId = ns("OutcomeCont"),
        label = "Outcome for continuous data:",
        choices = c(
          "Mean Difference (MD)" = "MD",
          "Standardised Mean Difference (SMD)" = "SMD"
        )
      )
    ),
    # Intentionally not namespaced in order to capture values from global environment.
    # This is not the right way to do this. It should be fixed in future
    conditionalPanel(
      condition = "output.ContBin == 'binary'",
      radioButtons(
        inputId = ns("OutcomeBina"),
        label = "Outcome for binary data:",
        choices = c(
          "Odds Ratio (OR)" = "OR",
          "Risk Ratio (RR)" = "RR",
          "Risk Difference (RD)" = "RD"
        )
      )
    ),
    # Intentionally not namespaced in order to capture values from global environment.
    # This is not the right way to do this. It should be fixed in future
    conditionalPanel(
      condition= "input.AnalysisType == 'Bayesian Analysis'",
      h5("Bayesian Options"),
      fluidRow(
        column(
          width = 6,
          radioButtons(
            inputId = ns("prior"),
            label = "Vague prior for between study standard deviation:",
            choices = c(
              "Half-Cauchy(0,0.5)" = "half-cauchy",
              "Uniform(0,2)" = "uniform",
              "Half-Normal(0,1)" = "half-normal"
            )
          ),
          actionButton(
            inputId = ns("bayes_help"),
            label = "Help",
            class = "btn-xs"
          )
        ),
        column(
          width = 6,
          numericInput(
            inputId = ns("chains"),
            label = "Number of chains:",
            value = 2,
            min = 1
          ),
          numericInput(
            inputId = ns("iter"),
            label = "Number of iterations:",
            value = 4000,
            min = 1
          ),
          numericInput(
            inputId = ns("burn"),
            label = "Burn-in:",
            value = 400,
            min = 1
          )
        )
      )
    )
  )
}

optionsPanelServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      treatment_reactives <- treatment_selection_panel_server(id = "treatment_selection", data = data)
      
      # Interactive help boxes #
      
      steps <- reactive(data.frame(
        element = paste0("#", session$ns(c("prior", "chains", "iter", "burn"))),
        intro = c("Choose which vague prior to use to initially model the between-study standard deviation (used for random-effects models)",
            "Choose the number of chains. A chain represents a run-through of the analysis, with each chain starting with different values to aid robustness. The results then incorporate all chains.",
            "The number of iterations to run through. A higher number of iterations is likely to lead to more robust results but does take longer.",
            "The number of iterations to 'burn' (i.e. not include in the results) at the start. In early iterations, estimated parameters are unlikely to have converged and thus are likely to give spurious results.")
      ))
      # Bayesian settings #
      observeEvent(
        input$bayes_help,
        {
          rintrojs::introjs(
            session,
            options = list(
              steps = steps(),
              showBullets = FALSE,
              showProgress = TRUE,
              showStepNumbers = FALSE,
              nextLabel = "Next",
              prevLabel = "Prev",
              skipLabel = "Skip"
            )
          )
        }
      )
      
      return(
        list(
          Pair_ctrl = shinymeta::metaReactive({ shinymeta::..(treatment_reactives$reference()) }),
          Pair_trt = shinymeta::metaReactive({ shinymeta::..(treatment_reactives$intervention()) }),
          FixRand = shinymeta::metaReactive({ shinymeta::..(input$FixRand) }),
          OutcomeCont = shinymeta::metaReactive({ shinymeta::..(input$OutcomeCont) }),
          OutcomeBina = shinymeta::metaReactive({ shinymeta::..(input$OutcomeBina) }),
          prior = reactive({ input$prior }),
          chains = reactive({ input$chains }),
          iter = reactive({ input$iter }),
          burn = reactive({ input$burn})
        )
      )
    }
  )
}