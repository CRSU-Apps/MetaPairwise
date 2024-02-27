optionsPanelUI <- function(id) {
  ns = NS(id)
  div(
    h4("Synthesis Options"),
    fluidRow(
      div(
        style = "display: flex; justify-content: space-between; margin: 0 15px",
        selectInput(
          inputId = ns("Pair_Trt"),
          label = "Select Treatment",
          choices = NULL,
          selectize = FALSE
        ),
        div(
          style = "display: inline-block; align-self: center;",
          actionButton(
            inputId = ns("swap_treatments"),
            label = NULL,
            icon = icon(name = "arrow-right-arrow-left")
          )
        ),
        selectInput(
          inputId = ns("Pair_Ctrl"),
          label = "Select Comparator",
          choices = NULL,
          selectize = FALSE
        )
      )
    ),
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
            class = "btn-xs",
            style = "position: absolute; left: 0; top: 220px"
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
      
      # Update available treatments when the data changes
      observe({
        updateSelectInput(inputId = "Pair_Trt", choices = data()$levels, selected = data()$levels[1])
      }) %>% bindEvent(data())
      
      # Update available comparators when treatment changes
      observe({
        items <- data()$levels
        items <- items[items != input$Pair_Trt]
        
        selected <- input$Pair_Ctrl
        if (is.null(selected) || !selected %in% items) {
          selected <- items[1]
        }
        updateSelectInput(inputId = "Pair_Ctrl", choices = items, selected = selected)
        
        # Allow the comparator selection to be updated given the new choices.
        ctrl_observer$resume()
      }) %>% bindEvent(input$Pair_Trt)
      
      # Used to swap treatments
      new_ctrl <- reactiveVal()
      
      # Swap treatments when button clicked
      observe({
        new_trt <- input$Pair_Ctrl
        # Prevent the comparator selection from updating until after the choices have been updated.
        ctrl_observer$suspend()
        new_ctrl(input$Pair_Trt)
        updateSelectInput(inputId = "Pair_Trt", selected = new_trt)
      }) %>% bindEvent(input$swap_treatments)
      
      # Swap treatments when button clicked
      ctrl_observer <- observe({
        updateSelectInput(inputId = "Pair_Ctrl", selected = new_ctrl())
        new_ctrl(NULL)
      }) %>% bindEvent(new_ctrl(), ignoreNULL = TRUE)
      
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
      
      return(list(Pair_ctrl=reactive({ input$Pair_Ctrl }),
                  Pair_trt=reactive({ input$Pair_Trt }),
                  FixRand=reactive({ input$FixRand }),
                  OutcomeCont=reactive({ input$OutcomeCont }),
                  OutcomeBina=reactive({ input$OutcomeBina }),
                  prior=reactive({ input$prior }),
                  chains=reactive({ input$chains }),
                  iter=reactive({ input$iter }),
                  burn=reactive({ input$burn})
                  ))
      
      
      
    }
  )
}