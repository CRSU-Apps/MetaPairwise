optionsPanelUI <- function(id) {
  ns=NS(id)
  div(h4("Synthesis Options"),
               
               fluidRow(column(6,selectInput(inputId = ns("Pair_Trt"), label = "Select Treatment", choices = NULL)
               ),
               column(6,selectInput(inputId = ns("Pair_Ctrl"), label = "Select Comparator", choices = NULL)
               )
               ),
               radioButtons(ns("FixRand"), "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random")
               ),
               conditionalPanel(condition = "output.ContBin=='continuous'",
                                radioButtons(ns("OutcomeCont"), "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD")
                                )
               ),
               conditionalPanel(condition = "output.ContBin=='binary'",
                                radioButtons(ns("OutcomeBina"), "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD")
                                )
               ),
               conditionalPanel(condition= "input.AnalysisType=='Bayesian Analysis'",
                                h5("Bayesian Options"),
                                fluidRow(column(6, radioButtons(ns("prior"), "Vague prior for between study standard deviation:", c("Half-Cauchy(0,0.5)" = "half-cauchy", "Uniform(0,2)" = "uniform", "Half-Normal(0,1)" = "half-normal")),
                                                actionButton(ns("bayes_help"), "Help", class="btn-xs", style="position: absolute; left: 0; top: 220px")
                                ),
                                column(6, numericInput(ns("chains"), "Number of chains:", value=2, min=1),
                                       numericInput(ns("iter"), "Number of iterations:", value=4000, min=1),
                                       numericInput(ns("burn"), "Burn-in:", value=400, min=1)
                                )
                                )
               )
  )
}

optionsPanelServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      pairwise_ref <- function(trt_ctrl) {   # pairwise options
        if (trt_ctrl=='trt') {
          ref <- reactive({
              return(data()$levels[1])
          })
        } else {
          ref <- reactive({
              return(data()$levels[2])
          })
        }
        return(ref())
      }
      
      observe({
        updateSelectInput(session=session, inputId = "Pair_Trt", choices = data()$levels, selected = pairwise_ref(trt_ctrl='trt'))
      })
      observe({
        updateSelectInput(session=session, inputId = "Pair_Ctrl", choices = data()$levels, selected = pairwise_ref(trt_ctrl='ctrl'))
      })
      
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