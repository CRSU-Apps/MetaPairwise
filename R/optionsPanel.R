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
                                                actionButton(ns("bayes_help"), "Help (Needs Fixing)", class="btn-xs", style="position: absolute; left: 0; top: 220px")
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
        category=c(rep("CalcSettings",5), rep("BayesSettings",4)),
        element=c("#samplesizes", "#its", "#impact_type", "#cutoff", "#plot_sims",  "#prior", "#chains", "#iter", "#burn"),
        intro=c("This is where you specify sample sizes for which you wish to estimate power. You can enter one sample size, or multiple by separating them with a semi-colon (;). Currently, it is assumed that future designed trials have two arms of equal size.",
                "Choose how many iterations (i.e. times the algorithm is run) you wish to have per simulation (sample size). If you choose a higher number of iterations, the simulations will take longer but give more precise estimates (narrower confidence intervals), and vice versa.",
                "Making an 'impact' on the current evidence base can be done in multiple ways - choose here which method you wish to focus on (1. Having a significant p-value; 2. Having a 95% confidence interval of a certain width; 3. Having the lower bound of the 95% CI above a certain value; 4. Having the upper bound of the 95% CI below a certain value).",
                "Depending on which type of impact has been chosen, please choose a specific cut-off value for which you define as 'impactful' (e.g. a p-value of less than 0.05).",
                "Choose to plot the results from every simulated 'new study' into the extended funnel plot to visually see how the power is calculated (when viewed alongside the significance contours",
                "Choose which vague prior to use to initially model the between-study standard deviation (used for random-effects models)",
                "Choose the number of chains. A chain represents a run-through of the analysis, with each chain starting with different values to aid robustness. The results then incorporate all chains.",
                "The number of iterations to run through. A higher number of iterations is likely to lead to more robust results but does take longer.",
                "The number of iterations to 'burn' (i.e. not include in the results) at the start. In early iterations, estimated parameters are unlikely to have converged and thus are likely to give spurious results.")
      ))
      # Calculator settings #
      observeEvent(input$calc_help,
                   introjs(session, options = list(steps=steps() %>% filter(category=="CalcSettings"), "showBullets"="false", "showProgress"="true",
                                                   "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))   # IMPACT_TYPE NOT WORKING and don't know why...
      )
      # Bayesian settings #
      observeEvent(input$bayes_help,
                   introjs(session, options = list(steps=steps() %>% filter(category=="BayesSettings"), "showBullets"="false", "showProgress"="true",
                                                   "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))
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