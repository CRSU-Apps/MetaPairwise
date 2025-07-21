#' UI elements of the forest plot panel.
#'
#' @param id ID of module
#'
#' @returns Div containing forest plot and download options
FrequentistForestPlotUI <- function(id) {
  ns = NS(id)
  
  div(
    fluidRow(
      align = 'center',
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
    )
  )
}

#' Server for forest plot panel.
#'
#' @param id ID of the module
#' @param freqpair Reactive containing frequentist pairwise analysis
#' @param FixRand Reactive containing model effects: "fixed" or "random"
#' @param outcome Reactive containing what the outcome measure is: one of ["OR", "RR", "RD", "MD", "SMD"]
#' @param Pair_trt Reactive containing name of treatment of interest
#' @param Pair_ctrl Reactive containing name of reference treatment
FrequentistForestPlotServer <- function(id, freqpair, FixRand, outcome, Pair_trt, Pair_ctrl) {
  moduleServer(
    id,
    function(input, output, session) {
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
    }
  )
}
