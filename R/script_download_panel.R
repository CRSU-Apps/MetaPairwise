
#' Create the panel for downloading a reproducible script.
#'
#' @param id ID of this module.
#' @param script_title Title of the script to be displayed on the download button.
#'
#' @return Div containing script download panel.
ScriptDownloadPanel <- function(id, script_title) {
  ns = NS(id)
  return(
    div(
      radioButtons(
        inputId = ns("extras"),
        label = div(
          tags$html("Include library functions:", tags$i(class="fa-regular fa-circle-question")),
          title = "How much of the underlying app code to download with the reproducible script"
        ),
        choiceNames = list(
          span(
            "Script Only", tags$i(class="fa-regular fa-circle-question"),
            title = "Only download the script, with none of the background functions"
          ),
          span(
            "Essential Only", tags$i(class="fa-regular fa-circle-question"),
            title = "Download the script with all background functions required to run it successfully"
          ),
          span(
            "Full Package", tags$i(class="fa-regular fa-circle-question"),
            title = "Download the script with all background functions used in the app, whether or not they are needed for this script"
          )
        ),
        choiceValues = c("none", "required", "all"),
        selected = "required"
      ),
      downloadButton(
        outputId = ns("download"),
        label = glue::glue("Download {script_title} script")
      )
    )
  )
}

#' Server for a script download panel.
#'
#' @param id ID of this module.
#' @param output_to_reproduce The shiny output object to be reproduced.
#' @param script_name Name of the script to be used in file and folder names.
#' @param required_meta_actions Meta actions required to create a fully reproducible script.
ScriptDownloadServer <- function(id, output_to_reproduce, script_name, required_meta_actions) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          if (input$extras == "none") {
            return(glue::glue("{script_name}.R"))
          } else {
            return(glue::glue("{script_name}.zip"))
          }
        },
        content = function(file) {
          if (input$extras == "none") {
            prerequisites <- list()
          } else if (input$extras == "required") {
            prerequisites <- required_meta_actions
          } else {
            prerequisites <- all_meta_pairwise_functions
          }
          
          ExportMetaPairwiseScript(
            output_file_name = file,
            script_name = script_name,
            prerequisite_definitions = prerequisites,
            main_content = shinymeta::expandChain(
              # Load libraries
              MetaLoadLibraries(),
              # create output
              output_to_reproduce()
            )
          )
        }
      )
    }
  )
}
