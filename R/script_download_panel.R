
#' Create the panel for downloading a reproducible script.
#'
#' @param id_base Base of the IDs to create for the library selection and download button.
#' @param extras_id ID for the library functions radio buttons. Defaults to `glue::glue("{id_base}_extras")`
#' @param download_id ID for the download button. Defaults to `glue::glue("{id_base}_download")`
#' @param script_title Title of the script to be displayed on the download button.
#' @param ns Namespace function if in a module. Defaults to `identity()` function.
#'
#' @return Div containing script download panel.
CreateScriptDownloadPanel <- function(
    id_base,
    extras_id = glue::glue("{id_base}_extras"),
    download_id = glue::glue("{id_base}_download"),
    script_title,
    ns = identity) {
  return(
    div(
      radioButtons(
        inputId = ns(extras_id),
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
        choiceValues = c("none", "required", "all")
      ),
      downloadButton(
        outputId = ns(download_id),
        label = glue::glue("Download {script_title} script")
      )
    )
  )
}
