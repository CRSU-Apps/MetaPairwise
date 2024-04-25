
#' Gather and format code to be exported.
#'
#' @param meta_action The return from a call to `shinymeta::metaAction()`.
#' @param expanded_chain The result from a call to `shinymeta::expandChain()`.
#'
#' @return Formatted code as text.
.BuildMetaPairwiseExportCode <- function(meta_action = NULL, expanded_chain = NULL) {
  if (is.null(meta_action) == is.null(expanded_chain)) {
    stop("When building code for export, either a meta action, or an expanded chain must be provided, not both")
  }
  
  if (is.null(expanded_chain)) {
    expanded_chain <- shinymeta::expandChain(meta_action())
  }
  
  expanded_chain %>%
    deparseCode() %>%
    formatCode(formatter = function(txt) { styler::style_text(txt) })
}

meta_data_definitions <- list(
  file_name = "data_definitions.R",
  directory = "R",
  meta_action = .BuildMetaPairwiseExportCode(MetaDataDefinitions)
)
meta_data_sorting_functions <- list(
  file_name = "data_sorting_functions.R",
  directory = "R",
  meta_action = .BuildMetaPairwiseExportCode(MetaDataSortingFunctions)
)
meta_data_wrangling_functions <- list(
  file_name = "data_wrangling_functions.R",
  directory = "R",
  meta_action = .BuildMetaPairwiseExportCode(MetaDataWranglingFunctions)
)
meta_freq_forest_plot_functions <- list(
  file_name = "plot_functions.R",
  directory = "R",
  meta_action = .BuildMetaPairwiseExportCode(MetaCreatePairwiseForestPlot)
)
meta_freq_analysis_functions <- list(
  file_name = "frequentist_analysis_functions.R",
  directory = "R",
  meta_action = .BuildMetaPairwiseExportCode(MetaFrequentistAnalysis)
)


#' Export a reproducible script to a zip file.
#'
#' @param zip_file_name Name of the zip file to create.
#' @param script_directory_name Name of the directory within the zip file.
#' @param prerequisite_definitions Definitions for files to be created in the "R" directory within the zip file.
#' @param main_content Contents of the "main.R" script created in the zip file.
#'
#' @return Name of the created zip file
#'
#' @examples
#' output$forest_script_download <- downloadHandler(
#'   filename = "frequentist_forest_plot.zip",
#'   content = function(file) {
#'     ExportMetaPairwiseScript(
#'       zip_file_name = file,
#'       script_directory_name = "frequentist_forest_plot",
#'       prerequisite_definitions = list(
#'         meta_data_functions,
#'         meta_freq_analysis_functions,
#'         meta_freq_forest_plot_functions
#'       ),
#'       main_content = shinymeta::expandChain(
#'         # Load libraries
#'         MetaLoadLibraries(),
#'         # Execute functions
#'         output$ForestPlotPairF()
#'       )
#'     )
#'   }
#' )
ExportMetaPairwiseScript <- function(zip_file_name, script_directory_name, prerequisite_definitions, main_content) {
  # Generate temporary directory name
  zip_dir <- file.path(tempfile(), script_directory_name)
  
  # Create temporary directories
  dir.create(zip_dir, recursive = TRUE)
  
  # Write prerequisites to files
  for (definition in prerequisite_definitions) {
    sub_dir <- file.path(zip_dir, definition$directory)
    if (!dir.exists(sub_dir)) {
      dir.create(sub_dir, recursive = TRUE)
    }
    readr::write_lines(
      file = file.path(sub_dir, definition$file_name),
      definition$meta_action
    )
  }
  
  # Write main script to file
  readr::write_lines(
    file = file.path(zip_dir, "main.R"),
    .BuildMetaPairwiseExportCode(expanded_chain = main_content)
  )
  
  # Create zip file
  zip::zip(zipfile = zip_file_name, files = zip_dir, mode = "cherry-pick")
}
