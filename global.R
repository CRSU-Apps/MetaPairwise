
# load libraries

MetaLoadLibraries <- shinymeta::metaAction({
  # These comments are here to instruct users to input their own data
  "##############################################################"
  "# A data file will need to be added to make this script work #"
  "##############################################################"
  "# Uncomment the next line ↓ to use your data"
  "# loaded_data <- function() { rio::import('<YOUR_DATA_FILE_HERE>') }"
  
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(glue)
  library(markdown)
  library(metafor)
  library(MetaStan)
  library(purrr)
  library(rintrojs)
  library(rio)
  library(rmarkdown)
  library(rstan)
  library(shiny)
  library(shinyBS)
  library(shinycssloaders)
  library(shinydashboard)
  library(shinyjs)
  library(shinymeta)
  library(shinythemes)
  library(shinyWidgets)
  library(tidybayes)
  library(tidyr)
  library(tidyverse)
  
  "# Source all files in 'R' directory"
  library(R.utils)
  R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)
})
