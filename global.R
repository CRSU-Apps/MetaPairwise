
# load libraries

library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinyWidgets)

MetaLoadLibraries <- shinymeta::metaAction({
  LoadOrInstall <- function(package_name) {
    if (!require(package_name, character.only = TRUE)) {
      install.packages(package_name)
      library(package_name, character.only = TRUE)
    }
  }
  
  LoadOrInstall("dplyr")
  LoadOrInstall("forcats")
  LoadOrInstall("ggplot2")
  LoadOrInstall("glue")
  LoadOrInstall("markdown")
  LoadOrInstall("metafor")
  LoadOrInstall("MetaStan")
  LoadOrInstall("purrr")
  LoadOrInstall("rintrojs")
  LoadOrInstall("rio")
  LoadOrInstall("rmarkdown")
  LoadOrInstall("rstan")
  LoadOrInstall("tidybayes")
  LoadOrInstall("tidyr")
  LoadOrInstall("tidyverse")
  
  "# Source all files in 'R' directory"
  LoadOrInstall("R.utils")
  R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)
})
