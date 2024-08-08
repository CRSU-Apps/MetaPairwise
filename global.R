
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
  library(tidybayes)
  library(tidyr)
  library(tidyverse)
  
  "# Source all files in 'R' directory"
  library(R.utils)
  R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)
})
