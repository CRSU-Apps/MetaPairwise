# MetaImpact UI #
#---------------#

# load libraries #
#----------------#
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(rintrojs)
library(shinydashboard)
library(markdown)


# load user-written functions #
#-----------------------------#



# UI Content #
#------------#
shinyUI(
  fluidPage(
  
  includeCSS("www/app.css"),
  
  navbarPage(id="MetaPairwise",
             title="MetaPairwise",
             theme = shinytheme("readable"),
             
            
             # Data Tab #
             #----------#
             
             tabPanel("Data", dataPageUI(id="Data")),

             
             
             # Evidence Synthesis Tab #
             #------------------------#
             
             tabPanel("Calculator",
                      
                      # Meta-analysis #
                      #---------------#
                      
                      sidebarLayout(
                        
                        sidebarPanel(optionsPanelUI(id="optionsPanel"),width=3),
                        
                         mainPanel(

                           tabsetPanel(id="AnalysisType",
                                       type="tabs",
                                       
                                       tabPanel("Frequentist Analysis",freqAnalysisUI(id="freqAnalysis")),
                                       
                                       tabPanel("Bayesian Analysis",bayesAnalysisUI(id="bayesAnalysis"))
                                       )
                                  )
                                    )
                      )
             )
                  )
        )