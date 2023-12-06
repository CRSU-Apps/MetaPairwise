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
                                       
                                       tabPanel("Frequentist Analysis",
                                                fluidRow(align="center",
                                                         br(),
                                                         actionButton("FreqRun", "Run frequentist meta-analysis", class="btn-primary btn-lg")
                                                         ),
                                                conditionalPanel(condition = "input.FreqRun!=0",
                                                                fluidRow(p(htmlOutput("SynthesisSummaryFreq"))
                                                                         ),
                                                                p("To change the model options, please adjust synthesis options and re-run analysis."),
                                                                fluidRow(align='center', withSpinner(htmlOutput("SummaryTableF"))
                                                                         ),
                                                                fluidRow(p(strong("Model fit statistics"))
                                                                         ),
                                                                dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                h6("Model fit statistics"),
                                                                p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
                                                                htmlOutput("ModelFitF")),
                                                                fluidRow(column(10, withSpinner(plotOutput("ForestPlotPairF"))),
                                                                         column(2,radioButtons('forestpairF_choice', "Download forest plot as:", c('pdf','png')),
                                                                                 downloadButton('forestpairF_download', "Download forest plot"),    #Forest plot
                                                                                )
                                                                         )
                                                                 )
                                                 ),
                                       
                                      tabPanel("Bayesian Analysis",
                                               fluidRow(align="center",
                                                        br(),
                                                        actionButton("BayesRun", "Run Bayesian meta-analysis", class="btn-primary btn-lg")
                                                        ),
                                               conditionalPanel(condition = "input.BayesRun!=0",
                                                                fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                                                                         p("To change the model options, please adjust synthesis options above and re-run analysis.")
                                                                         ),
                                                                fluidRow(align='center', withSpinner(htmlOutput("SummaryTableB"))),   # Summary table
                                                                fluidRow(p(strong("Model assessment"))),
                                                                dropMenu(dropdownButton(size='xs',icon=icon('info')
                                                                                        ), 
                                                                         align='left',
                                                                         h6("Model assessment"),
                                                                         p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
                                                                         p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")
                                                                         ),
                                                                htmlOutput("ModelFitB"),
                                                                h3("Trace Plot"),
                                                                fluidRow(column(10,withSpinner(plotOutput("TracePlot"))
                                                                                ),                            # Trace plot
                                                                         column(2,radioButtons('tracepair_choice', "Download trace plot as:", c('pdf','png')),
                                                                                  downloadButton('tracepair_download', "Download trace plot")
                                                                                )
                                                                         ),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                h3("Forest plot"),
                                                                fluidRow(column(10,withSpinner(plotOutput("ForestPlotPairB")),   # Forest plot)
                                                                                ),                            
                                                                         column(2,radioButtons('forestpairB_choice', "Download forest plot as:", c('pdf','png')),
                                                                                  downloadButton('forestpairB_download', "Download forest plot")
                                                                                )
                                                                         )
                                                                                             
                                                                )
                                               )
                                       )
                                  )
                                    )
                      )
             )
                  )
        )