# MetaPairwise UI #
#---------------#



# UI Content #
#------------#
shinyUI(
  fluidPage(
  
  includeCSS("www/app.css"),
  
  rintrojs::introjsUI(),
  shinyjs::useShinyjs(),
  
  navbarPage(id="MetaPairwise",
             title="MetaPairwise",
             theme = shinytheme("readable"),
             
             # Front Page #
             #------------#
             
             tabPanel("Home", frontPageUI(id="Front")),
             
            
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