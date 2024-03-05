
shinyUI(
  fluidPage(
    tags$head(
      includeCSS("www/app.css"),
      rintrojs::introjsUI(),
      shinyjs::useShinyjs()
    ),
    navbarPage(
      id = "MetaPairwise",
      title = "MetaPairwise",
      theme = shinytheme("readable"),
      
      tabPanel(
        title = "Home",
        frontPageUI(id = "Front")
      ),
      tabPanel(
        title = "Data",
        dataPageUI(id = "Data")
      ),
      tabPanel(
        title = "Calculator",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            width = 3,
            optionsPanelUI(id = "optionsPanel")
          ),
          mainPanel = mainPanel(
            tabsetPanel(
              id = "AnalysisType",
              tabPanel(
                title = "Frequentist Analysis",
                freqAnalysisUI(id = "freqAnalysis")
              ),
              tabPanel(
                title = "Bayesian Analysis",
                bayesAnalysisUI(id = "bayesAnalysis")
              )
            )
          )
        )
      )
    )
  )
)
