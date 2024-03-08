
shinyUI(
  fluidPage(
    tags$head(
      includeCSS("www/app.css"),
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      
      includeHTML("www/favicon/favicon.html"),
      tags$meta(name="description", content="A interactive web tool for pairwise meta-analysis that leverages established analysis routines"),
      tags$meta(name="keywords", content="MetaPairwise, Pairwise, Meta, Analysis, App"),
      tags$meta(property="og:title", content="Meta Pairwise"),
      tags$meta(property="og:description", content="An interactive web tool for pairwise meta-analysis that leverages established analysis routines"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaPairwise/main/www/images/MetaPairwiseLogo.png")
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
