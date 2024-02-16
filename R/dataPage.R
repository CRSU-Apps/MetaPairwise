
#' UI for the load data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
dataPageUI <- function(id) {
  ns = NS(id)
  div(
    column(
      width = 4,
      # Insert own data or choose example data
      h4("Choose Data"),
      p("Please upload your data as a .csv file, formatted as described on the right-hand side of this page. Treatment coding (i.e. numbering rather than labels) and specifying whether the outcome is continuous or binary is not necessary."),
      fileInput(
        inputId = ns("data"),
        label = NULL,
        buttonLabel = "Select",
        accept = c(".csv", ".xlsx")
      ),
      br(),
      p("If you wish to explore the app without using your own data, you are welcome to choose one of the example datasets below."),
      p(
        "Example datasets are based on (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema. Visual acuity (VA) outcomes were reported and chosen for these examples. The continuous outcome example is extracted from a meta-analysis by Virgili et al which can be found ",
        a(href = "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here."),
        "The binary outcome example is extracted from a meta-analysis by Pham et al which can be found ",
        a(href = "https://bmjopen.bmj.com/content/9/5/e022031", "here.")
      ),
      p("Options to explore an example network meta-analysis will be available in the near future."),
      radioButtons(
        inputId = ns("ChooseExample"),
        label = "Example Datasets Available",
        choices = c(
          "Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx",
          "Binary outcome: Number of people that improved their best-corrected VA by gaining 15+ letters during a vision test" = "binaryEx"
        ),
        width = '100%'
      )
    ),
   
    column(
      width = 4,
      h4("View Data"),
      uiOutput(outputId = ns("data"))
    ),
   
    column(
      width = 4,
      h4("Format Requirements"),
          
      tabsetPanel(
        id = 'format_instructions',
        
        tabPanel(
          title = "Binary Data",
          p("MetaPairwise can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
          p("The data file should contain five or eight columns columns for long or wide format respectively. Headings of columns are case sensitive."),
          p("The following columns are needed:"),
          tags$ul(
            tags$li(
              "A column labelled ",
              tags$strong("StudyID"),
              " containing the study identifier, starting from 1, then 2, 3, 4... etc."
            ),
            tags$li(
              "A column labelled ",
              tags$strong("Study"),
              " containing the name (e.g., author, year) of the study. The study name must be unique for each study."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("T"),
              " containing the name or label of treatment used in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("T.1 & T.2"),
              " containing the name or label of treatment given for study arm 1 and 2 respectively."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("R"),
              " containing the number of participants with the outcome of interest in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("R.1 & R.2"),
              " containing the number of participants with the outcome of interest for study arm 1 and 2 respectively."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("N"),
              " containing the number of participants in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("N.1 & N.2"),
              " containing the number of participants for study arm 1 and 2 respectively."
            )
          )
        ),
    
        tabPanel(
          title = "Continuous Data",
          p("MetaImpact can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
          p("The data file should contain six or ten columns columns for long or wide format respectively. Headings of columns are case sensitive."),
          p("The following columns are needed:"),
          tags$ul(
            tags$li(
              "A column labelled ",
              tags$strong("StudyID"),
              " containing the study identifier, starting from 1, then 2, 3, 4... etc."
            ),
            tags$li(
              "A column labelled ",
              tags$strong("Study"),
              " containing the name (e.g., author, year) of the study. The study name must be unique for each study."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("T"),
              " containing the name or label of treatment used in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("T.1 & T.2"),
              " containing the name or label of treatment given for study arm 1 and 2 respectively."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("Mean"),
              " containing the mean value of the outcome in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("Mean.1 & Mean.2"),
              " containing the the mean value of the outcome for study arm 1 and 2 respectively."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("SD"),
              " containing the standard deviation value of the outcome in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("SD.1 & SD.2"),
              " containing the the standard deviation value of the outcome for study arm 1 and 2 respectively."
            ),
            tags$li(
              "For long format, a column labelled ",
              tags$strong("N"),
              " containing the number of participants in each arm of the study.",
              " For wide format, two columns labelled ",
              tags$strong("N.1 & N.2"),
              " containing the number of participants for study arm 1 and 2 respectively."
            )
          )
        )
      )
    )
  )
}

#' Server for the load data page.
#'
#' @param id ID of the module.
#'
#' @return Reactive containing loaded data frame.
dataPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Read in user or default data
      data <- reactive({
        file <- input$data
        if (is.null(file)) {
          if (input$ChooseExample == 'continuousEx') {
            data <- read.csv("AntiVEGF_Continuous_Pairwise.csv")
          } else {
            data <- read.csv("AntiVEGF_Binary_Pairwise.csv")
          }
        } else {
          data <- read.table(file = file$datapath)
        }
        # extract treatment names/levels
        levels <- levels(
          as_vector(
            lapply(
              data[grep(pattern = "^T", names(data), value = TRUE)],
              factor
            )
          )
        )
        return(list(data = data, levels = levels))
      })
      
      # Create a table which displays the raw data just uploaded by the user
      output$data <- renderTable({
        data()$data
      })
      
      return(data)
    }
  )
}