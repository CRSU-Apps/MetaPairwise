library(shiny)

# Define UI 
ui <- fluidPage(
  FrequentistForestPlotUI(id = "ForestPlotPairF")
)

# Define server logic
server <- function(input, output) {
  # Load data
  loaded_data <- reactive({
    return(rio::import("data/AntiVEGF_Continuous_Pairwise_Long.csv"))
  })
  
  FixRand <- reactive({ "fixed" })
  ContBin <- reactive({ "continuous" })
  outcome <- reactive({ "MD" })
  Pair_trt <- reactive({ "BEVA" })
  Pair_ctrl <- reactive({ "RANI" })
  
  # Clean and sort data
  cleaned_data <- reactive({
    return(CleanData(loaded_data()))
  })
  
  # Wrangle data into useful form
  wrangled_data <- reactive({
    WrangleUploadData(cleaned_data())
  })
  
  # Extract treatment names/levels
  data_levels <- reactive({
    return(
      levels(
        as_vector(
          lapply(
            wrangled_data()[grep(pattern = "^T", names(wrangled_data()), value = TRUE)],
            factor
          )
        )
      )
    )
  })
  
  # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
  WideData <- shinymeta::metaReactive({
    SwapTrt(
      CONBI = shinymeta::..(ContBin()),
      data = Long2Wide(data = shinymeta::..(wrangled_data())),
      trt = shinymeta::..(Pair_trt())
    )
  })
  
  freqpair <- shinymeta::metaReactive({
    shinymeta::metaExpr({
      FreqPair(
        data = shinymeta::..(WideData()),
        outcome = shinymeta::..(outcome()),
        model = 'both',
        CONBI = shinymeta::..(ContBin())
      )
    })
  })
  
  FrequentistForestPlotServer(
    id = "ForestPlotPairF",
    freqpair = freqpair,
    FixRand = FixRand,
    outcome = outcome,
    Pair_trt = Pair_trt,
    Pair_ctrl = Pair_ctrl
  )
}

# Run the application 
shinyApp(ui = ui, server = server)