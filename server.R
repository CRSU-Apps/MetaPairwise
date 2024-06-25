
function(input, output, session) {
  
  frontPageServer(id = "Front")
  
  data <- dataPageServer("Data")
  
  optionsReactives <- optionsPanelServer("optionsPanel", data)
  
  Pair_ctrl <- optionsReactives$Pair_ctrl
  Pair_trt <- optionsReactives$Pair_trt
  FixRand <- optionsReactives$FixRand
  OutcomeCont <- optionsReactives$OutcomeCont
  OutcomeBina <- optionsReactives$OutcomeBina
  prior <- optionsReactives$prior
  chains <- optionsReactives$chains
  iter <- optionsReactives$iter
  burn <- optionsReactives$burn
  
  filtered_data <- reactive({
    raw_data <- data()$data
    "# Gather the treatments in each study"
    study_treatments <- sapply(
      unique(raw_data$Study),
      function(study) {
        return(FindTreatmentsForStudy(raw_data, study))
      }
    )
    "# Filter out any studies which don't compare the 2 treatments of interest"
    rows <- sapply(
      raw_data$Study,
      function(study) {
        treatments <- study_treatments[, study]
        return(Pair_trt() %in% treatments && Pair_ctrl() %in% treatments)
      }
    )
    return(raw_data[rows, ])
  })
  
  ContBin <- reactive({
    "# automatically detect if continuous or binary"
    if (max(grepl("^Mean", names(filtered_data())))) {
      return('continuous')
    } else if (max(grepl("^R", names(filtered_data())))) {
      return ('binary')
    } else {
      stop("Cannot identify data type from column names")
    }
  })
  
  output$ContBin <- ContBin
  outputOptions(output, "ContBin", suspendWhenHidden = FALSE) #needed for UI options, but doesn't need displaying itself
  
  outcome <- reactive({
    "# different outcome variables if continuous or binary"
    if (ContBin() == 'continuous') {
      OutcomeCont()
    } else if (ContBin() == 'binary') {
      OutcomeBina()
    } else {
      stop("Data type should be 'continuous' or 'binary'")
    }
  })
  
  freqAnalysisServer(id = "freqAnalysis", filtered_data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl)
  
  bayesAnalysisServer(id = "bayesAnalysis", filtered_data,  FixRand, outcome, ContBin, Pair_trt, Pair_ctrl, prior, iter, chains, burn)
}
