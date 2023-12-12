# MetaImpact Server #
#-------------------#

#----------------#
# Server Content #
#----------------#
function(input, output, session) {
  
  
  data <- dataPageServer("Data")

  
  optionsReactives <- optionsPanelServer("optionsPanel",data)
  
  Pair_ctrl=optionsReactives$Pair_ctrl
  Pair_trt=optionsReactives$Pair_trt
  FixRand=optionsReactives$FixRand
  OutcomeCont=optionsReactives$OutcomeCont
  OutcomeBina=optionsReactives$OutcomeBina
  prior=optionsReactives$prior
  chains=optionsReactives$chains
  iter=optionsReactives$iter
  burn=optionsReactives$burn
  
  ContBin <- reactive({           # automatically detect if continuous or binary
    if (max(grepl("^Mean", names(data()$data)))==TRUE) {
      return('continuous')
    } else if (max(grepl("^R", names(data()$data)))==TRUE) {
      return ('binary')
    }
  })
  output$ContBin <- renderText({
    ContBin()
  })
  outputOptions(output, "ContBin", suspendWhenHidden=FALSE) #needed for UI options, but doesn't need displaying itself
  
  outcome <- reactive({                  # different outcome variables if continuous or binary
    if (ContBin()=='continuous') {
      OutcomeCont()
    } else {
      OutcomeBina()
    }
  })
  
  freqAnalysisServer(id="freqAnalysis", data, FixRand, outcome, ContBin, Pair_trt, Pair_ctrl)
  
  bayesAnalysisServer(id="bayesAnalysis", data,  FixRand, outcome, ContBin, Pair_trt, Pair_ctrl, prior, iter, chains, burn)
  
  
}
