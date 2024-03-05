
PairwiseSummary_functionB <- function(outcome, MA.Model, model) {   # MA.Model has to have MAData, MA.Fixed and MA.Random
  line0 <- strong("Results")
  line1 <- paste0("Number of studies: ", nrow(MA.Model$MA.Fixed$data_wide)) # same for fixed or random
  if (model == "random") {
    # already exponentiated where needed within BayesPair function
    line2 <- paste0(
      "Pooled estimate: ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "RE Model", "est"], 2),
      " (95% CI: ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "RE Model","lci"], 2),
      " to ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "RE Model","uci"], 2),
      ")"
    )
    if (outcome == "OR") {
      line3 <- "Between study standard-deviation (log-odds scale): "
    } else if (outcome == "RR") {
      line3 <- "Between study standard-deviation (log-probability scale): "
    } else {
      line3 <- "Between study standard-deviation: "
    }
    line3 <- paste0(
      line3,
      round(MA.Model$MA.Random$fit_sum["tau[1]", 1], 3),
      " (95% CI: ",
      round(MA.Model$MA.Random$fit_sum["tau[1]", 4], 3),
      " to ",
      round(MA.Model$MA.Random$fit_sum["tau[1]", 8], 3),
      ")"
    )
  } else {
    # already exponentiated where needed within BayesPair function
    line2 <- paste(
      "Pooled estimate: ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "FE Model", "est"], 2),
      " (95% CI: ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "FE Model", "lci"], 2),
      " to ",
      round(MA.Model$MAdata[MA.Model$MAdata$Study == "FE Model", "uci"], 2),
      ")"
    )
    line3 <- "For fixed models, between study standard-deviation is set to 0."
  }
  HTML(paste(line0, line1, line2, line3, sep = "<br/>"))
}
