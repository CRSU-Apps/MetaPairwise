
PairwiseSummary_functionF <- function(outcome, model) {
  sum <- summary(model)
  line0 <- paste(strong("Results"))
  line1 <- paste("Number of studies: ", sum$k, sep = "")
  if (outcome == "OR") {
    line2 <- paste0(
      "Pooled estimate: ",
      round(exp(sum$b), 2),
      " (95% CI: ",
      round(exp(sum$ci.lb), 2),
      " to ",
      round(exp(sum$ci.ub), 2),
      "); p-value: ",
      round(sum$pval, 3)
    )
    line4 <- "Between study standard-deviation (log-odds scale): "
  } else if (outcome == "RR") {
    line2 <- paste0(
      "Pooled estimate: ",
      round(exp(sum$b), 2),
      " (95% CI: ",
      round(exp(sum$ci.lb), 2),
      " to ",
      round(exp(sum$ci.ub), 2),
      "); p-value: ",
      round(sum$pval, 3)
    )
    line4 <- "Between study standard-deviation (log-probability scale): "
  } else {
    line2 <- paste(
      "Pooled estimate: ",
      round(sum$b, 2),
      " (95% CI: ",
      round(sum$ci.lb, 2),
      " to ",
      round(sum$ci.ub, 2),
      "); p-value: ",
      round(sum$pval, 3)
    )
    line4 <- "Between study standard-deviation: "
  }
  line3 <- strong("Heterogeneity results")
  line4 <- paste0(
    line4,
    round(sqrt(sum$tau2), 3),
    "; I-squared: ",
    round(sum$I2, 1),
    "%; P-value for testing heterogeneity: ",
    round(sum$QEp, 3)
  )
  HTML(paste(line0, line1, line2, line3, line4, sep = "<br/>"))
}

PairwiseModelFit_functionF <- function(model) {
  sum <- summary(model)
  HTML(paste0("AIC: ", round(sum$fit.stats[3, 1], 2), "; BIC: ", round(sum$fit.stats[4, 1], 2)))
}

#' Create forest plot.
#'
#' @param reference Name of reference treatment.
#' @param intervention Name of intervention treatment.
#' @param meta_analysis Meta-analysis object returned from `FreqPair` function.
#' @param model_effects Either "fixed" or "random".
#' @param outcome_measure Outcome measure being analysed. One of: "OR", "RR", "RD", "MD", "SMD".
#'
#' @return Forest plot from meta-analysis.
CreatePairwiseForestPlot <- function(reference, intervention, meta_analysis, model_effects, outcome_measure) {
  if (model_effects == "fixed") {
    model <- meta_analysis$MA.Fixed
  } else if (model_effects == "random") {
    model <- meta_analysis$MA.Random
  } else {
    stop("Model effects must be 'fixed' or 'random'")
  }
  
  if (outcome_measure == "OR" || outcome_measure == "RR") {
    forestTemp <- metafor::forest(
      model,
      atransf = exp,
      ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(model), 2))
    )

    text(
      x = forestTemp$ilab.xpos,
      y = model$k + 2,
      labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
      font = 2
    )
  } else if (outcome_measure == "RD") {
    forestTemp <- metafor::forest(
      model,
      atransf = NA,
      ilab = cbind(R.1, N.1 - R.1, R.2, N.2 - R.2, round(weights(model), 2))
    )

    text(
      x = forestTemp$ilab.xpos,
      y = model$k + 2,
      labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights(%)"),
      font = 2
    )
  } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
    forestTemp <- metafor::forest(
      model,
      atransf = NA,
      ilab = cbind(Mean.1, SD.1, Mean.2, SD.2, round(weights(model), 2))
    )

    text(
      x = forestTemp$ilab.xpos,
      y = model$k + 2,
      labels = c("Mean", "SD", "Mean", "SD","Weights(%)"),
      font = 2
    )
  } else {
    stop("Outcome measure must be one of: 'OR', 'RR', 'RD', 'MD' OR 'SMD'")
  }
  
  text(
    x = c(
      (forestTemp$ilab.xpos[1] + forestTemp$ilab.xpos[2]) / 2 ,
      (forestTemp$ilab.xpos[3] + forestTemp$ilab.xpos[4]) / 2
    ),
    y = model$k + 3, 
    labels = c(intervention, reference)
  )
  title(glue::glue("Forest plot of studies with overall estimate from {model_effects}-effects model"))
  
  return(forestTemp)
}
