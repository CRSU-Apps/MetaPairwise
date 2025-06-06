
MetaCreatePairwiseSummaryAndFit <- shinymeta::metaAction({
  PairwiseSummary_functionF <- function(outcome, model) {
    sum <- summary(model)
    line0 <- "Results"
    line1 <- paste0("Number of studies: ", sum$k)
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
    line3 <- "Heterogeneity results"
    line4 <- paste0(
      line4,
      round(sqrt(sum$tau2), 3),
      "; I-squared: ",
      round(sum$I2, 1),
      "%; P-value for testing heterogeneity: ",
      round(sum$QEp, 3)
    )
    return(c(line0, line1, line2, line3, line4))
  }

  PairwiseModelFit_functionF <- function(model) {
    sum <- summary(model)
    return(paste0("AIC: ", round(sum$fit.stats[3, 1], 2), "; BIC: ", round(sum$fit.stats[4, 1], 2)))
  }
})

#' Create forest plot.
#'
#' @param reference Name of reference treatment.
#' @param intervention Name of intervention treatment.
#' @param meta_analysis Meta-analysis object returned from `FreqPair` function.
#' @param model_effects Either "fixed" or "random".
#' @param outcome_measure Outcome measure being analysed. One of: "OR", "RR", "RD", "MD", "SMD".
#'
#' @return Forest plot from meta-analysis.
MetaCreatePairwiseForestPlot <- shinymeta::metaAction({
  CreatePairwiseForestPlot <- function(reference, intervention, meta_analysis, model_effects, outcome_measure) {
    if (model_effects == "fixed") {
      model <- meta_analysis$MA.Fixed
      other_model <- meta_analysis$MA.Random
    } else if (model_effects == "random") {
      model <- meta_analysis$MA.Random
      other_model <- meta_analysis$MA.Fixed
    } else {
      stop("Model effects must be 'fixed' or 'random'")
    }
    
    if (outcome_measure == "OR" || outcome_measure == "RR") {
      forestTemp <- metafor::forest(
        x = model,
        atransf = exp,
        ilab = cbind(R.2, N.2 - R.2, R.1, N.1 - R.1, round(weights(model), 2)),
        ylim = c(-2.5, model$k + 3)
      )
      metafor::addpoly(other_model)
  
      text(
        x = forestTemp$ilab.xpos,
        y = model$k + 2,
        labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights\n(%)"),
        font = 2
      )
    } else if (outcome_measure == "RD") {
      forestTemp <- metafor::forest(
        x = model,
        ilab = cbind(R.2, N.2 - R.2, R.1, N.1 - R.1, round(weights(model), 2)),
        ylim = c(-2.5, model$k + 3)
      )
      metafor::addpoly(other_model)
  
      text(
        x = forestTemp$ilab.xpos,
        y = model$k + 2,
        labels = c("Pos+", "Neg-", "Pos+", "Neg-","Weights\n(%)"),
        font = 2
      )
    } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
      forestTemp <- metafor::forest(
        x = model,
        ilab = cbind(Mean.2, SD.2, Mean.1, SD.1, round(weights(model), 2)),
        ylim = c(-2.5, model$k + 3)
      )
      metafor::addpoly(other_model)
  
      text(
        x = forestTemp$ilab.xpos,
        y = model$k + 2,
        labels = c("Mean", "SD", "Mean", "SD","Weights\n(%)"),
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
      y = model$k + 3.3,
      labels = c(reference, intervention),
      pos = 1
    )
    title(glue::glue("Forest plot of studies with overall estimate from {model_effects}-effects model"))
    
    return(forestTemp)
  }
})
