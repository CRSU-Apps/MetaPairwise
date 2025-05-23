#' Export the frequentist meta-analysis results to a json string.
#'
#' @param meta_analysis {metafor} output from function `FreqPair()`.
#' @param model_effects Type of model effects; either "fixed" or "random".
#' @param outcome_measure Type of outcome measure; one of "OR", "RR", "RD", "MD", SMD".
#' 
#' @return The meta-analysis results in a JSON string.
ExportFrequentistJson <- function(meta_analysis, model_effects, outcome_measure) {
  fixed <- meta_analysis$MA.Fixed
  random <- meta_analysis$MA.Random
  
  # Get the primary model from the analysis
  if (model_effects == "fixed") {
    model <- fixed
  } else if (model_effects == "random") {
    model <- random
  } else {
    stop("Model effects must be 'fixed' or 'random'")
  }
  
  # Exponentiate results if required, else do not transform the results
  if (outcome_measure == "OR" || outcome_measure == "RR") {
    transform <- exp
  } else {
    transform <- identity
  }
  
  pooled_results <- list()
  
  # Binary export
  if (outcome_measure == "RD" || outcome_measure == "OR" || outcome_measure == "RR") {
    individual <- lapply(
      1:length(model$data$yi),
      function(index) {
        return(
          list(
            "study" = model$data$Study[index],
            "reference-r" = model$data$R.2[index],
            "reference-n" = model$data$N.2[index],
            "intervention-r" = model$data$R.1[index],
            "intervention-n" = model$data$N.1[index],
            "weight" = weights(model)[index] / 100,
            "beta" = transform(model$data$yi[index]),
            "2.5%" = transform(model$data$yi[index] - 1.96 * model$data$sei[index]),
            "97.5%" = transform(model$data$yi[index] + 1.96 * model$data$sei[index])
          )
        )
      }
    )
    # Continuous export
  } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
    individual <- lapply(
      1:length(model$data$yi),
      function(index) {
        return(
          list(
            "study" = model$data$Study[index],
            "reference-mean" = model$data$Mean.2[index],
            "reference-sd" = model$data$SD.2[index],
            "reference-n" = model$data$N.2[index],
            "intervention-mean" = model$data$Mean.1[index],
            "intervention-sd" = model$data$SD.1[index],
            "intervention-n" = model$data$N.1[index],
            "weight" = weights(model)[index] / 100,
            "beta" = model$data$yi[index],
            "2.5%" = model$data$yi[index] - 1.96 * model$data$sei[index],
            "97.5%" = model$data$yi[index] + 1.96 * model$data$sei[index]
          )
        )
      }
    )
  } else {
    stop("Outcome measure must be one of: 'OR', 'RR', 'RD', 'MD' OR 'SMD'")
  }
  
  pooled_results[["fixed"]] <- list(
    "beta" = transform(fixed$beta[1]),
    "2.5%" = transform(fixed$ci.lb),
    "97.5%" = transform(fixed$ci.ub),
    "p" = fixed$pval
  )
  pooled_results[["random"]] <- list(
    "beta" = transform(random$beta[1]),
    "2.5%" = transform(random$ci.lb),
    "97.5%" = transform(random$ci.ub),
    "p" = random$pval
  )
  
  pooled_heterogeneity <- list(
    fixed = list(
      "sd" = sqrt(fixed$tau2),
      "i^2" = fixed$I2 / 100,
      "p" = fixed$QEp
    ),
    random = list(
      "sd" = sqrt(random$tau2),
      "i^2" = random$I2 / 100,
      "p" = random$QEp
    )
  )
  
  pooled_model_fit <- list(
    fixed = list(
      aic = fixed$fit.stats$ML[3],
      bic = fixed$fit.stats$ML[4]
    ),
    random = list(
      aic = random$fit.stats$ML[3],
      bic = random$fit.stats$ML[4]
    )
  )
  
  pooled <- list(
    results = pooled_results,
    heterogeneity = pooled_heterogeneity,
    model_fit = pooled_model_fit
  )
  
  data <- list(
    individual = individual,
    pooled = pooled
  )
  
  return(
    jsonlite::toJSON(
      pretty = TRUE,
      auto_unbox = TRUE,
      x = data
    )
  )
}

#' Export the bayesian meta-analysis results to a json string
#'
#' @param meta_analysis {MetaStan} output from function `BayesPair()`.
#' @param model_effects Type of model effects; either "fixed" or "random".
#' @param outcome_measure Type of outcome measure; one of "OR", "RR", "RD", "MD", SMD".
#' 
#' @return The meta-analysis results in a JSON string.
ExportBayesianJson <- function(meta_analysis, model_effects, outcome_measure) {
  data <- meta_analysis$MAdata
  fixed <- meta_analysis$MA.Fixed
  random <- meta_analysis$MA.Random
  
  # Get the primary model from the analysis
  if (model_effects == "fixed") {
    model <- fixed
  } else if (model_effects == "random") {
    model <- random
  } else {
    stop("Model effects must be 'fixed' or 'random'")
  }
  
  # Exponentiate results if required, else do not transform the results
  if (outcome_measure == "OR" || outcome_measure == "RR") {
    transform <- exp
  } else {
    transform <- identity
  }
  
  pooled_results <- list()
  
  # Binary export
  if (outcome_measure == "RD" || outcome_measure == "OR" || outcome_measure == "RR") {
    individual <- lapply(
      1:(length(data$Study) - 2),
      function(index) {
        return(
          list(
            "study" = data$Study[index],
            "reference-r" = data$R.2[index],
            "reference-n" = data$N.2[index],
            "intervention-r" = data$R.1[index],
            "intervention-n" = data$N.1[index],
            "beta" = transform(data$yi[index]),
            "2.5%" = data$lci[index],
            "97.5%" = data$uci[index]
          )
        )
      }
    )
    # Continuous export
  } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
    individual <- lapply(
      1:(length(data$Study) - 2),
      function(index) {
        return(
          list(
            "study" = data$Study[index],
            "reference-mean" = data$Mean.2[index],
            "reference-sd" = data$SD.2[index],
            "reference-n" = data$N.2[index],
            "intervention-mean" = data$Mean.1[index],
            "intervention-sd" = data$SD.1[index],
            "intervention-n" = data$N.1[index],
            "beta" = data$yi[index],
            "2.5%" = data$lci[index],
            "97.5%" = data$uci[index]
          )
        )
      }
    )
  } else {
    stop("Outcome measure must be one of: 'OR', 'RR', 'RD', 'MD' OR 'SMD'")
  }
  
  pooled_results[["fixed"]] <- list(
    "beta" = data$est[data$Study == "FE Model"],
    "2.5%" = data$lci[data$Study == "FE Model"],
    "97.5%" = data$uci[data$Study == "FE Model"]
  )
  pooled_results[["random"]] <- list(
    "beta" = data$est[data$Study == "RE Model"],
    "2.5%" = data$lci[data$Study == "RE Model"],
    "97.5%" = data$uci[data$Study == "RE Model"]
  )
  
  pooled_heterogeneity <- list(
    fixed = list(
      "sd" = 0
    ),
    random = list(
      "sd" = random$fit_sum["tau[1]", 1]
    )
  )
  
  pooled_model_fit <- list(
    fixed = list(
      "r-hat" = fixed$Rhat.max
    ),
    random = list(
      "r-hat" = random$Rhat.max
    )
  )
  
  pooled <- list(
    results = pooled_results,
    heterogeneity = pooled_heterogeneity,
    model_fit = pooled_model_fit
  )
  
  data <- list(
    individual = individual,
    pooled = pooled
  )
  
  return(
    jsonlite::toJSON(
      pretty = TRUE,
      auto_unbox = TRUE,
      x = data
    )
  )
}
