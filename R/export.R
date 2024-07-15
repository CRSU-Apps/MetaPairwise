ExportFrequentistJson <- function(meta_analysis, model_effects, outcome_measure, filename) {
  fixed <- meta_analysis$MA.Fixed
  random <- meta_analysis$MA.Random
  
  if (model_effects == "fixed") {
    model <- fixed
  } else if (model_effects == "random") {
    model <- random
  } else {
    stop("Model effects must be 'fixed' or 'random'")
  }
  
  # individual <- list(Study = model$data$Study)
  pooled_results <- list()
  
  if (outcome_measure == "OR" || outcome_measure == "RR") {
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
            "beta" = exp(model$data$yi[index]),
            "2.5%" = exp(model$data$yi[index] - 1.96 * model$data$sei[index]),
            "97.5%" = exp(model$data$yi[index] + 1.96 * model$data$sei[index])
          )
        )
      }
    )
    
    # individual[["reference-r"]] <- model$data$R.2
    # individual[["reference-n"]] <- model$data$N.2
    # individual[["intervention-r"]] <- model$data$R.1
    # individual[["intervention-n"]] <- model$data$N.1
    # individual[["weight"]] <- weights(model) / 100
    # individual[["beta"]] <- exp(model$data$yi)
    # individual[["2.5%"]] <- exp(model$data$yi - 1.96 * model$data$sei)
    # individual[["97.5%"]] <- exp(model$data$yi + 1.96 * model$data$sei)
    
    pooled_results[["fixed"]] <- list(
      "beta" = exp(fixed$beta[1]),
      "2.5%" = exp(fixed$ci.lb),
      "97.5%" = exp(fixed$ci.ub),
      "p" = fixed$pval
    )
    pooled_results[["random"]] <- list(
      "beta" = exp(random$beta[1]),
      "2.5%" = exp(random$ci.lb),
      "97.5%" = exp(random$ci.ub),
      "p" = random$pval
    )
    
  } else if (outcome_measure == "RD") {
    individual[["reference-r"]] <- model$data$R.2
    individual[["reference-n"]] <- model$data$N.2
    individual[["intervention-r"]] <- model$data$R.1
    individual[["intervention-n"]] <- model$data$N.1
    individual[["weight"]] <- weights(model) / 100
    individual[["beta"]] <- model$data$yi
    individual[["2.5%"]] <- model$data$yi - 1.96 * model$data$sei
    individual[["97.5%"]] <- model$data$yi + 1.96 * model$data$sei
    
    pooled_results[["fixed"]] <- list(
      "beta" = fixed$beta[1],
      "2.5%" = fixed$ci.lb,
      "97.5%" = fixed$ci.ub,
      "p" = fixed$pval
    )
    pooled_results[["random"]] <- list(
      "beta" = random$beta[1],
      "2.5%" = random$ci.lb,
      "97.5%" = random$ci.ub,
      "p" = random$pval
    )
    
  } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
    individual[["reference-mean"]] <- model$data$Mean.2
    individual[["reference-sd"]] <- model$data$SD.2
    individual[["intervention-mean"]] <- model$data$Mean.1
    individual[["intervention-sd"]] <- model$data$SD.1
    individual[["weight"]] <- weights(model) / 100
    individual[["beta"]] <- model$data$yi
    individual[["2.5%"]] <- model$data$yi - 1.96 * model$data$sei
    individual[["97.5%"]] <- model$data$yi + 1.96 * model$data$sei
    
    pooled_results[["fixed"]] <- list(
      "beta" = fixed$beta[1],
      "2.5%" = fixed$ci.lb,
      "97.5%" = fixed$ci.ub,
      "p" = fixed$pval
    )
    pooled_results[["random"]] <- list(
      "beta" = random$beta[1],
      "2.5%" = random$ci.lb,
      "97.5%" = random$ci.ub,
      "p" = random$pval
    )
    
  } else {
    stop("Outcome measure must be one of: 'OR', 'RR', 'RD', 'MD' OR 'SMD'")
  }
  
  pooled_herterogeneity <- list(
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
    heterogeneity = pooled_herterogeneity,
    model_fit = pooled_model_fit
  )
  data <- list(
    individual = individual,
    pooled = pooled
  )
  
  json <- jsonlite::toJSON(
    pretty = TRUE,
    auto_unbox = TRUE,
    x = data
  )
  
  write_file(x = json, file = filename)
}

ExportBayesianJson <- function(meta_analysis, model_effects, outcome_measure, filename) {
  data <- meta_analysis$MAdata
  fixed <- meta_analysis$MA.Fixed
  random <- meta_analysis$MA.Random
  
  if (model_effects == "fixed") {
    model <- fixed
  } else if (model_effects == "random") {
    model <- random
  } else {
    stop("Model effects must be 'fixed' or 'random'")
  }
  
  # individual <- list(Study = model$data$Study)
  pooled_results <- list()
  
  if (outcome_measure == "OR" || outcome_measure == "RR") {
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
            "beta" = exp(data$yi[index]),
            "2.5%" = data$lci[index],
            "97.5%" = data$uci[index]
          )
        )
      }
    )
    
    # individual[["reference-r"]] <- data$R.2
    # individual[["reference-n"]] <- data$N.2
    # individual[["intervention-r"]] <- data$R.1
    # individual[["intervention-n"]] <- data$N.1
    # individual[["beta"]] <- exp(data$yi)
    # individual[["2.5%"]] <- exp(data$lci)
    # individual[["97.5%"]] <- exp(data$uci)
    
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
    
  } else if (outcome_measure == "RD") {
    individual[["reference-r"]] <- data$R.2
    individual[["reference-n"]] <- data$N.2
    individual[["intervention-r"]] <- data$R.1
    individual[["intervention-n"]] <- data$N.1
    individual[["beta"]] <- data$yi
    individual[["2.5%"]] <- data$lci
    individual[["97.5%"]] <- data$uci
    
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
    
  } else if (outcome_measure == "MD" || outcome_measure == "SMD") {
    individual[["reference-mean"]] <- data$Mean.2
    individual[["reference-sd"]] <- data$SD.2
    individual[["intervention-mean"]] <- data$Mean.1
    individual[["intervention-sd"]] <- data$SD.1
    individual[["beta"]] <- data$yi
    individual[["2.5%"]] <- data$lci
    individual[["97.5%"]] <- data$uci
    
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
    
  } else {
    stop("Outcome measure must be one of: 'OR', 'RR', 'RD', 'MD' OR 'SMD'")
  }
  
  pooled_herterogeneity <- list(
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
    heterogeneity = pooled_herterogeneity,
    model_fit = pooled_model_fit
  )
  data <- list(
    individual = individual,
    pooled = pooled
  )
  
  json <- jsonlite::toJSON(
    pretty = TRUE,
    auto_unbox = TRUE,
    x = data
  )
  
  write_file(x = json, file = filename)
}
