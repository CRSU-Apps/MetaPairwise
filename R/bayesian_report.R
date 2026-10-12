#' Render an HTML report for a Bayesian pairwise meta-analysis.
#'
#' @param output_file Path of the HTML file to create.
#' @param meta_analysis Meta-analysis object returned from `BayesPair` function.
#' @param model_effects Either "fixed" or "random".
#' @param outcome_measure Outcome measure being analysed. One of: "OR", "RR", "RD", "MD", "SMD".
#' @param data_type Either "binary" or "continuous".
#' @param intervention Name of intervention treatment.
#' @param reference Name of reference treatment.
#' @param prior Prior distribution for the between-study standard deviation.
#' @param chains Number of MCMC chains.
#' @param iter Number of iterations per chain.
#' @param burn Number of burn-in iterations per chain.
#' @param seed Seed used for the random number generator.
#' @param trace_plot Trace plot to include in the report.
#' @param forest_plot Forest plot to include in the report.
#'
#' @return Path of the created HTML file.
CreateBayesianReport <- function(
    output_file, meta_analysis, model_effects, outcome_measure, data_type, intervention, reference,
    prior, chains, iter, burn, seed, trace_plot, forest_plot) {
  # Render in a temporary directory, as the app directory may not be writable
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  template <- file.path(tmp_dir, "bayesian_report.Rmd")
  file.copy("reports/bayesian_report.Rmd", template)
  
  rendered <- rmarkdown::render(
    input = template,
    output_dir = tmp_dir,
    params = list(
      meta_analysis = meta_analysis,
      model_effects = model_effects,
      outcome_measure = outcome_measure,
      data_type = data_type,
      intervention = intervention,
      reference = reference,
      prior = prior,
      chains = chains,
      iter = iter,
      burn = burn,
      seed = seed,
      trace_plot = trace_plot,
      forest_plot = forest_plot
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  
  file.copy(rendered, output_file, overwrite = TRUE)
  return(output_file)
}
