#' Define JAGS model for a Linear Regression
#'
#' @param jags_data The output of \code{\link{jags_data_prep}}
#' @param num_chains How many chains for the MCMC.
#' @param checks Functions to perform posterior predictive checks with
#' @param ... Additional arguments for \link[rjags2]{jags.model}
#'
#' @return JAGS model object
#' @export
#'
#' @examples
#' # coming soon
jags_model_lm <- function(jags_data, num_chains, checks = NULL, ...) {

  template <- readLines(system.file("reg-linear.txt", package = "mickjaggr"))

  if (!is.null(checks)) {

    internal_checks <- paste0("pp.", checks, " <- ", checks, "(response.new[])", collapse = " \n")

    data <- list(response_new = "response.new[i] ~ dnorm(inprod(predictors[i,], beta[]), tau)",
                 pp_checks = internal_checks)
  } else if (is.null(checks)) {
    data <- list(pp_checks = NULL)
  }


  if ("test_predictors" %in% names(jags_data)) {

    data[["predict"]] <-
    "
    for (t in 1:num_test_preds) {
    predictions ~ dnorm(inprod(test_predictors[t,], beta[]), tau)
    }
    "

    jags_data[["num_test_preds"]] <- nrow(jags_data[["test_predictors"]])
  }

  # create temp file
  tmpfile <- tempfile(fileext = ".txt")

  # write file with pp_checks
  writeLines(whisker::whisker.render(template, data), con = tmpfile)

  model <- rjags::jags.model(file = tmpfile,
                             data = jags_data,
                             n.chains = num_chains,
                             ...)

  on.exit(unlink(tmpfile))

  return(model)
}

#' Define JAGS model for a Poisson Regression
#'
#' @inheritParams jags_model_lm
#' @return JAGS model object
#' @export
#'
#' @examples
#' # coming soon
jags_model_poisson <- function(jags_data, num_chains, checks = NULL, ...) {

  template <- readLines(system.file("reg-poisson.txt", package = "mickjaggr"))

  if (!is.null(checks)) {

    internal_checks <- paste0("pp.", checks, " <- ", checks, "(response.new[])", collapse = " \n")

    data <- list(response_new = "response.new[i] ~ dpois(theta[i])",
                 pp_checks = internal_checks)
  } else if (is.null(checks)) {
    data <- list(pp_checks = NULL)
  }

  # create temp file
  tmpfile <- tempfile(fileext = ".txt")

  # write file with pp_checks
  writeLines(whisker::whisker.render(template, data), con = tmpfile)

  model <- rjags::jags.model(file = tmpfile,
                             data = jags_data,
                             n.chains = num_chains,
                             ...)

  on.exit(unlink(tmpfile))

  return(model)

}


#' Run JAGS models
#'
#' @param .jags_model The output of \code{\link{jags_model_define}}
#' @param num_iter Number of iterations per chain.
#' @param variable_names The variables to save from JAGS output.
#' @param ... Additional arguments for \link[rjags2]{coda.samples}
#'
#' @return A JAGS model output of class "mcmc.list"
#' @export
#'
#' @examples
#' # coming soon
jags_model_run <- function(.jags_model, num_iter, variable_names, checks = NULL, ...) {

  if (!is.null(checks)) {
    variable_names <- c(variable_names, paste0("pp.", checks))
  }

  if ("test_predictors" %in% names(.jags_model$data())) {
    variable_names <- c(variable_names, "test_predictions")
  }

  rjags::coda.samples(model = .jags_model,
                      variable.names = variable_names,
                      n.iter = num_iter,
                      ...)
}
