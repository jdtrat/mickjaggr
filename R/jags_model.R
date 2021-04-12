#' Define JAGS model
#'
#' @param jags_data The output of \code{\link{jags_data_prep}}
#' @param num_chains How many chains for the MCMC.
#' @param ... Additional arguments for \link[rjags2]{jags.model}
#'
#' @return
#' @export
#'
#' @examples
jags_model_lm <- function(jags_data, num_chains, ...) {

  rjags::jags.model(file = system.file("reg-linear.txt", package = "mickjaggr"),
                    data = jags_data,
                    n.chains = num_chains,
                    ...)

}

#' Run JAGS models
#'
#' @param .jags_model The output of \code{\link{jags_model_define}}
#' @param num_iter Number of iterations per chain.
#' @param ... Additional arguments for \link[rjags2]{coda.samples}
#'
#' @return
#' @export
#'
#' @examples
jags_model_run <- function(.jags_model, num_iter, ...) {
  rjags::coda.samples(model = .jags_model,
                      variable.names = c("beta", "sigma.sq"),
                      n.iter = num_iter,
                      ...)
}

