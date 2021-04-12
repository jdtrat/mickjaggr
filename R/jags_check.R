
#' Get residuals
#'
#' @param .recipe A recipe object
#' @param .model The output of \code{\link{jags_model_run}}
#'
#' @return A data frame containing two columns `.pred`, which are the estimated
#'   predictor values and `.resid`, the difference between the true data and the
#'   predicted data.
#' @export
#'
#' @examples
#' # coming soon
jags_check_residuals <- function(.recipe, .model) {

  # Create an index column for all output coefficients that have beta parameters
  # colnames should be same for all chains, so we're just using first chain.
  coef_cols_index <- grepl("beta", colnames(.model[[1]]))

  # creaate a matrix with the means of each beta coef from each chain
  beta_chains <- vapply(.model[,coef_cols_index], colMeans, FUN.VALUE = numeric(sum(coef_cols_index)))
  # average across all chains (rows) for each beta coefficient
  betas <- apply(beta_chains, 1, mean)

  predictors <- jags_data_predictors(.recipe = .recipe)
  response <- jags_data_response(.recipe = .recipe)

  y_hat <- predictors %*% betas

  residual_df <- data.frame(.preds = y_hat,
                            .resid = response - y_hat)

  return(residual_df)
}
