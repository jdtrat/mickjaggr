#' Get the matrix of predictors
#'
#' @param .recipe A recipe object
#'
#' @return A matrix of predictors for the model
#' @export
#'
jags_data_predictors <- function(.recipe) {

  data <- jags_data_all(.recipe = .recipe)
  outcome_var <- subset(summary(.recipe), role == "outcome")[["variable"]]

  # set the predictors
  predictors <- subset(data, select = !names(data) %in% outcome_var)
  # add a column of ones
  predictors <- transform(predictors, ones = 1)
  # move the column of ones to the front
  predictors <- predictors[,c("ones", setdiff(names(predictors), "ones"))]
  # convert to a predictors matrix
  predictors <- as.matrix(predictors)

  return(predictors)
}


#' Get numbers of predictors
#'
#' @param .pred_matrix The object returned by \code{\link{jags_data_predictors}}.
#'
#' @return
#' @export
#'
#' @examples
#' # coming soon
jags_data_num_preds <- function(.pred_matrix) {
  ncol(.pred_matrix)
}


#' Get numbers of observations
#'
#' @param .recipe A recipe object.
#'
#' @return The number of observations
#' @export
#'
#' @examples
#' # coming soon
jags_data_num_obs <- function(.recipe) {
  nrow(.recipe$template)
}


#' Pull the modeling data
#'
#' @param .recipe A recipe object.
#'
#' @return
#' @export
#'
#' @examples
#' # coming soon
jags_data_all <- function(.recipe) {
  all_data <- .recipe$template
  return(all_data)
}

#' Get the response vector
#'
#' @param .recipe A recipe object
#'
#' @return
#' @export
#'
#' @examples
#' # coming soon
jags_data_response <- function(.recipe) {

  # get the column name for the outcome variable
  outcome_var <- subset(summary(.recipe), role == "outcome")[["variable"]]

  # get the response vector
  .recipe[["template"]][[outcome_var]]

}

#' Prep Matrices
#'
#' @param .recipe A recipe object
#' @param family The type of model to perform (e.g. "lm").
#'
#' @return A list to be passed into `jags.model`
#' @export
#'
#' @examples
#' # coming soon
jags_data_prep <- function(.recipe, family) {

  # Get the prepped data
  data <- jags_data_all(.recipe = .recipe)

  response <- jags_data_response(.recipe)

  # get the column name for the outcome variable
  outcome_var <- subset(summary(.recipe), role == "outcome")[["variable"]]

  predictors <- jags_data_predictors(.recipe = .recipe)

  num_preds <- jags_data_num_preds(.pred_matrix = predictors)

  num_obs <- jags_data_num_obs(.recipe = .recipe)

  output <- list("response" = response,
                 "predictors" = predictors,
                 "num_preds" = num_preds,
                 "num_obs" = num_obs)

  class(output) <- c(family, "list")

  return(output)

}
