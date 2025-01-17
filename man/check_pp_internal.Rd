% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jags_check.R
\name{check_pp_internal}
\alias{check_pp_internal}
\title{Calculate Bayesian P-Values for each check}
\usage{
check_pp_internal(.list, .check, .response_check)
}
\arguments{
\item{.list}{The output of \code{\link{jags_model_run}}}

\item{.check}{A character vector of summary stats to perform}

\item{.response_check}{The actual statistics from the response variable}
}
\value{
A matrix with the Bayesian P value for each summary stat for every iteration
}
\description{
Calculate Bayesian P-Values for each check
}
\keyword{internal}
