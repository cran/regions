% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{validate_parameters}
\alias{validate_parameters}
\title{Assertion for Correct Function Calls}
\usage{
validate_parameters(typology = NULL, param = NULL, param_name = NULL)
}
\arguments{
\item{typology}{Currently the following typologies are supported:
\code{"NUTS1"}, \code{"NUTS2"}, \code{"NUTS3"} or \code{"NUTS"} for any of
the NUTS typologies. The technical typology \code{"NUTS0"}
can be used to translate Eurostat country codes to ISO 3166-1 alpha-2
country codes.}

\item{param}{A parameter value that must not be \code{NULL}.}

\item{param_name}{The name of the parameter that must not have a value of \code{NULL}.}
}
\value{
A boolean, logical variable if the parameter calls are valid.
}
\description{
Assertions are made to give early and precise error messages for wrong
API call parameters.
}
\details{
These assertions are called from various wrapper functions.  However, you can also call this
function directly to make sure that you are adding (programmatically) the correct
parameters to a call.

All \code{\link{validate_parameters}} parameters default to \code{NULL}.
Asserts the correct parameter values for any values that are not \code{NULL}.
}
