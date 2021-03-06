#' European Union: All Valid NUTS Codes
#'
#' A dataset containing all recognised geo codes in the EU
#' NUTS correspondence tables. This is re-arranged from
#' \code{\link{nuts_changes}}.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{geo}{NUTS geo identifier}
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3}
#'   \item{nuts}{The NUTS definition where the geo code can be found.}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
#' @seealso nuts_recoded, nuts_changes, nuts_exceptions
"all_valid_nuts_codes"
