#' Google Mobility Report European Correspondence Table
#'
#' A dataset containing the correspondence table between the EU
#' NUTS 2016 typology and the typology used by Google in the
#' Google Mobility Reports.
#'
#' In some cases only a full correspondence is not possible. In these
#' cases we created pseudo-NUTS codes, which have a \code{FALSE}
#' \code{valid_2016} value. These pseudo-NUTS codes can help
#' approximation for the underlying regions.
#'
#' Pseudo-NUTS codes were used in Estonia, Italy, Portugal, Slovenia
#' and in parts of Latvia.
#'
#' In Latvia and Slovenia, the pseudo NUTS code is a combination of the
#' the containing NUTS3 code and the municipality's LAU code.
#'
#' In Estonia, they are a combination of the NUTS3 code and the
#' \code{ISO-3166-2} LAU code (county level.) This is the case in most of
#' Portugal and the United Kingdom, too. In these cases the pseudo-codes refer to a
#' quasi-NUTS4 code, which are smaller than the containing NUTS3 region,
#' therefore they should be aggregated.
#'
#' A special case is \code{ITD_IT-32}, which is is a combination
#' of two NUTS2 statistical regions, but it forms under the \code{ISO-3166-2}
#' \code{ITD_IT-32} a single unit, the autonomous region of
#' Trentino and South Tyrol. In this case, they should be disaggregated.
#'
#' A similar solution is required for the United Kingdom.
#'
#' @format A data frame with 817 rows and 6 variables:
#' \describe{
#'   \item{country_code}{ISO 3166-1 alpha2 code}
#'   \item{google_region_level}{Hierarchical level in the Google Mobility Reports}
#'   \item{google_region_name}{The name used by Google.}
#'   \item{code_2016}{NUTS code in the 2016 definition}
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3, nuts_level_3_lau, nuts_level_3_iso-3166-2}
#'   \item{valid_2016}{Logical variable, if the coding is valid in NUTS2016}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
#' @author Istvan Zsoldos, Daniel Antal
"google_nuts_matchtable"
