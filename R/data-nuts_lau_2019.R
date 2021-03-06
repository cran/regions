#' European Union: NUTS And LAU Correspondence
#'
#' A dataset containing the joined correspondence tables of the
#' EU NUTS and local administration units (LAU) typologies.
#'
#' This is also the authoritative vocabulary for local administration,
#' names, including city and metropolitan area names.
#'
#' @format A data frame with 99140 rows and 22 variables:
#' \describe{
#'   \item{code_2016}{NUTS3 code of the local administrative unit, 2016 definition}
#'   \item{lau_code}{Local Administrative Unit code}
#'   \item{lau_name_national}{LAU name, official in national language(s)}
#'   \item{lau_name_latin}{LAU name, official Latin alphabet version}
#'   \item{name_change_last_year}{Change in name in the year before?}
#'   \item{population}{Population}
#'   \item{total_area_m2}{Area in square meters}
#'   \item{degurba}{Degree of urbanization}
#'   \item{degurba_change_last_year}{Change in degree of urbanization?}
#'   \item{coastal_area}{Part of coastal area classification?}
#'   \item{coastal_change_last_year}{Change in coastal area classification}
#'   \item{city_id}{NUTS territorial name in the 2006 definition}
#'   \item{city_id_change_last_year}{NUTS territorial name in the 2010 definition}
#'   \item{city_name}{Name of the city}
#'   \item{greater_city_id}{Containing metro area ID, if applicable}
#'   \item{greater_city_id_change_last_year}{Change in metro area ID}
#'   \item{greater_city_name}{Name of containing greater city (metropolitan) area, if applicable}
#'   \item{fua_id}{FUA ID}
#'   \item{fua_id_change_last_year}{Change of FUA ID since last year}
#'   \item{fua_name}{Name in FUA database}
#'   \item{country}{NUTS country code with exceptions: EL for Greece, UK for United Kingdom}
#'   \item{gisco_id}{GISCO ID}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/local-administrative-units}
#' @seealso nuts_recoded, all_valid_nuts_codes
"nuts_lau_2019"
