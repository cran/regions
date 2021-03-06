#' Imputing Data From Larger To Smaller Units in the EU NUTS
#'
#' This is a special case of \code{\link{impute_down}} for the EU NUTS
#' hierarchical typologies. All valid actual rows will be projected down
#' to all smaller constituent typologies where data is missing.
#'
#' The more general function requires typology information from the higher
#' and lower level typologies.  This is not needed when the EU vocabulary
#' is used, and the hierarchy can be established from the EU vocabularies.
#'
#' Be mindful that while all possible imputations are made, imputations
#' beyond one hierarchical level will result in very crude estimates.
#'
#' The imputed dataset \code{dat} must refer to a single time unit, i.e.
#' panel data is not supported.
#' @param dat A data frame with exactly two or three columns: \code{geo}
#' for the geo codes of the units, \code{values} for the values, and
#' optionally \code{method} for describing the data source.
#' @param values_var The variable that contains the upstream data to be
#' imputed to the downstream data, defaults to \code{"values"}.
#' @param geo_var The variable that contains the geographical codes in the
#' NUTS typologies, defaults to code{"geo_var".}
#' @param method_var The variable that contains the metadata on various
#' processing information, defaults to \code{NULL} in which case it will
#' be returned as \code{'method'}.
#' @param nuts_year The year of the NUTS typology to use, it defaults to the
#' currently valid \code{2016}.  Alternative values can be any of these:
#' \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013} and the already
#'  announced and defined \code{2021}. For example, use \code{2013} for
#' \code{NUTS2013} data.
#' @return An augmented version of the \code{dat} imputed data frame with all
#' possible projections to valid smaller units, i.e. \code{NUTS0 = country} values
#' imputed to all missing \code{NUTS1} units, \code{NUTS1} values
#' imputed to all missing \code{NUTS2} units, \code{NUTS2} values
#' imputed to all missing \code{NUTS3} units.
#' @family impute functions
#' @importFrom dplyr mutate select distinct distinct_at filter bind_rows
#' @importFrom dplyr rename arrange full_join
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom utils data
#' @examples
#' \donttest{
#' data(mixed_nuts_example)
#' impute_down_nuts(mixed_nuts_example, nuts_year = 2016)
#' }
#' @export

impute_down_nuts <- function (dat,
                              geo_var = "geo",
                              values_var = "values",
                              method_var = NULL,
                              nuts_year = 2016) {
  ## non-standard evaluation initialization
  . <- NULL
  all_valid_nuts_codes <- NULL
  
  validation <- paste0("valid_", nuts_year)
  
  if (is.null(method_var)) {
    dat$method <- ""
    method_var <- 'method'
  }
  
  validate_data_frame(dat, 
                      values_var = values_var,
                      geo_var = geo_var,
                      method_var = method_var,
                      nuts_year = nuts_year )
  
  if ("typology" %in% names(dat)) {
    message ("The 'typology' column is refreshed.")
    dat <- dat %>% select (-all_of("typology"))
  }
  
  if (validation %in% names(dat)) {
    message("The '", validation , "' column is refreshed.")
    dat <- dat %>% select (-all_of(validation))
  }
  
  get_valid_nuts_codes <- function(this_env) {
    data("all_valid_nuts_codes",
         package = "regions",
         envir = this_env)
    all_valid_nuts_codes
  }
  
  validated <- dat %>%
    rename (## will be turned back on return, easier to handle
      ## non-programatic geo, values names.
      geo = !!geo_var,
      values = !!values_var) %>%
    validate_nuts_regions(nuts_year = nuts_year)
  
  names(validated)[which(names(validated) == paste0("valid_", nuts_year))] <-
    "valid"
  
  ## subset for selecting only valid NUTS1 geo codes for imputing down
  validated_nuts_1 <- validated %>%
    filter (.data$typology == "nuts_level_1",
            .data$valid    == TRUE)
  
  ## subset for selecting only valid NUTS2 geo codes for imputing down
  validated_nuts_2 <- validated %>%
    filter (.data$typology == "nuts_level_2",
            .data$valid    == TRUE)
  
  countries_present <- validated %>%
    mutate (country_code = get_country_code(.data$geo)) %>%
    select (.data$country_code) %>%
    distinct (.data$country_code) %>%
    unlist() %>% as.character() %>% sort()
  
  nuts_filter <- paste0("code_", nuts_year)
  
  all_valid_nuts_codes_year <-
    get_valid_nuts_codes(this_env = environment())
  
  all_valid_nuts_codes_year <- all_valid_nuts_codes_year %>%
    filter (.data$nuts     == nuts_filter,
            .data$typology == "nuts_level_3") %>%
    mutate (## start with smallest hierarchical unit, which is NUTS3
      country_code = get_country_code (.data$geo),
      row_id       = seq_len(nrow(.)))  %>%
    pivot_wider (names_from = "typology",
                 values_from = 'geo') %>%
    mutate (
      nuts_level_2 = substr(.data$nuts_level_3, 1, 4),
      nuts_level_1 = substr(.data$nuts_level_3, 1, 3),
      country      = substr(.data$nuts_level_3, 1, 2),
    ) %>%
    select (-all_of("row_id"))
  
  full_code_table <-
    all_valid_nuts_codes_year %>%   ## all the regions of the countries present in the datatable
    filter (.data$country_code %in% countries_present)
  
  ## Create all possible imputations from NUTS0 >> NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_country <- full_code_table %>%
    filter (.data$country_code %in% countries_present) %>%
    dplyr::rename (geo  = .data$country_code) %>%
    left_join (dat, by = 'geo') %>%
    filter (!is.na(.data$values)) %>%
    mutate (method = paste0("imputed from country ",
                            .data$geo, " [", .data$method, "]"))
  
  ## Create all possible imputations from NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_nuts_1 <- full_code_table %>%
    filter (.data$nuts_level_1 %in% validated_nuts_1$geo) %>%
    rename (geo = .data$nuts_level_1) %>%
    left_join (dat, by = 'geo') %>%
    mutate (method = paste0("imputed from NUTS1 ",
                            .data$geo, " [", .data$method, "]")) %>%
    select (all_of(c("nuts_level_2", "values", "method"))) %>%
    dplyr::rename (geo = .data$nuts_level_2)
  
  ## Now add potential imputations from NUTS0 if not present in NUTS1
  imputed_from_nuts_1 <- potentially_imputed_from_country %>%
    distinct_at  (all_of (c("nuts_level_2", "values", "method")))  %>%
    rename (geo = .data$nuts_level_2) %>%
    filter (!.data$geo %in% potentially_imputed_from_nuts_1$geo) %>%
    bind_rows (potentially_imputed_from_nuts_1)
  
  ## Create all possible imputations from NUTS2 >> NUTS3
  potentially_imputed_from_nuts_2 <- full_code_table %>%
    filter (.data$nuts_level_2 %in% validated_nuts_2$geo) %>%
    rename (geo = .data$nuts_level_2) %>%
    select ( -all_of(c("nuts_level_1")) ) %>%
    left_join (dat, by = 'geo') %>%
    mutate (method = paste0("imputed from NUTS2 ",
                            .data$geo, " [", .data$method, "]")) %>%
    select (all_of(c("nuts_level_3", "values", "method"))) %>%
    rename (geo = .data$nuts_level_3) %>%
    filter (!.data$geo %in% validated$geo)
  
  ## Now add possible imputations from country level NUTS0 >> NUTS3
  imputed_from_nuts_2 <- potentially_imputed_from_country %>%
    distinct_at  (all_of (c("nuts_level_3", "values", "method")))  %>%
    rename (geo = .data$nuts_level_3) %>%
    filter (!.data$geo %in% potentially_imputed_from_nuts_2$geo) %>%
    bind_rows (potentially_imputed_from_nuts_2)
  
  ## Now add the original data and the the NUTS2 >> NUTS3 imputations
  actual_to_imputed <- validated %>%
    full_join (imputed_from_nuts_2,
               by = c("geo", "values", "method"))
  
  ## Now add whatever can be added from NUTS0 or NUTS1
  imputed_df <- imputed_from_nuts_1 %>%
    filter (!.data$geo %in% actual_to_imputed$geo) %>%
    distinct_at (all_of(c("geo", "values", "method"))) %>%
    bind_rows (actual_to_imputed) %>%
    distinct_at (all_of(c("geo", "values", "method")))
  
  ## And at last, add NUTS0 >> NUTS1 imputations
  imputed_df_2 <- potentially_imputed_from_country %>%
    distinct_at (all_of(c("nuts_level_1", "values", "method"))) %>%
    dplyr::rename (geo = .data$nuts_level_1) %>%
    bind_rows (imputed_df)
  
  imputed_dfv <- validate_nuts_regions(imputed_df_2,
                                       nuts_year = nuts_year) %>%
    dplyr::arrange(.data$geo) %>%
    mutate (## remove case when method was originally "" and we have [] in the end
      method = gsub("\\s\\[\\]", "", .data$method))
  
  names(imputed_dfv)[which (names(imputed_dfv) == "geo")] <-
    geo_var
  names(imputed_dfv)[which (names(imputed_dfv) == "values_var")] <-
    values_var
  
  ## Now only valids are returned.  Maybe all should be.
  imputed_dfv
}
