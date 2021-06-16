## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warnings=FALSE, echo=FALSE-------------------------
library(regions)
library(dplyr)
library(tidyr)
data(daily_internet_users)

## ----example, eval=FALSE------------------------------------------------------
#  isoc_r_iuse_i <- eurostat::get_eurostat("isoc_r_iuse_i",
#                                          time_format = "num")
#  
#  daily_internet_users <- isoc_r_iuse_i  %>%
#    dplyr::filter ( unit == "PC_IND",          # percentage of individuals
#                    indic_is == "I_IDAY") %>%  # daily internet users
#    select (geo, time, values )

## ----testvalidate-------------------------------------------------------------
test <- daily_internet_users  %>% 
  mutate ( country_code = get_country_code(geo = .data$geo) ) %>%
  dplyr::filter ( time %in% c(2012, 2018),
                  country_code %in% c("FR", "HU", "LT")) %>%
  mutate ( time = paste0("Y", time )) %>%
  pivot_wider (., names_from ="time", values_from = "values") %>%
  validate_nuts_regions() %>%  # default year the current valid 2016
  validate_nuts_regions(.,  nuts_year = 2010 )

## ----new----------------------------------------------------------------------
# only first 10 regions are printed
knitr::kable( 
  head(test [ ! test$valid_2010, ], 10)
)

## ----old----------------------------------------------------------------------
knitr::kable(
  test [ ! test$valid_2016, ]
)

