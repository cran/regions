## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warnings=FALSE-------------------------------------
library(regions)
library(dplyr)
library(tidyr)

## ----changes, results='asis'--------------------------------------------------
data(nuts_changes)

nuts_changes %>%
  mutate ( geo_16 = code_2016, 
           geo_13 = code_2013 ) %>%
  filter ( code_2016 %in% c("FRB", "HU11") | 
             code_2013 %in% c("FR7", "HU10", "FR24")) %>%
  select ( typology, geo_16, geo_13, start_year,
           code_2013, change_2013,
           code_2016, change_2016 ) %>%
  pivot_longer ( cols = starts_with("code"), 
                 names_to = 'definition', 
                 values_to = 'code') %>%
  pivot_longer ( cols = starts_with("change"), 
                 names_to = 'change', 
                 values_to = 'description')  %>%
  filter (!is.na(description), 
          !is.na(code)) %>%
  select ( -change ) %>%
  knitr::kable ()

## ----recode, results='asis'---------------------------------------------------
example_df <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" ,
            "HU12", "DED", 
            "FRK"), 
  values = runif(6, 0, 100 ),
  stringsAsFactors = FALSE )

recode_nuts(dat = example_df, nuts_year = 2013) %>%
  select ( geo, values, code_2013) %>%
  knitr::kable()

## ----recode2013, results='asis'-----------------------------------------------
recode_nuts(example_df, nuts_year = 2013) %>%
  select ( geo, values, typology_change, code_2013 ) %>%
  knitr::kable()

## ----changes2013, results='asis'----------------------------------------------
recode_nuts(example_df, nuts_year = 2013) %>%
  select ( code_2013, values, typology_change ) %>%
  rename ( geo = code_2013 ) %>% 
  filter ( !is.na(geo)) %>%
  knitr::kable()

## ----recode2021, results='asis'-----------------------------------------------
recode_nuts(example_df, nuts_year = 2021) %>%
  select ( code_2021, values, typology_change ) %>%
  rename ( geo = code_2021 ) %>% 
  filter ( !is.na(geo)) %>%
  knitr::kable()

## ----recodeHU12, results='asis'-----------------------------------------------
data(nuts_changes) 
nuts_changes %>% 
  select ( code_2016, geo_name_2016, change_2016) %>%
  filter ( code_2016 == "HU12") %>%
  filter(complete.cases(.)) %>%
  knitr::kable()

