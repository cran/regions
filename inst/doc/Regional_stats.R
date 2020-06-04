## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(regions)
library(dplyr)

## ----fr-----------------------------------------------------------------------
data(nuts_changes)
overseas <- nuts_changes[ 
  which(nuts_changes$geo_name_2016 %in% c("La Réunion", "Guyane")),
  ]

knitr::kable(
  overseas[, c("code_2010", "code_2013", "code_2016",
               "geo_name_2010", "geo_name_2013", "geo_name_2016")]
  )

## -----------------------------------------------------------------------------
data("mixed_nuts_example")
mixed_nuts_example %>%
  filter ( substr(geo,1,2) == "MT") %>%
  knitr::kable ()

## -----------------------------------------------------------------------------
impute_down_nuts (mixed_nuts_example) %>%
  filter ( substr(geo,1,2) == "MT") %>%
  knitr::kable ()

## ----southern-----------------------------------------------------------------
data(nuts_changes)
southern <- nuts_changes[ 
  which(nuts_changes$geo_name_2016 == "Southern"), ]
knitr::kable(
  southern[, c("code_2013", "code_2016", "geo_name_2016", "change_2016")]
  )

