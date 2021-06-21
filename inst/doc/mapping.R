## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
here::here()

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
library(regions)
library(eurostat)
library(dplyr, quietly = T)

## ----rd-workforce-get, eval=FALSE, message=FALSE------------------------------
#  regional_rd_personnel <- eurostat::get_eurostat_json (
#    id = "rd_p_persreg",
#    filters = list (
#      sex = "T",
#      prof_pos = "TOTAL",
#      sectperf = "TOTAL",
#      unit = "FTE" )
#  )
#  
#  regional_rd_personnel <- regional_rd_personnel %>%
#    filter ( .data$time %in% c("2009", "2018") )

## ----rd-presaved--------------------------------------------------------------
data("regional_rd_personnel")

## ---- include=FALSE-----------------------------------------------------------
## make sure that regional_rd_personnel does not remain a promise
regional_rd_personnel <- regional_rd_personnel

## ----missing-cases------------------------------------------------------------
summary(is.na(regional_rd_personnel$values))

## ---- echo=FALSE--------------------------------------------------------------
library(ggplot2)

## ----get-map, eval=FALSE------------------------------------------------------
#  map_nuts_2 <- eurostat::get_eurostat_geospatial(
#    resolution = "60",
#    nuts_level = "2",
#    year = 2016)

## ---- eval=FALSE--------------------------------------------------------------
#  indicator_with_map <- map_nuts_2 %>%
#    left_join ( regional_rd_personnel, by = "geo" )

## ---- eval=FALSE--------------------------------------------------------------
#  indicator_with_map  %>%
#    ggplot () +
#    geom_sf(aes(fill=values),
#            color="dim grey", size=.1) +
#    scale_fill_gradient( low ="#FAE000", high = "#00843A") +
#    facet_wrap ( facets = "time") +
#    labs(title="R&D Personnel & Researchers",
#         subtitle = "In all sectors, both sexes by NUTS 2 regions",
#         caption="\ua9 EuroGeographics for the administrative boundaries
#                  \ua9 Tutorial and ready-to-use data on economy.dataobservatory.eu",
#         fill = NULL) +
#    theme_light() +
#    theme(legend.position="none") +
#    coord_sf(xlim=c(-22,48), ylim=c(34,70))

## ----original-map, echo=FALSE, out.width='80%', fig.align='center'------------
knitr::include_graphics(
  here::here("vignettes", "indicator_with_map.png")
)

## -----------------------------------------------------------------------------
validated_indicator <- regions::validate_nuts_regions(regional_rd_personnel)

## ----validation-summary, message=FALSE----------------------------------------
library(dplyr)
validation_summary_2016 <- validated_indicator %>%
  group_by ( .data$time, .data$typology) %>%
  summarize ( observations = n(),
              values_missing = sum(is.na(.data$values)), 
              values_present = sum(!is.na(.data$values)), 
              valid_present = values_present /observations)

## -----------------------------------------------------------------------------
validation_summary_2016  %>%
  ungroup() %>%
  filter (.data$time == "2009")

## -----------------------------------------------------------------------------
validation_summary_2016  %>%
  ungroup() %>%
  filter (.data$time == "2018")

## ----non-conforming-geo-------------------------------------------------------
 validated_indicator %>%
  filter ( ! .data$valid_2016 ) %>%
  select ( all_of("geo") ) %>% 
  unlist %>% as.character()

## ----recoding-----------------------------------------------------------------
recoded_indicator <- regional_rd_personnel %>% 
  regions::recode_nuts(
    geo_var = "geo", # your geograhical ID variable name
    nuts_year = 2016 # change this for other definitions
    )

## ----recode-summary, message=FALSE--------------------------------------------
recoding_summary <- recoded_indicator %>%
  mutate ( observations  = nrow(.data)) %>%
  mutate ( typology_change = ifelse ( grepl("Recoded", .data$typology_change), 
                                      yes = "Recoded", 
                                      no = .data$typology_change) ) %>%
  group_by ( .data$typology_change, .data$time ) %>%
  summarize ( values_missing = sum(is.na(.data$values)), 
              values_present = sum(!is.na(.data$values)), 
              pct = values_present / (values_present + values_missing ))

## -----------------------------------------------------------------------------
recoding_summary 

## ----geocode-changes----------------------------------------------------------
recoded_indicator %>%
  filter ( .data$typology == "nuts_level_2" ) %>%
  filter ( !is.na(.data$typology_change) )  %>%
  filter ( 
    # Keep only pairs where we actually save 
    # non-missing observations
    !is.na(values) ) %>%
  distinct ( .data$geo, .data$code_2016 ) %>%
  filter ( 
    # We filter out cases of countries who 
    # joined the NUTS system later
    .data$geo != .data$code_2016 )

## ----change-to-nuts2016, eval=FALSE-------------------------------------------
#  recoded_with_map <- map_nuts_2 %>%
#    left_join (
#      recoded_indicator %>%
#        mutate (geo = .data$code_2016),
#      by = "geo"  )

## -----------------------------------------------------------------------------
regional_rd_personnel_recoded <- recoded_indicator %>%
  mutate ( geo = .data$code_2016 ) %>%
  rename ( values_2016 = .data$values ) %>%
  select ( -all_of(c("typology", "typology_change", "code_2016"))) %>%
  full_join ( 
    regional_rd_personnel, 
    by = c("prof_pos", "sex", "sectperf", "unit", "geo", "time")
             ) %>%
  mutate ( type = case_when ( 
    is.na(.data$values_2016) & is.na(.data$values) ~ "missing",
    is.na(.data$values) ~ "after",
    TRUE ~ "before"
    ))

## ---- eval=FALSE--------------------------------------------------------------
#  map_nuts_2 %>%
#    left_join ( regional_rd_personnel_recoded , by = "geo") %>%
#    filter (
#      # remove completely missing cases
#      !is.na(.data$time) ) %>%
#    ggplot () +
#    geom_sf(aes(fill=type),
#            color="dim grey", size=.1) +
#    scale_fill_manual ( values = c( "#FAE000", "#007CBB", "grey70") ) +
#    guides(fill = guide_legend(reverse=T, title = NULL)) +
#    facet_wrap ( facets = "time") +
#    labs(title="R&D Personnel & Researchers",
#         subtitle = "In all sectors, both sexes by NUTS 2 regions",
#         caption="\ua9 EuroGeographics for the administrative boundaries
#                  \ua9 Daniel Antal, rOpenGov",
#         fill = NULL) +
#    theme_light() +
#    theme(legend.position=c(.93,.7)) +
#    coord_sf(xlim=c(-22,48), ylim=c(34,70))

## ----recoded-map, echo=FALSE, out.width='80%', fig.align='center'-------------
knitr::include_graphics(
  here::here("vignettes", "recoded_indicator_with_map.png")
)

## ----citation-eurostat, message=FALSE, eval=TRUE, echo=TRUE-------------------
citation("eurostat")

## ----citation-regions, message=FALSE, eval=TRUE, echo=TRUE--------------------
citation("regions")

## ----sessioninfo, message=FALSE, warning=FALSE--------------------------------
sessionInfo()

