# Calculating range edges and distance from coast ####
library(here) 
library(tidyverse)
library(gmRi)

## Read in survey data ####
clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)

clean_survey <- clean_survey %>% 
  distinct(est_year, survey_area, stratum, tow, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(est_year, survey_area, stratum, tow, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname, abundance) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")

str(clean_survey)

survey_tows <- get_survdat_tows(clean_survey)
summary(survey_tows)

# Example ####
## Calculate quantiles
cod_example <- clean_survey %>%
  filter(comname == "atlantic cod")

cod_example %>% 
  select(comname, est_year, decdeg_beglat, decdeg_beglon, season, biomass_kg) %>%
  rename("lat" = "decdeg_beglat", 
         "lon" = "decdeg_beglon") %>%
  group_by(est_year, season) %>%
  mutate(north_edge = quantile(lat, 0.9),
         south_edge = quantile(lat, 0.1),
         minLat     = min(lat),
         maxLat     = max(lat)) -> cod_example
