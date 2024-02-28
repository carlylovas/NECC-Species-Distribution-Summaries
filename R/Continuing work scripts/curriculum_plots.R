library(here)
library(tidyverse)
library(gmRi)
install.packages("matrixStats")
library(matrixStats)

# species data
species <- read_csv(here("data", "speciesList_inNECC.csv")) %>%
  rename("comname" = "Species_comnam") %>%
  mutate(comname   = tolower(comname)) %>%
  select(comname)

# Load NEFSC Bottom Trawl Survey data ####
trawl_data <- gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage")

GoM_survey  <- trawl_data %>% 
  distinct(svspp, est_year, survey_area, stratum, tow, id, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(svspp, est_year, survey_area, stratum, tow, id, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname, abundance) %>% 
  filter(comname %in% species$comname & survey_area == "GoM") %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")

# Weight by biomass
grouped_center_bio <- function(GoM_survey, ...){
  clean_survey %>% 
    group_by(comname, ...) %>% 
    summarise(
      # Un-weighted averages
      total_biomass   = sum(biomass_kg),
      avg_biomass     = mean(biomass_kg),
      biomass_sd      = sd(biomass_kg),
      # All below are weighted by biomass
      avg_depth       = weightedMean(avgdepth, w = biomass_kg, na.rm = T),
      avg_bot_temp    = weightedMean(bottemp, w = biomass_kg, na.rm = T),
      avg_sur_temp    = weightedMean(surftemp, w = biomass_kg, na.rm = T),
      avg_lat         = weightedMean(decdeg_beglat, w = biomass_kg, na.rm = T),
      avg_lon         = weightedMean(decdeg_beglon, w = biomass_kg, na.rm = T),
      depth_sd        = weightedSd(avgdepth, w = biomass_kg, na.rm = T),
      temp_sd         = weightedSd(bottemp, w = biomass_kg, na.rm = T),
      lat_sd          = weightedSd(decdeg_beglat, w = biomass_kg, na.rm = T),
      lon_sd          = weightedSd(decdeg_beglon, w = biomass_kg, na.rm = T),
      .groups = "drop") 
}

weighted_survey_data <- grouped_center_bio(GoM_survey, est_year)


# Monkfish, Silver Hake, Atlantic Mackerel, Winter Flounder, Black Sea Bass, Butterfish, Scup, Yellowtail flounder

plots <- weighted_survey_data %>%
  filter(comname %in% c("goosefish", "silver hake", "atlantic mackerel", "winter flounder", "black sea bass",
                        "butterfish", "scup", "yellowtail flounder")) %>%
  mutate(comname = stringr::str_to_sentence(comname))

# rolling_means <- GoM_survey %>%
#   filter(comname %in% c("goosefish", "silver hake", "atlantic mackerel", "winter flounder", "black sea bass",
#                         "butterfish", "scup", "yellowtail flounder")) %>%
#   mutate(comname = stringr::str_to_sentence(comname)) %>%
#   group_by(comname, est_year) %>%
#   summarise(rmean = zoo::rollapplyr(biomass_kg, width = 5, FUN = mean, align = "center", partial = T),
#             .groups = "drop")

ggplot(plots) +
  geom_line(aes(x = est_year, y = avg_biomass)) +
  facet_wrap(~comname, scales = "free_y") +
  theme_gmri(strip.background = element_rect(fill = "transparent"),
             strip.text = element_text(color = "black"))
