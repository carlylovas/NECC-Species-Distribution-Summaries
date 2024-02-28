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
  filter(comname %in% species$comname & survey_area %in% c("GoM", "GB", "SNE")) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")

## change goosefish to monkfish
GoM_survey %>% filter(comname == "goosefish") -> monkfish
monkfish$comname <- "monkfish"

GoM_survey <- GoM_survey %>%
  filter(!comname == "goosefish") %>%
  rbind(monkfish) %>%
  arrange(comname)

# Weight by biomass
grouped_center_bio <- function(GoM_survey, ...){
  GoM_survey %>% 
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
  filter(comname %in% c("monkfish", "silver hake", "atlantic mackerel", "winter flounder", "black sea bass",
                        "butterfish", "scup", "yellowtail flounder")) %>%
  mutate(comname = stringr::str_to_sentence(comname))

cc_plot <- ggplot(plots) +
  geom_line(aes(x = est_year, y = avg_biomass), color = "#535353", alpha = 0.6) +
  geom_smooth(aes(x = est_year, y = avg_biomass), color = "#00608A", linetype = 2, method = "lm", se = F) +
  facet_wrap(~comname, scales = "free_y", ncol = 2) +
  xlab("Year") + ylab("Average Biomass (kg/sq. km)") + 
  ggtitle("Average Biomass", subtitle = "Gulf of Maine, Georges Bank, and Southern New England Survey Areas") +
  theme_gmri(strip.background = element_rect(fill = "transparent"),
             strip.text = element_text(color = "black"),
             plot.subtitle = element_text(size = 9, face = "bold"),
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray"),
             axis.title = element_text(size = 10, face = "bold"))
print(cc_plot)
ggsave(file = paste0(here("Temp_Results", "Plots/"), "lesson_4.png"), width = 6.5, height = 9, units = "in", bg = "white")



# Monkfish

plots %>%
  filter(comname == "Monkfish") -> monkfish

summary(lm(est_year ~ avg_biomass, data = monkfish))

