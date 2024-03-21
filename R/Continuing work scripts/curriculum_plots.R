library(here)
library(tidyverse)
library(gmRi)
install.packages("matrixStats")
library(matrixStats)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)

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
  filter(comname %in% species$comname & survey_area %in% c("GoM", "GB", "SNE") & est_year >= 1980) %>% 
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
                        "butterfish", "smooth dogfish", "yellowtail flounder")) %>%
  mutate(comname = stringr::str_to_title(comname)) %>%
  group_by(comname) %>%
  nest() %>%
  mutate(plot = map2(data, comname, function(x,y){
    out <- ggplot(x) +
      geom_line(aes(x = est_year, y = avg_biomass), color = "#535353", alpha = 0.6) +
      geom_smooth(aes(x = est_year, y = avg_biomass), color = "#00608A", linetype = 2, method = "lm", se = F) +
      # facet_wrap(~comname, scales = "free_x", ncol = 2) +
      ylim(c(0, 35)) +
      xlab("Year") + ylab("Average Biomass (kg/sq. km)") + 
      ggtitle(paste("Average Biomass of", comname, sep = " "), subtitle = "Gulf of Maine, Georges Bank, and Southern New England Survey Areas") +
      theme_gmri(strip.background = element_rect(fill = "transparent"),
                 strip.text = element_text(color = "black"),
                 plot.subtitle = element_text(size = 9, face = "bold"),
                 panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray"),
                 axis.title = element_text(size = 10, face = "bold"))
    
    filename = paste(comname, "biomass.png", sep = "_")
    ggsave(file = paste0(here("Temp_Results/Lesson 4/"), filename), out, width = 9, height = 6.5, units = "in", bg = "white")
  })) 

  
plots$plot[[3]]
# ggsave(file = paste0(here("Temp_Results", "Plots/"), "lesson_4.png"), width = 6.5, height = 9, units = "in", bg = "white")

# Maps
world <- ne_states(returnclass = "sf")
cols <- c("#7D7C7C", "#22444B", "#0E6686", "#028ABD", "#02D2FF")

maps <- plots %>% 
  unnest(data) %>% 
  select(comname, est_year, avg_lat, avg_lon) %>% 
  mutate(decade = 10*est_year %/% 10) %>%
  group_by(comname) %>%
  nest() %>% 
  mutate(map = map2(data, comname, function(x,y){
    out <- ggplot(data = world)+
      geom_sf()+ 
      coord_sf(xlim=c(-74, -66), ylim=c(38, 46)) +
      geom_point(data = x, aes(x = avg_lon, y = avg_lat, color = as.factor(decade)), size = 2) +
      scale_color_manual(values = cols)+
      xlab("Longitude") + ylab("Latitude") + ggtitle(comname) +
      facet_wrap(~decade, nrow =1) + 
      scale_y_continuous(breaks = c(38,46)) + scale_x_continuous(breaks = c(72,-70)) +
      theme_gmri(legend.position = "none", 
             legend.title = element_text(face = "bold", size = 10),
             legend.text = element_text(size = 10), 
             strip.background = element_rect(fill = "#00608A", color = "#00608A"), 
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) +
      guides(color = guide_legend(title = "Decade", override.aes = list(size = 3)))
    
    filename = paste(comname, "map.png", sep = "_")
    ggsave(file = paste0(here("Temp_Results/Lesson 4/"), filename), out, width = 9, height = 3.5, units = "in", bg = "white")
  }))

maps$map[[4]]
