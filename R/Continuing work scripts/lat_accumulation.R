## Combining A.Fredston's range edges and L.Carlson's latitudinal accumulation code
## load libraries 
library(here)
library(tidyverse)
library(gmRi)
install.packages("matrixStats")
library(matrixStats)
install.packages("ggridges")
library(ggridges)
# library(plotly)

# species data
species <- read_csv(here("data", "speciesList_inNECC.csv")) %>%
  rename("comname" = "Species_comnam") %>%
  mutate(comname   = tolower(comname)) %>%
  select(comname)

# Load NEFSC Bottom Trawl Survey data ####
trawl_data <- gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage")

clean_survey  <- trawl_data %>% 
  distinct(svspp, est_year, survey_area, stratum, tow, id, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(svspp, est_year, survey_area, stratum, tow, id, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname, abundance) %>% 
  filter(comname %in% species$comname) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop") %>%
  mutate(decade = 10*est_year %/% 10)

# Sum total biomass by species in each year (L. Carlson)
sum_biomass_year <- trawl_data %>%
  group_by(est_year, svspp) %>% 
  summarise(annual_species_biomass = sum(biomass_kg))

# Total catch by unique haul (L. Carlson)
sum_biomass_haul <- trawl_data %>% 
  distinct(id, season, comname, catchsex, biomass_kg, abundance, .keep_all = T) %>%
  group_by(svspp, id) %>%
  summarise(species_biomass = sum(biomass_kg), species_abundance = sum(abundance))

# Weight by biomass
grouped_center_bio <- function(clean_survey, ...){
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

weighted_survey_data <- grouped_center_bio(clean_survey, est_year) # leaving out season 


weighted_survey_data %>%
  filter(comname == "acadian redfish") %>% 
  mutate(decade = 10*est_year %/% 10) %>%
  ggplot() +
  geom_density_ridges(aes(x = avg_lat, y = decade, fill = as.factor(decade)), alpha = .9) +
  guides(fill = guide_legend(title = "Decade")) +
  # scale_y_continuous(breaks = c("1970", "2010")) +
  # cale_y_reverse() +
  # ylim(c(2010, 1960)) +
  scale_fill_gmri() +
  theme_gmri(legend.position = "left") 

# Latitudinal distribution  
lat_dist <- weighted_survey_data %>% 
  mutate(decade = 10*est_year %/% 10) %>%
  group_by(comname) %>% 
  nest() %>% 
  mutate(plot = map2(data, comname, function(x,y){
    out <- ggplot(x) +
      geom_density_ridges(aes(x = avg_lat, y = decade, fill = as.factor(decade)), alpha = .9) +
      guides(fill = guide_legend(title = "Decade")) +
      ylab("Decade") + xlab("Biomass-weighted average latitude") + ggtitle(str_to_sentence(comname)) +
      ylim(c(1970, NA)) +
      scale_fill_gmri() +
      coord_flip() +
      theme_gmri()
    filename = paste(comname, "biomass_percentiles.png", sep="_")
    ggsave(out, filename = paste(here("Temp_Results", "Biomass percentiles"), filename, sep="/"), width = 7, height =  5, units="in", bg = "white")
    
  }))

lat_dist$plot[[9]] # I like this

# Center of Biomass 
centerBiomass <- weighted_survey_data %>% 
  select(comname, est_year, avg_lat, avg_lon, avg_biomass) %>% 
  group_by(comname) %>% 
  nest() %>% 
  mutate(lat_lm   = map(data, ~lm(avg_lat ~ est_year, data = .x)),
         lon_lm   = map(data, ~lm(avg_lon ~ est_year, data = .x)),
         tidy_lat = map(lat_lm, broom::tidy),
         tidy_lon = map(lon_lm, broom::tidy))

lat_centroid <- centerBiomass %>% 
  select(comname, lat_lm, tidy_lat) %>% 
  # mutate(slope_lat = tidy_lat %>% map_dbl(function(x) x$estimate[2]))
  unnest(tidy_lat) %>%
  filter(!term == "(Intercept)") %>% 
  select(comname, lat_lm, estimate, p.value) %>%
  rename("slope" = "estimate") %>%
  drop_na() %>%
  mutate(movement = ifelse(slope > 0, "T","F")) %>% 
  mutate(significant = ifelse(p.value < 0.05, "T","F")) %>% 
  arrange(desc(movement), desc(significant), comname)


lat_centroid$trend = NA
lat_centroid$trend[lat_centroid$movement == "T" & lat_centroid$significant == "T"] = "Northward" 
lat_centroid$trend[lat_centroid$movement == "T" & lat_centroid$significant == "F"] = "Stable"
lat_centroid$trend[lat_centroid$movement == "F" & lat_centroid$significant == "F"] = "Stable"
lat_centroid$trend[lat_centroid$movement == "F" & lat_centroid$significant == "T"] = "Southward"

lat_centroid %>% 
  filter(significant == "T") %>%
  mutate(comname = str_to_sentence(comname)) %>%
  select(!lat_lm) %>%
  gt(groupname_col = NULL) %>%
  cols_hide(c(slope, movement, significant)) %>%
  cols_label(
    comname = md("**Species**"),
    p.value = md("**P**"),
    trend   = md("**Trend**")) %>%
  fmt_number(columns = p.value, decimals = 2)

# Calculate and plot 5%, 10%, 25%, 75%, 90%, and 95% biomass-weighed percentiles
grouped_quantiles <- function(clean_survey, ...){
  clean_survey %>% 
    group_by(comname, ...) %>% 
    summarise(
      # Un-weighted averages
      total_biomass   = sum(biomass_kg),
      avg_biomass     = mean(biomass_kg),
      avg_lat         = mean(decdeg_beglat),
      # Weight quantiles
      `5%`  = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.05, na.rm = T),
      `10%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.10, na.rm = T), 
      `25%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.25, na.rm = T),
      `50%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.50, na.rm = T),
      `75%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.75, na.rm = T), 
      `90%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.90, na.rm = T),
      `95%` = Hmisc::wtd.quantile(decdeg_beglat, weights = biomass_kg, probs = 0.95, na.rm = T),
      .groups = "drop") %>%
    mutate(across(where(is.numeric), round, 4))
}

quantiles <- grouped_quantiles(clean_survey, est_year)

quantiles %>% 
  filter(comname == "black sea bass") %>% 
  mutate(decade = 10*est_year %/% 10) %>%
  ggplot() + 
  geom_col(aes(x = avg_biomass, y = as.factor(`5%`)), fill = "#00608A") +
  geom_col(aes(x = avg_biomass , y = as.factor(`25%`)), fill = "#EACA00") + 
  geom_col(aes(x = avg_biomass , y = as.factor(`50%`)), fill = "#535353") +
  geom_col(aes(x = avg_biomass, y = as.factor(`75%`)), fill = "#EACA00") +
  geom_col(aes(x = avg_biomass, y = as.factor(`95%`)), fill = "#00608A") +
  # ylim(c(30, 50)) +
  scale_y_discrete(breaks= c(30, 35, 40, 45)) +
  facet_wrap(~decade, scales = "free_y") +
  theme_gmri() # I don't like this, refer back to Lindsey's code


# Quantiles by decade?
## add to weighted lat shift? 
decadal_quantiles     <- grouped_quantiles(clean_survey, decade)
weighted_survey_data %>% 
  mutate(decade = 10*est_year %/% 10) %>%
  relocate(decade, .after = est_year) %>% 
  select(comname, est_year, decade, avg_lat) %>%
  full_join(decadal_quantiles %>% select(!c(avg_lat, total_biomass, avg_biomass))) -> decadal_quantiles

# bsb_2 <- 
decadal_quantiles %>% 
  filter(comname == "black sea bass") %>% # & decade == "1970") %>%
  ggplot() +
  geom_density(aes(y = avg_lat), fill = "#00608A", alpha = 0.9) +
  geom_segment(aes(y = `95%`, yend = (`95%`), x = 0, xend = 1.25), color = "#EA4F12", linewidth = 0.5, linetype = 2) +
  geom_segment(aes(y = `50%`, yend = (`50%`), x = 0, xend = 1.25), color = "#EACA00", linewidth = 0.5, linetype = 2) +
  geom_segment(aes(y = `5%`,  yend = (`5%`),  x = 0, xend = 1.25), color = "#407331", linewidth = 0.5, linetype = 2) +
  facet_wrap(~decade, nrow = 1) +
  ylab("Average Latitude") + xlab("Density") + ggtitle("Black Sea Bass") +
  theme_gmri(strip.background = element_blank(),
             strip.text = element_text(color = "black"))


# Latitudinal shift percentiles 
lat_shift <- quantiles %>% 
  pivot_longer(cols = 6:12, names_to = "quantile", values_to = "lat") %>%
  select(comname, est_year, quantile, lat) %>%
  group_by(comname, quantile) %>%
  mutate(rollmean = zoo::rollapplyr(lat, width = 5, FUN = mean, align = "center", partial = T)) %>%
  group_by(comname) %>% 
  nest() %>% 
  mutate(plot = map2(data, comname, function(x,y){
    out <- ggplot(x) +
      geom_line(aes(x = est_year, y = rollmean, color = quantile), linetype = 2) +
      geom_smooth(aes(x = est_year, y = rollmean, color = quantile), method = "lm", se = F) +
      ylab("Latitude") + xlab("Year") + ggtitle(str_to_sentence(comname)) + 
      scale_color_gmri() +
      theme_gmri()
    
    filename = paste(comname, "lat_shift_percentiles.png", sep="_")
    ggsave(out, filename = paste(here("Temp_Results", "Lat percentiles"), filename, sep="/"), width = 7, height =  5, units="in", bg = "white")
    }))

lat_shift$plot[[27]]


## difference in quantile slopes -> lean, retract, march, etc.
# can we use the difference in magnitude and direction of these slopes to characterize types of movement?
# quantiles %>%
#   select(est_year, comname, `5%`, `95%`) %>%
#   mutate(`5%`  = zoo::rollapplyr(`5%`,  width = 5, FUN = mean, align = "center", partial = T),
#          `95%` = zoo::rollapplyr(`95%`, width = 5, FUN = mean, align = "center", partial = T)) %>%
#   group_by(comname) %>%
#   nest() %>%
#   mutate(anova = map(data, function(x){
#     mod_05 <- aov(`5%`  ~ est_year,  data = x)
#     mod_95 <- aov(`95%` ~ est_year, data = x)
#     anova  <- anova(mod_05, mod_95)
#     return(anova)
#   })) -> diff_lat_perc  # this doesn't work 

# Interactions...?
quantiles %>%
  select(est_year, comname, `5%`, `95%`) %>%
  pivot_longer(cols = c(`5%`, `95%`), names_to = "percentiles", values_to = "latitude") %>%
  mutate(rmean_lat = zoo::rollapplyr(latitude,  width = 5, FUN = mean, align = "center", partial = T)) %>%
  mutate(percentiles = factor(percentiles)) %>%
  group_by(comname) %>%
  nest() %>%
  mutate(mod = map(data, function(x){
    mod <- lm(rmean_lat ~ est_year*percentiles, data = x) # this seems...wrong...
    tidy <- broom::tidy(mod)
    return(tidy)
  })) %>%
  unnest(mod) -> quantiles_lm 

quantiles_lm %>% 
  filter(p.value <= 0.05)

# by season ?
seasonal_distributions <- grouped_center_bio(clean_survey, est_year, season)
seasonal_quantiles     <- grouped_quantiles(clean_survey, est_year, season)

seasonal_distributions %>%
  mutate(decade = 10*est_year %/% 10) %>%
  group_by(comname) %>% 
  nest() %>% 
  mutate(plot = map2(data, comname, function(x,y){
    out <- ggplot(x) +
      geom_density_ridges(aes(x = avg_lat, y = decade, fill = as.factor(decade)), alpha = .9) +
      guides(fill = guide_legend(title = "Decade")) +
      facet_wrap(~season, ncol = 2) +
      ylab("Decade") + xlab("Biomass-weighted average latitude") + ggtitle(str_to_sentence(comname)) +
      ylim(c(1970, NA)) +
      scale_fill_gmri() +
      coord_flip() +
      theme_gmri()
  })) -> seasonal_distributions
seasonal_distributions$plot[29]


# difference in seasonal center of biomass ? rate of change? is spring center changing faster than fall? 
seasonal_quantiles %>%
  pivot_longer(cols = 7:13, names_to = "quantile", values_to = "lat") %>%
  select(comname, est_year, season, quantile, lat) %>%
  group_by(comname, quantile, season) %>%
  mutate(rollmean = zoo::rollapplyr(lat, width = 5, FUN = mean, align = "center", partial = T)) %>%
  group_by(comname) %>% 
  nest() %>% 
  mutate(plot = map2(data, comname, function(x,y){
    out <- ggplot(x) +
      geom_line(aes(x = est_year, y = rollmean, color = quantile), linetype = 2) +
      geom_smooth(aes(x = est_year, y = rollmean, color = quantile), method = "lm", se = F) +
      facet_wrap(~season, ncol = 2) +
      ylab("Latitude") + xlab("Year") + ggtitle(str_to_sentence(comname)) + 
      scale_color_gmri() +
      theme_gmri()
  })) -> seasonal_lat_percentiles

seasonal_lat_percentiles$plot[[28]]
seasonal_lat_pdf <- gridExtra::marrangeGrob(seasonal_lat_percentiles$plot[c(1:41)], layout_matrix = matrix(1:3, ncol = 1, nrow = 3, byrow= TRUE), top = NULL)
ggsave(seasonal_lat_pdf, filename = here("Temp_Results/", "seasonal_lat_percentiles.pdf"), height = 11, width = 8.5, unit = "in")

# 5th and 95 maps??



