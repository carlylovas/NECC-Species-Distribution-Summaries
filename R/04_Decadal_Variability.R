##Examining distribution changes by decade##
###Survey data with depth, surface & bottom temperature, and Janet Nye's method of calculating lat/lon
install.packages("matrixStats")
library(matrixStats)
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

weighted_data<-grouped_center_bio(clean_survey, est_year, season)
dec_data<-weighted_data%>%
  select(comname, est_year, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  mutate(decade = 10*est_year %/% 10)%>%
  group_by(comname, season)%>%
  nest()

##linear model functions
depth_mod<-function(df){
  lm(avg_depth~est_year, data=df)
}
bot_temp_mod<-function(df){
  lm(avg_bot_temp~est_year, data=df)
}
sur_temp_mod<-function(df){
  lm(avg_sur_temp~est_year, data=df)
}
avg_lat_mod<-function(df){
  lm(avg_lat~est_year, data=df)
}
avg_lon_mod<-function(df){
  lm(avg_lon~est_year, data=df)
}
slope<-function(x) x$estimate[2]

dec_data<-dec_data%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))

dec_data<-dec_data%>%
  nest(models = c(depth_mod:avg_lon_mod))

library(readr)
species <- read_csv("Data/species for dist analyses.csv")
species<-species%>%
  rename(comname = ...1)
species<-tolower(species$comname)

dec_data<-dec_data%>%
  filter(comname %in% species)

##include and nest all stats
dec_data<-dec_data%>%
  unnest(models)%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

#WITH season####
decadal_w_season<-dec_data

#WITHOUT season####
no_season<-weighted_data%>%
  select(comname, est_year,avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  mutate(decade = 10*est_year %/% 10)%>%
  filter(comname %in% species)%>%
  group_by(comname)%>%
  nest()

no_season<-no_season%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))%>%
  nest(models = c(depth_mod:avg_lon_mod))  

decadal_no_season<-no_season%>%
  unnest(models)%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

#2000-2009 WITH season
pre2010season<-weighted_data%>%
  filter(est_year %in% c(2000:2009))%>%
  drop_na()%>%
  select(comname, est_year, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  filter(comname %in% species)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, possibly(broom::glance), NA), surf_glance = map(sur_temp_mod, possibly(broom:: glance), NA),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

#2000-2009 WITHOUT season
pre2010<-weighted_data%>%
  filter(est_year %in% c(2000:2009))%>%
  select(comname, est_year, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  filter(comname %in% species)%>%
  group_by(comname)%>%
  nest()%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

#2010 onward WITH season
post2010season<-weighted_data%>%
  filter(est_year %in% c(2010:2020))%>%
  select(comname, est_year, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  filter(comname %in% species)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

#2010 onward WITHOUT season
post2010<-weighted_data%>%
  filter(est_year %in% c(2010:2020))%>%
  select(comname, est_year,avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  filter(comname %in% species)%>%
  group_by(comname)%>%
  nest()%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)),
         avg_lat_mod  = map(data, possibly(avg_lat_mod, NA)),
         avg_lon_mod  = map(data, possibly(avg_lon_mod, NA)))%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),lat_tidy = map(avg_lat_mod, broom::tidy), lon_tidy = map(avg_lon_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),lat_glance = map(avg_lat_mod, broom::glance), lon_glance = map(avg_lon_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope), lat_slope = map(lat_tidy, slope), lon_slope = map(lon_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"), lat_p = lat_glance %>% map_dbl("p.value"), lon_p = lon_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod, avg_lat_mod, avg_lon_mod),
       tidy_glance = c(depth_tidy:lon_glance),
       slope = c(depth_slope:lon_slope),
       p = c(depth_p:lon_p))

##slope tables
YearSeasonSlopes<-decadal_w_season%>%
  select(comname, season, slope, p)%>%
  unnest(slope)%>%
  unnest(p)
write.csv(YearSeasonSlopes, "YearSeasonSlopes.csv")

YearlySlopes<-decadal_no_season%>%
  select(comname, slope, p)%>%
  unnest(slope)%>%
  unnest(p)
write.csv(YearlySlopes, "YearlySlopes.csv")

pre2010_with_season<-pre2010season%>%
  select(comname, season, slope, p)%>%
  unnest(slope)%>%
  unnest(p)
write.csv(pre2010_with_season, "pre2010YearSeasonSlopes.csv")

pre2010_no_season<-pre2010%>%
  select(comname, slope, p)%>%
  unnest(slope)%>%
  unnest(p)
write.csv(pre2010_no_season, "pre2010YearlySlopes.csv")

post2010_with_season<-post2010season%>%
  select(comname, season, slope, p)%>%
  unnest(c(slope, p))
write.csv(post2010_with_season, "post2010YearSeasonSlopes.csv")

post2010_no_season<-post2010%>%
  select(comname, slope, p)%>%
  unnest(c(slope, p))
write.csv(post2010_no_season, "post2010YearlySlopes.csv")

