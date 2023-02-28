###SEASONAL MEANS AVERAGED TO YEAR
##Examining distribution changes by decade##
###Survey data with depth, surface & bottom temperature, and Janet Nye's method of calculating lat/lon
install.packages("matrixStats")
library(matrixStats)
head(clean_survey)

clean_survey <- clean_survey %>% 
  distinct(est_year, survey_area, stratum, tow, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(est_year, survey_area, stratum, tow, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")

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
weighted_data<-weighted_data%>%
  mutate(decade = 10*est_year %/% 10)

library(readr)
species <- read_csv("Data/species for dist analyses.csv")
species<-species%>%
  rename(comname = ...1)
species<-tolower(species$comname)

dec_data<-weighted_data%>%
  select(comname, est_year, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  mutate(decade = 10*est_year %/% 10)%>%
  group_by(comname, season)%>%
  nest()%>%
  filter(comname %in% species)
  
dec_data<-dec_data%>% 
  filter(!comname %in% c("atlantic croaker",
                       "atlantic hagfish",
                       "barndoor skate",
                       "horseshoe crab",
                       "northern kingfish",
                       "spot",
                       "striped bass",
                       "tautog",
                       "tilefish",
                       "weakfish")) %>%
  mutate(num_obs = map(data, count))

##aggregated averages
test<-dec_data%>%
  unnest(data)%>%
  group_by(comname, est_year)%>%
  nest()

test<-test%>%
  mutate( avg_depth       = as.numeric(map(data, possibly(mean_depth, NA))),
          avg_bot_temp    = as.numeric(map(data, possibly(avg_bt, NA))),
          avg_sur_temp    = as.numeric(map(data, possibly(avg_sst, NA))),
          avg_lat         = as.numeric(map(data, possibly(mean_lat, NA))),
          avg_lon         = as.numeric(map(data, possibly(mean_lon, NA))))

test<-test%>%
  group_by(comname)%>%
  nest()


#equal observations
equal_obs_fun<- function(df){df$num_obs[1] == df$num_obs[2]}
equal_obs<-dec_data%>%
  select(comname, season, num_obs)%>%
  group_by(comname)%>%
  nest()%>%
  mutate(equal_obs = map(data, equal_obs_fun),
       equal_obs_years = unlist(equal_obs))%>%
  select(comname, equal_obs_years)

###T-TEST####
subset1<-test%>%
  unnest(data)%>%
  filter(est_year %in% c(1970:2009))%>%
  mutate(group = as.character("group_1"))
subset2<-test%>%
  unnest(data)%>%
  filter(est_year %in% c(2000:2009))%>%
  mutate(group = as.character("group_2"))
subset3<-test%>%
  unnest(data)%>%
  filter(est_year %in% c(2010:2019))%>%
  mutate(group = as.character("group_3"))

t_test<-bind_rows(subset1, subset2, subset3)

#unequal variance test####
#1970-2009 vs 2010-2019
ttest_group_1<-t_test%>%
  filter(group %in% c("group_1", "group_3"))%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_obs      = map(data, possibly(count, NA)))%>%
  mutate(bart_sst     = map(data, possibly(bart_sst, NA)),
         bart_bt      = map(data, possibly(bart_bt, NA)),
         bart_depth   = map(data, possibly(bart_depth, NA)),
         bart_lat     = map(data, possibly(bart_lat, NA)),
         bart_lon     = map(data, possibly(bart_lon, NA)))%>%
  mutate(welch_sst    = map(data, possibly(welch_sst, NA)),
         welch_bt     = map(data, possibly(welch_bt,NA)),
         welch_depth  = map(data, possibly(welch_depth, NA)),
         welch_lat    = map(data, possibly(welch_lat, NA)),
         welch_lon    = map(data, possibly(welch_lon, NA)))%>%
  mutate(tidy_bart_sst     = map(bart_sst,   broom::tidy),
         tidy_bart_bt      = map(bart_bt,    broom::tidy),
         tidy_bart_depth   = map(bart_depth, broom::tidy),
         tidy_bart_lat     = map(bart_lat,   broom::tidy),
         tidy_bart_lon     = map(bart_lon,   broom::tidy))%>%
  mutate(tidy_welch_sst     = map(welch_sst,   broom::tidy),
         tidy_welch_bt      = map(welch_bt,    broom::tidy),
         tidy_welch_depth   = map(welch_depth, broom::tidy),
         tidy_welch_lat     = map(welch_lat,   broom::tidy),
         tidy_welch_lon     = map(welch_lon,   broom::tidy))%>%
  mutate(bart_sst_p        = map(tidy_bart_sst,   p),
         bart_bt_p         = map(tidy_bart_bt,    p),
         bart_depth_p      = map(tidy_bart_depth, p),
         bart_lat_p        = map(tidy_bart_lat,   p),
         bart_lon_p        = map(tidy_bart_lon,   p))%>%
  mutate(welch_sst_p       = map(tidy_welch_sst, p),
         welch_bt_p        = map(tidy_welch_bt, p),
         welch_depth_p     = map(tidy_welch_depth, p),
         welch_lat_p       = map(tidy_welch_lat, p),
         welch_lon_p       = map(tidy_welch_lon, p))%>%
  nest(bartlett_test  = c(bart_sst:bart_lon),
       welch_t_test   = c(welch_sst:welch_lon),
       tidy           = c(tidy_bart_sst:tidy_welch_lon),
       bart_p_values  = c(bart_sst_p:bart_lon_p),
       welch_p_values = c(welch_sst_p:welch_lon_p))


#2000-2009 vs 2010-2019
ttest_group_2<-t_test%>%
  filter(group %in% c("group_2", "group_3"))%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_obs      = map(data, possibly(count, NA)))%>%
  mutate(bart_sst     = map(data, possibly(bart_sst, NA)),
         bart_bt      = map(data, possibly(bart_bt, NA)),
         bart_depth   = map(data, possibly(bart_depth, NA)),
         bart_lat     = map(data, possibly(bart_lat, NA)),
         bart_lon     = map(data, possibly(bart_lon, NA)))%>%
  mutate(welch_sst    = map(data, possibly(welch_sst, NA)),
         welch_bt     = map(data, possibly(welch_bt,NA)),
         welch_depth  = map(data, possibly(welch_depth, NA)),
         welch_lat    = map(data, possibly(welch_lat, NA)),
         welch_lon    = map(data, possibly(welch_lon, NA)))%>%
  mutate(tidy_bart_sst     = map(bart_sst,   broom::tidy),
         tidy_bart_bt      = map(bart_bt,    broom::tidy),
         tidy_bart_depth   = map(bart_depth, broom::tidy),
         tidy_bart_lat     = map(bart_lat,   broom::tidy),
         tidy_bart_lon     = map(bart_lon,   broom::tidy))%>%
  mutate(tidy_welch_sst     = map(welch_sst,   broom::tidy),
         tidy_welch_bt      = map(welch_bt,    broom::tidy),
         tidy_welch_depth   = map(welch_depth, broom::tidy),
         tidy_welch_lat     = map(welch_lat,   broom::tidy),
         tidy_welch_lon     = map(welch_lon,   broom::tidy))%>%
  mutate(bart_sst_p        = map(tidy_bart_sst,   p),
         bart_bt_p         = map(tidy_bart_bt,    p),
         bart_depth_p      = map(tidy_bart_depth, p),
         bart_lat_p        = map(tidy_bart_lat,   p),
         bart_lon_p        = map(tidy_bart_lon,   p))%>%
  mutate(welch_sst_p       = map(tidy_welch_sst, p),
         welch_bt_p        = map(tidy_welch_bt, p),
         welch_depth_p     = map(tidy_welch_depth, p),
         welch_lat_p       = map(tidy_welch_lat, p),
         welch_lon_p       = map(tidy_welch_lon, p))%>%
  nest(bartlett_test  = c(bart_sst:bart_lon),
       welch_anova    = c(welch_sst:welch_lon),
       tidy           = c(tidy_bart_sst:tidy_welch_lon),
       bart_p_values  = c(bart_sst_p:bart_lon_p),
       welch_p_values = c(welch_sst_p:welch_lon_p))

##Clean Welch's ANOVA
Welch_TTest_1<-ttest_group_1%>%
  select(comname, bart_p_values, welch_p_values, num_obs)%>%
  unnest(c(bart_p_values, welch_p_values))%>%
  drop_na()%>%
  mutate(bart_sst_p        = as.numeric(bart_sst_p),
         welch_sst_p       = as.numeric(welch_sst_p),
         bart_bt_p         = as.numeric(bart_bt_p),
         welch_bt_p        = as.numeric(welch_bt_p),
         bart_depth_p      = as.numeric(bart_depth_p),
         welch_depth_p     = as.numeric(welch_depth_p),
         bart_lat_p        = as.numeric(bart_lat_p),
         welch_lat_p       = as.numeric(welch_lat_p),
         bart_lon_p        = as.numeric(bart_lon_p),
         welch_lon_p       = as.numeric(welch_lon_p))%>%
  relocate(welch_sst_p, .after =bart_sst_p)%>%
  relocate(welch_bt_p, .after=bart_bt_p)%>%
  relocate(welch_depth_p, .after=bart_depth_p)%>%
  relocate(welch_lat_p, .after=bart_lat_p)%>%
  relocate(welch_lon_p, .after=bart_lon_p)%>%
  mutate(across(where(is.numeric), round, 3))

Welch_TTest_2<-ttest_group_2%>%
  select(comname, bart_p_values, welch_p_values, num_obs)%>%
  unnest(c(bart_p_values, welch_p_values))%>%
  drop_na()%>%
  mutate(bart_sst_p        = as.numeric(bart_sst_p),
         welch_sst_p       = as.numeric(welch_sst_p),
         bart_bt_p         = as.numeric(bart_bt_p),
         welch_bt_p        = as.numeric(welch_bt_p),
         bart_depth_p      = as.numeric(bart_depth_p),
         welch_depth_p     = as.numeric(welch_depth_p),
         bart_lat_p        = as.numeric(bart_lat_p),
         welch_lat_p       = as.numeric(welch_lat_p),
         bart_lon_p        = as.numeric(bart_lon_p),
         welch_lon_p       = as.numeric(welch_lon_p))%>%
  relocate(welch_sst_p, .after =bart_sst_p)%>%
  relocate(welch_bt_p, .after=bart_bt_p)%>%
  relocate(welch_depth_p, .after=bart_depth_p)%>%
  relocate(welch_lat_p, .after=bart_lat_p)%>%
  relocate(welch_lon_p, .after=bart_lon_p)%>%
  mutate(across(where(is.numeric), round, 3))

#MEANS####
decadal_means<-t_test%>%
  group_by(comname, group)%>%
  nest()%>%
  mutate(mean_sst   = as.numeric(map(data, avg_sst)),
         mean_bt    = as.numeric(map(data, avg_bt)),
         mean_depth = as.numeric(map(data, mean_depth)),
         mean_lat   = as.numeric(map(data, mean_lat)),
         mean_lon   = as.numeric(map(data, mean_lon)))

group_1_means<-decadal_means%>%
  filter(group == "group_1")%>%
  select(comname, mean_sst:mean_lon)%>%
  group_by(comname)%>%
  mutate(across(where(is.numeric), round, 3))
group_2_means<-decadal_means%>%
  filter(group == "group_2")%>%
  select(comname, mean_sst:mean_lon)%>%
  group_by(comname)%>%
  mutate(across(where(is.numeric), round, 3))
group_3_means<-decadal_means%>%
  filter(group == "group_3")%>%
  select(comname, mean_sst:mean_lon)%>%
  group_by(comname)%>%
  mutate(across(where(is.numeric), round, 3))

library(MASS)
write.matrix(Welch_TTest_1, "Welch_t_test_1.csv", sep=",")
write.matrix(Welch_TTest_2, "Welch_t_test_2.csv", sep=",")
write.csv(group_1_means, "Group_1_means.csv")
write.csv(group_2_means, "Group_2_means.csv")
write.csv(group_3_means, "Group_3_means.csv")

##Kathy's revisions
table<-Welch_TTest_1%>%
  select(welch_lat_p, welch_lon_p, welch_sst_p, welch_bt_p, welch_depth_p)
write.csv(table, "p_values.csv")

#with species
survey<-clean_survey%>%
  left_join(., survey_tows)%>%
  select(id, comname, est_year, season, avgdepth, surftemp, bottemp)%>%
  filter(comname %in% species)%>%
  distinct()%>% 
  group_by(comname, season, est_year)%>%
  nest()%>%
  mutate(total_obs = map(data, count))

survey_test<-survey%>%
  unnest(data)%>%
  summarise(st_na = sum(is.na(surftemp)),
            bt_na = sum(is.na(bottemp)),
            depth_na = sum(is.na(avgdepth)))

species_survey<-survey_test%>%
  left_join(survey %>%
              select(total_obs)) %>%
  unite(st_na, total_obs, col = "st_na", sep="/")%>%
  left_join(survey %>%
              select(total_obs)) %>%
  unite(bt_na, total_obs, col = "bt_na", sep="/")%>%
  left_join(survey %>%
              select(total_obs)) %>%
  unite(depth_na, total_obs, col = "depth_na", sep ="/")

#without species
survey_season<- clean_survey%>%
  left_join(., survey_tows)%>%
  select(id, comname, est_year, season, avgdepth, surftemp, bottemp)%>%
  filter(comname %in% species)%>%
  distinct()%>% 
  group_by(est_year, season)%>%
  nest()%>%
  mutate(total_obs = map(data, count)) 

survey_NAs<-survey_season%>%
  unnest(data)%>%
  summarise(st_na = sum(is.na(surftemp)),
            bt_na = sum(is.na(bottemp)),
            depth_na = sum(is.na(avgdepth)),
            .groups = "keep")%>%
  group_by(est_year, season)

survey_NAs<-survey_NAs%>%
  left_join(survey_season %>%
              select(total_obs)) %>%
  unite(st_na, total_obs, col = "st_na", sep="/")%>%
  left_join(survey_season %>%
              select(total_obs)) %>%
  unite(bt_na, total_obs, col = "bt_na", sep="/")%>%
  left_join(survey_season %>%
              select(total_obs)) %>%
  unite(depth_na, total_obs, col = "depth_na", sep ="/")

write.csv(survey_NAs, "survey_NAs.csv")
