#####
## Summarizing species distribution changes in space and time
#####

## Libraries and other necessities
library(here)
library(tidyverse)
library(sf)

## Center of Gravity from SDMTools: https://rdrr.io/cran/SDMTools/src/R/COGravity.R
source(here("R", "helper_funcs.R"))

# Make sure gmRi package is up to date!!
devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(gmRi)

## Start work

# load the necc_fishes data, which we created using the O1_SurveyDataPrep.R function
NECC_fishes<- readRDS(here("Data", "necc_fishes_occu.rds"))


####Calculate Center of Gravity
##linear model functions to map over nested data

species_lat_mod<-function(df){
  lm(COGy~est_year, data=df)
}

species_lon_mod<-function(df){
  lm(COGx~est_year, data=df)
}

#Center of Gravity with season
COG_w_season<- NECC_fishes%>%
  group_by(comname,est_year,season)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "season", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

COG_w_season<-COG_w_season%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(centerLat=map(data, species_lat_mod))%>%
  mutate(centerLon=map(data, species_lon_mod))

#Center of Gravity without season
COG_wo_season<-NECC_fishes%>%
  group_by(comname,est_year)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

COG_wo_season<-COG_wo_season%>%
  group_by(comname)%>%
  nest()%>%
  mutate(centerLat=map(data, species_lat_mod))%>%
  mutate(centerLon=map(data, species_lon_mod))

##extract coefficients from nested models
COG_wo_season<-COG_wo_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         glanceLat=map(centerLat,broom::glance),
         glanceLon=map(centerLon,broom::glance),
         slopeLat = tidyLat %>% map_dbl(function(x) x$estimate[2]),
         slopeLon = tidyLon %>% map_dbl(function(x) x$estimate[2]),
         pLat = glanceLat %>% map_dbl("p.value"),
         pLon = glanceLon %>% map_dbl("p.value"))

COG_w_season<-COG_w_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         glanceLat=map(centerLat,broom::glance),
         glanceLon=map(centerLon,broom::glance),
         slopeLat = tidyLat %>% map_dbl(function(x) x$estimate[2]),
         slopeLon = tidyLon %>% map_dbl(function(x) x$estimate[2]),
         pLat = glanceLat %>% map_dbl("p.value"),
         pLon = glanceLon %>% map_dbl("p.value"))

##tidy datasets 
clean_w_season<-COG_w_season%>%
  unnest(data)%>%
  select("comname","est_year","season","COGy","COGx")%>%
  distinct()

season_slope<-COG_w_season%>%
  select("comname", "season", "slopeLat", "slopeLon", "pLat", "pLon")%>%
  distinct()

clean_wo_season<-COG_wo_season%>%
  unnest(data)%>%
  select("comname","est_year","COGy","COGx")%>%
  distinct()

wo_season_slope<-COG_wo_season%>%
  select("comname", "slopeLat", "slopeLon", "pLat", "pLon")%>%
  distinct()

####Calculate seasonal distance
season_dist<-clean_w_season%>%
  select(comname, season, est_year, COGx, COGy)%>%
  group_by(comname, est_year)%>%
  nest()

point_dist<-function(df){
  if(FALSE){
    df<-season_dist$data[[201]]
  }
  temp<-st_as_sf(df,coords=c("COGx","COGy"), crs=4326, remove=FALSE)
  out<-st_distance(temp)[1,2]
  return(out)
}

season_dist<-season_dist%>%
  mutate(dist=map_dbl(data,possibly(point_dist, NA)))

season_dist<-season_dist%>%
  select(comname, est_year, dist)%>%
  group_by(comname)%>%
  nest()

season_dist_km<-season_dist%>%
  unnest(data)%>%
  group_by(comname, est_year)%>%
  summarise(dist_km=(dist/1000))

write.csv(season_dist_km, "Seasonal_Distance_CofBiomass.csv", row.names = FALSE)

#map linear model (distance ~ year)

dist_mod<-function(df){
  temp<-df%>%
    drop_na(dist)
  lm(dist~est_year, data=temp)
}
dist_count<-function(df){
  temp<-df%>%
    drop_na()%>%
    nrow()
}
slope<-function(x) x$estimate[2]

season_dist_km<-season_dist_km%>%
  rename("dist" = "dist_km")%>%
  group_by(comname)%>%
  nest()

season_dist_km<-season_dist_km%>%
  mutate(mod=map(data, possibly(dist_mod, NA)),
         num_obs=map(data, possibly(dist_count, NA)),
         tidy=map(mod, possibly(broom::tidy, NA)),
         slope=map(tidy, possibly(slope, NA)))


####clean and write csv files
library(MASS)

clean_w_season<-clean_w_season%>%
  rename("Center of Latitude"="COGy", "Center of Longitude"="COGx",
         "Common Name"="comname", "Year"="est_year", "Season"="season")
write.csv(clean_w_season, "NEFSC_YearSeason_Center_of_Biomass.csv", row.names = FALSE)

season_slope<-season_slope%>%
  rename("Common Name" = "comname", "Season" = "season")
write.csv(season_slope, "NEFSC_YearSeason_Rate_of_Change.csv")

clean_wo_season<-COG_wo_season%>%
  rename("Center of Latitude"="COGy", "Center of Longitude"="COGx",
         "Common Name"="comname", "Year"="est_year")
write.csv(clean_wo_season, "NEFSC_Yearly_Center_of_Biomass.csv", row.names = FALSE)

wo_season_slope<-wo_season_slope%>%
  rename("Common Name"="comname")
write.csv(wo_season_slope, "NEFSC_Yearly_Rate_of_Change.csv")

season_dist_km<-season_dist_km%>%
  select(comname, num_obs, slope)
write.matrix(season_dist_km, "Seasonal_Rate_Change.csv", sep=",")