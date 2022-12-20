library(gmRi)

#load NFMS Trawl Survey

clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)

#load NECC Species List

NECC<-read.csv("speciesList_inNECC.csv", header=TRUE)
NECC<-NECC %>%
  rename(comname = Species_comnam)
NECC <- tolower(NECC$comname)
head(NECC)

#Filter trawl by species
NECC_fishes<-clean_survey %>% 
  select("comname", "est_year", "biomass_kg", "decdeg_beglat",  "decdeg_beglon", "season", "survey_area") %>%
  filter(comname %in% NECC)
summary(NECC_fishes)


#center of latitude over time?

dogfish_fall<-NECC_fishes %>%
  filter(comname == "smooth dogfish") %>%
  filter(season == "Fall") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg))

ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm")

#more center of lat practice
NECC_fishes%>%
  filter(comname == "atlantic cod") %>%
  filter(season == "Fall") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

#center of lat for all species

All_spp_lat<-NECC_fishes %>%
  group_by(comname, est_year, season)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), 
            weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg))
  

#review linear regression models

dog_lm<-lm(weightedLat~est_year, dogfish_fall)
summary(dog_lm)

NECC_fishes%>% 
  filter(comname == "atlantic cod") %>%
  filter(season == "Fall") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

NECC_fishes%>%
  filter(comname == "atlantic cod") %>%
  filter(season == "Spring") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

All_spp_lat%>%
  filter(comname =="alewife", season == "Fall")%>%
  ggplot(aes(x=est_year, y=weightedLat)) +
  geom_point()+
  geom_smooth(method="lm")

##functions & looping
library(tidyverse)
library(tidyr)

All_spp_lat<-NECC_fishes %>%
  group_by(comname, season, est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), 
            weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg))%>%
  nest()

All_spp_lat$data[[1]]

species_mod<-function(df){
  lm(weightedLat~est_year, data=df)
}

All_spp_lat<-All_spp_lat %>%
  mutate(mod=map(data, species_mod))%>%
  group_by(season)

#extract lm outputs
All_spp_lat<-All_spp_lat%>%
  mutate(
    tidy=map(mod,broom::tidy),
    glance=map(mod,broom::glance),
    augment=map(mod,broom::augment)
  )
  

