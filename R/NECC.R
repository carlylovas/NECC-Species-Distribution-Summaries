install.packages("gmRi")
library(gmRi)
library(dplyr)
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
  select("comname", "est_year", "biomass_kg", "decdeg_beglat",  "decdeg_beglon", "season") %>%
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

#center of lat/lon for all species

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
    rsq=glance%>%map_dbl("r.squared"),
    augment=map(mod,broom::augment)
  )

#extract slope (tbd)
##unnest to plot by season & species
#subset by season, plot each species over time 

#Fall plots
All_spp_lat%>%
  unnest(data)%>%
  select(comname, season, est_year, weightedLat)%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, weightedLat))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~season+comname)
  
#Spring plots
All_spp_lat%>%
  unnest(data)%>%
  select(comname, season, est_year, weightedLat)%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, weightedLat))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~season+comname)

##loop to generate individual plots (incomplete)
library(gridExtra)
plotlist<-list()

for(i in length(All_spp_lat)){
  loop_df<-All_spp_lat%>%
    unnest(data)%>%
    select(comname, season, est_year, weightedLat)%>%
    group_by(comname)
  
plotlist[[i]]<-ggplot(loop_df,aes(est_year, weightedLat))+geom_point()+geom_smooth(method='lm')
    
}

list1 = plotlist[c(1:8)]
do.call(grid.arrange,c(list1, ncol = 4))

#new functions
species_lat_mod<-function(df){
  lm(COGy~est_year, data=df)
}

species_lon_mod<-function(df){
  lm(COGx~est_year, data=df)
}

species_biomass_mod<-function(df){
  lm(biomass_kg~est_year, data=df)
}

cog_mapped<- function(df) {
  COGravity(x=df$decdeg_beglon, y=df$decdeg_beglat, z=NULL, wt=df$biomass_kg)
}

#center of gravity loopz
library(here)
source(here("R", "helper_funcs.R"))
#create some points
x = seq(154,110,length=25) # your longitudes
y = seq(-10,-54,length=25) # your latitudes
z = NULL
wt = runif(25) # your biomasses
#calculate the Centre of Gravity for these points

test<-NECC_fishes%>%
  group_by(comname,est_year,season)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "season", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

test<- test%>%
  group_by(comname, season)%>%
  left_join(unique(All_species%>%
           select("weightedLat", "weightedLon")))

write.csv(test,"~\\Data\\Center of Gravity.csv", row.names=TRUE)

##linear models, COG
nestedData<-test%>%
  group_by(comname, season)%>%
  nest()

nestedData<-nestedData%>%
  mutate(centerLat=map(data, species_lat_mod))%>%
  mutate(centerLon=map(data, species_lon_mod))%>%
  group_by(season)

##fall plots
nestedData%>%
  unnest(data)%>%
  select(comname, season, est_year, COGy)%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Fall Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

##spring plots
nestedData%>%
  unnest(data)%>%
  select(comname, season, est_year, COGy)%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Spring Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

##center of gravity without season
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

#center of gravity with season
COG_w_season<-nestedData

##extract coefficients from nested models
COG_wo_season<-COG_wo_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         coefLat=tidyLat%>%map("estimate"),
         coefLon=tidyLon%>%map("estimate"))

COG_w_season<-COG_w_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         coefLat=tidyLat%>%map("estimate"),
         coefLon=tidyLon%>%map("estimate"))

##tidy dataset (change in lat only)
clean_w_season<-COG_w_season%>%
  unnest(data)%>%
  select("comname","est_year","season","COGy","COGx","coefLat") #tried unnest_longer() and separate()

tidyData<-COG_wo_season%>%
  unnest_wider(tidyLat)%>%
  select("comname","estimate")

str(tidyData)
