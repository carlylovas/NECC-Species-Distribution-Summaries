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

#review linear regression models



