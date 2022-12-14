library(gmRi)
clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)
NECC<-read.csv("speciesList_inNECC.csv", header=TRUE)
NECC<-NECC %>%
  rename(comname = Species_comnam)
NECC <- tolower(NECC$comname)
head(NECC)
NECC_fishes<-clean_survey %>% 
  select("comname", "est_year", "biomass_kg", "decdeg_beglat",  "decdeg_beglon", "season", "survey_area") %>%
  filter(comname %in% NECC)
