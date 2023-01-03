library(gmRi)
library(dplyr)
library(here)
#load NFMS Trawl Survey

clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)

# load NECC Species List

# NECC<-read.csv("speciesList_inNECC.csv", header=TRUE)
NECC <- read.csv(here("Data", "speciesList_inNECC.csv"))
NECC<-NECC %>%
  rename(comname = Species_comnam)
NECC <- tolower(NECC$comname)
head(NECC)

#Filter trawl by species
NECC_fishes<-clean_survey %>% 
  select("comname", "est_year", "biomass_kg", "decdeg_beglat",  "decdeg_beglon", "season") %>%
  filter(comname %in% NECC)
summary(NECC_fishes)


# center of latitude over time?

dogfish_fall <- NECC_fishes %>%
  filter(comname == "smooth dogfish") %>%
  filter(season == "Fall") %>%
  group_by(est_year) %>%
  summarize(weightedLat = weighted.mean(x = decdeg_beglat, w = biomass_kg), weightedLon = weighted.mean(x = decdeg_beglon, w = biomass_kg))

# This doesn't work for me...I think maybe it would need a `data = ` argument? Or be strung together to the line above with the pipe (%>%) operator.
# ggplot(aes(x=est_year, y=weightedLat))+
#   geom_point()+
#   geom_smooth(method="lm")

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

# extract lm outputs
All_spp_lat <- All_spp_lat %>%
  mutate(
    tidy = map(mod, broom::tidy),
    glance = map(mod, broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    augment = map(mod, broom::augment)
  )

# extract slope (tbd)
# AA here: I am guessing there are quite a few ways of doing this. Here's one option
All_spp_lat <- All_spp_lat %>%
  mutate(
    tidy = map(mod, broom::tidy),
    glance = map(mod, broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    slope = tidy %>% map_dbl(function(x) x$estimate[2]),
    augment = map(mod, broom::augment)
  )

# AA again: why does this work and what is it doing? We are grabbing the "tidy" column of `All_spp_lat` and then just getting the second value in the estimate column. What else is in there?
All_spp_lat$tidy[[1]]

# AA: So, if we also wanted the std_error, we could add in slope_se = tidy %>% map_dbl(function(x) x$std.error[2]). Good to know.
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

for (i in length(All_spp_lat)) {
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

# AA here: The above wasn't working for me and I know it says incomplete. Just for a few thoughts/ideas. On the loop set up, the first thing that jumps out is the "i in length(All_spp_lat)". With loops, I tend to add a `print(i)` line (or whatever I am looping over) inside the loop to make sure it is progressing through as I expect. When we do that here, instead of 1, 2, 3, 4, 5, 6, 7, 8, 9 (which is the length of All_spp_lat or the number of columns) I just get 9.
for (i in length(All_spp_lat)) {
  print(i)
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

# AA again: We can also see this in the `plotlist` object as only the 9th element is populated. For why this is happening, the "for" bit is saying "for i in 9" and we want it to say something like "for i in 1, 2, 3, 4, 5, 6, 7, 8, 9." To get that, we could write `for(i in 1:length(All_spp_lat)` and confirm that works by typing `1:length(All_spp_lat)` in the console. 
1:length(All_spp_lat)
for (i in 1:length(All_spp_lat)) {
  print(i)
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

list1 = plotlist[c(1:8)]
do.call(grid.arrange, c(list1, ncol = 4))

# AA again: That gets us the plots, though I am guessing it is pretty clear that they are all identical. I'm not entirely sure what this loop is for, though let's say we wanted to do this to get a plot for each species/season, with 2 species per layout? The first thing we would want to change is that we want to loop over rows and not the columns in `All_spp_lat`. We are also going to want to set up the plotlist storage ahead of time just to help with speed. A bunch of ways to do this, here's one option:
n_spp_seas <- nrow(All_spp_lat)
plotlist <- vector("list", length = n_spp_seas)
names(plotlist) <- paste(All_spp_lat$comname, All_spp_lat$season, sep = "_")
str(plotlist) # 81 list elements, which is what we want

# AA: now, we can run the loop with one small change. We are going to want to grab the species/season data from All_spp_lat instead of using everything. I also made an edit so that the species_season is printed to the title of the plot. Again, not entirely sure this is what you are after, though hopefully helpful either way :)
for (i in 1:n_spp_seas) {
  print(i)
  loop_df <- All_spp_lat[i,] %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    ggtitle(names(plotlist)[i]) +
    geom_smooth(method = "lm")
}

# Plot first four species 
list1 = plotlist[c(1:8)]
do.call(grid.arrange, c(list1, ncol = 4))

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

# AA: NICE!!!! 


test <- test %>%
  group_by(comname, season) %>%
  left_join(unique(All_species %>%
    select("weightedLat", "weightedLon")))

# A few things based on the above bit and trying to bring over other info from "All_spp_lat". I think we will just want to be explicit when doing the joins so that nothing weird happens and we can do that with specifying "by" and the column names. Last thing, in the select, may want to just walways keep comname/season/year handy?
test <- test %>%
  group_by(comname, season) %>%
  left_join(All_spp_lat, by = c("comname", "season"))
test

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


