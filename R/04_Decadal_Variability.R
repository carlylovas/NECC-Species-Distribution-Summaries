##Examining distribution changes by decade##

###plotting slopes pre- and post-2010
pre2010_slopes_fall<-pre2010%>%
  unnest(data)%>%
  filter(season == "Fall")%>%
  select(comname, est_year, slopeLat)%>%
  group_by(comname, slopeLat,season)%>%
  nest()%>%
  mutate(z=slopeLat>0)

pre2010_slopes_fall<-pre2010%>%
  unnest(data)%>%
  filter(season == "Spring")%>%
  select(comname, est_year, slopeLat)%>%
  group_by(comname, slopeLat,season)%>%
  nest()%>%
  mutate(z=slopeLat>0)

post2010_slopes_fall<-post2010%>%
  unnest(data)%>%
  filter(season == "Fall")%>%
  select(comname, est_year, slopeLat)%>%
  group_by(comname, slopeLat)%>%
  nest()%>%
  mutate(z=slopeLat>0)

pre2010_slopes_fall%>%
  ggplot()+
  geom_point(aes(x=comname, y=slopeLat, color=as.factor(z)))+
  geom_text_repel(aes(comname, slopeLat, label=comname), size=2.8, nudge_y=0.003)+
  theme_gmri(axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.position="none")+
  scale_color_gmri()+
  geom_hline(yintercept=0, linetype=2, linewidth=0.5, color="#00736D")+
  ylab("Rate of Change")+
  ggtitle("Changes in Center of Latitude, Fall 1970-2009")

post2010_slopes_fall%>%
  ggplot()+
  geom_point(aes(x=comname, y=slopeLat, color=as.factor(z)))+
  geom_text_repel(aes(comname, slopeLat, label=comname), size=2.8, nudge_y=0.003)+
  theme_gmri(axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.position="none")+
  scale_color_gmri()+
  geom_hline(yintercept=0, linetype=2, linewidth=0.5, color="#00736D")+
  ylab("Rate of Change")+
  ggtitle("Changes in Center of Latitude, Fall 2009-2019")

pre2010_slopes%>%
  ggplot()+
  geom_point(aes(x=comname, y=slopeLat, color=as.factor(z)))+
  geom_text_repel(aes(comname, slopeLat, label=comname), size=2.8, nudge_y=0.003)+
  theme_gmri(axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.position="none")+
  scale_color_gmri()+
  geom_hline(yintercept=0, linetype=2, linewidth=0.5, color="#00736D")+
  ylab("Rate of Change")+
  facet_wrap(~season)

#####plotting seasonal migration patterns#
dist_km<-Seasonal_Distance_CofBiomass
dist_km%>%
  filter(comname == "alewife") %>%
  ggplot(aes(est_year, dist_km))+
  geom_point()+
  geom_line()+
  geom_smooth(method="lm")+
  theme_gmri()+
  xlab("Year")+
  ylab("Distance (km)")+
  ggtitle("Migratory Distance of Alewife")+
  scale_color_gmri()

#season distance loopz
dist_km<-dist_km%>%
  group_by(comname)%>%
  nest()
n_dist<-nrow(dist_km)
dist_plotlist<-vector("list", length = n_dist)
names(dist_plotlist)=paste(dist_km$comname)
str(dist_plotlist)

for (i in 1:n_dist) {
  print(i)
  loop_df <- dist_km[i,] %>%
    unnest(data) %>%
    select(comname, est_year, dist_km) %>%
    group_by(comname)
  
  dist_plotlist[[i]] <- ggplot(loop_df, aes(est_year, dist_km)) +
    geom_point() +  
    theme_gmri(legend.position="none")+
    scale_color_gmri()+
    ggtitle(names(dist_plotlist)[i]) +
    geom_smooth(method = "lm")
}

list2 = dist_plotlist[c(1:4)]
do.call(grid.arrange, c(list2, ncol = 2))
list_3<-dist_plotlist[c("alewife", "scup", "smooth dogfish", "spiny dogfish")] #strong migrators 
do.call(grid.arrange, c(list_3, ncol=2))


#decadal maps with season
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()+
  coord_sf(crs="+init=epsg:4326")

map_df<-clean_w_season%>%
  mutate(decade = 10*est_year %/% 10)%>%
  group_by(comname)%>%
  nest()
nrow(map_df)
decade_maps<-vector("list",length=41)
names(decade_maps)=paste(unique(clean_w_season$comname))

for(i in 1:41){
  print(i)
  loop_df<-map_df[i,]%>%
    unnest(data)%>%
    select(comname, COGx, COGy,season, decade)%>%
    group_by(comname)
  
  decade_maps[[i]]<-ggplot(data=world)+
    geom_sf()+
    coord_sf(xlim=c(-80, -65), ylim=c(30,47))+
    geom_point(data=loop_df, aes(x=COGx,y=COGy,color=season))+
    theme_gmri()+
    ggtitle(toupper(names(decade_maps)[i]))+
    ylab("Center of Latitude")+
    xlab("Center of Longitude")+
    scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
    facet_wrap(~decade, ncol=5)
  
  filename = paste('Center_of_Biomass', unique(loop_df$comname), sep='_')
  ggsave(decade_maps[[i]], file= paste("Temp_Results/Maps/", filename,".pdf",sep=""),
         width=6.5, height=4.5)
}

decade_list<-decade_maps[21]
do.call(grid.arrange, decade_list)

###Calculate latitudinal biomass percentiles, plot per decade
#Kathy says not important
install.packages("Hmisc")
library(Hmisc)
biomass_by_lat<-clean_w_season%>%
  drop_na()%>%
  mutate(decade=10*est_year %/% 10)%>%
  group_by(comname, decade)%>%
  nest()

lat_quant<-function(df){
  quantile(df$COGy, probs=c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95))
}

biomass_by_lat<-biomass_by_lat%>%
  mutate(percentile = map(data, lat_quant))%>%
  group_by(comname, decade)

test<-biomass_by_lat%>%
  unnest_wider(percentile)

test2<-biomass_by_lat%>%
  unnest_longer(percentile)

test2%>%
  filter(comname == "alewife")%>%
  unnest(data)%>%
  ggplot()+
  geom_point(aes(COGx, COGy))+
  facet_wrap(~decade, ncol=5)

test2%>%
  unnest(data)%>%
  filter(comname == "smooth dogfish",
         season == "Spring")%>%
  ggplot()+
  geom_density(aes(COGy))+
  facet_wrap(~decade, nrow=3, ncol=3)

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

weighted_data<-grouped_center_bio(clean_survey, est_year)
dec_data<-weighted_data%>%
  select(comname, est_year, avg_depth, avg_bot_temp, avg_sur_temp)%>%
  group_by(comname)%>%
  nest()

