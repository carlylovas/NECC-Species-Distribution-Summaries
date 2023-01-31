##Examining distribution changes by decade##

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
  mutate(decade = 10*est_year %/% 10)%>%
  group_by(comname)%>%
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

slope<-function(x) x$estimate[2]

dec_data<-dec_data%>%
  mutate(depth_mod    = map(data, possibly(depth_mod, NA)),
         bot_temp_mod = map(data, possibly(bot_temp_mod, NA)),
         sur_temp_mod = map(data, possibly(sur_temp_mod, NA)))
dec_data<-dec_data%>%
  nest(models = c(depth_mod:sur_temp_mod))
dec_data<-dec_data%>%
  filter(comname %in% c(
    "acadian redfish"          ,"american plaice"          ,"atlantic cod"            , "atlantic herring"        ,
    "black sea bass"           ,"blackbelly rosefish"      ,"buckler dory"            , "butterfish"              ,
    "cunner"                   ,"fawn cusk-eel"            ,"goosefish"               , "haddock"                 ,
    "little skate"             ,"longhorn sculpin"         ,"northern searobin"       , "offshore hake"           ,
    "red hake"                 ,"scup"                     ,"sea raven"               , "silver hake"             ,
    "smooth skate"             ,"thorny skate"             ,"windowpane flounder"     , "winter flounder"         ,
    "witch flounder"           ,"yellowtail flounder"      ,"blueback herring"        , "fourspot flounder"       ,
    "spotted hake"             ,"atlantic wolffish"        ,"ocean pout"              , "white hake"              ,
    "chain dogfish"            ,"rosette skate"            ,"weakfish"                , "american shad"           ,
    "cusk"                     ,"winter skate"             ,"atlantic halibut"        , "pollock"                 ,
    "round herring"            ,"bluefish"                 ,"spiny dogfish"           , "clearnose skate"         ,
    "atlantic mackerel"        ,"atlantic thread herring"  ,"striped bass"            , "spanish sardine"         ,
    "spot"                     ,"barndoor skate"           ,"summer flounder"         , "atlantic croaker"        ,
    "smooth dogfish"           ,"northern kingfish"        ,"roughtail stingray"      , "atlantic spadefish"      ,
    "southern kingfish"        ,"atlantic angel shark"     ,"greater amberjack"       , "bullnose ray"            ,
    "spanish mackerel"         ,"spiny butterfly ray"      ,"smooth butterfly ray"    , "cownose ray"             ,
    "sand tiger"               ,"sandbar shark"            ,"atlantic sharpnose shark", "atlantic sturgeon"  
  ))
##missing 2

##include and nest all stats
dec_data<-dec_data%>%
  unnest(models)%>%
  mutate(depth_tidy = map(depth_mod, broom::tidy), bt_tidy = map(bot_temp_mod, broom::tidy), surf_tidy = map(sur_temp_mod, broom:: tidy),
         depth_glance = map(depth_mod, broom::glance), bt_glance = map(bot_temp_mod, broom::glance), surf_glance = map(sur_temp_mod, broom:: glance),
         depth_slope = map(depth_tidy, slope), bt_slope = map(bt_tidy, slope), surf_slope = map(surf_tidy, slope),
         depth_p = depth_glance %>% map_dbl("p.value"), bt_p = bt_glance %>% map_dbl("p.value"), surf_p = surf_glance %>% map_dbl("p.value"))%>%
  nest(models = c(depth_mod, bot_temp_mod, sur_temp_mod),
       tidy_glance = c(depth_tidy:surf_glance),
       slope = c(depth_slope:surf_slope),
       p = c(depth_p:surf_p))

##depth plots
dec_data%>%
  filter(comname == "acadian redfish")%>%
  unnest(data)%>%
  ggplot(aes(est_year, avg_depth))+
  geom_point()+
  theme_gmri(axis.text.x = element_text(size = 9, angle = 90))+
  facet_wrap(~decade, ncol=5, scales="free_x")+
  scale_x_continuous(breaks = seq(1970, 2020, by=2))+
  scale_y_reverse()+
  ggtitle("Average Depth of Acadian Redfish")+
  ylab("Average Depth")+
  xlab("Year")

##depth plot loop
depth_df<-dec_data%>%
  unnest(data)%>%
  select(comname, est_year, avg_depth, decade)%>%
  group_by(comname)%>%
  nest()

nrow(depth_df)
depth_plots<-vector("list",length=66)
names(depth_plots)=paste(unique(dec_data$comname))

for(i in 1:66){
  print(i)
  loop_df<-depth_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_depth, decade)%>%
    group_by(comname)
  
  depth_plots[[i]]<- ggplot(data=loop_df,aes(est_year, avg_depth))+
    geom_point()+
    theme_gmri(axis.text.x = element_text(size = 9, angle = 90))+
    facet_wrap(~decade, ncol=5, scales="free_x")+
    scale_x_continuous(breaks = seq(1970, 2020, by=2))+
    scale_y_reverse()+
    ggtitle(ggtitle(toupper(names(depth_plots)[i])))+
    ylab("Average Depth")+
    xlab("Year")
  
  filename = paste('average_depth', unique(loop_df$comname), sep='_')
  ggsave(depth_plots[[i]], file = paste(filename,".pdf",sep=""),
         width=11, height=8)
}
