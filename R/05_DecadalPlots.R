##PLOTS####
#testing multiplot layout####
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
install.packages("cowplot")
library(cowplot)
library(sf)

plots_df<-decadal_w_season%>%
  unnest(data)%>%
  select(comname, est_year, season, avg_lat, avg_lon, avg_sur_temp, avg_bot_temp, avg_depth, decade)%>%
  distinct()

pre2010_df<-weighted_data%>%
  filter(est_year %in% c(2000:2009))%>%
  select(comname, est_year, season, avg_lat, avg_lon, avg_sur_temp, avg_bot_temp, avg_depth)%>%
  distinct()

post2010_df<-weighted_data%>%
  filter(est_year %in% c(2010:2020))%>%
  select(comname, est_year, season, avg_lat, avg_lon, avg_sur_temp, avg_bot_temp, avg_depth)%>%
  distinct()

group1_averages<-dec_data%>%
  unnest(data)%>%
  filter(est_year %in% c(1970:2009))%>%
  select(comname, season, est_year, avg_sur_temp, avg_bot_temp, avg_depth, avg_lat, avg_lon)%>%
  drop_na()%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(overall_sst = map(data, possibly(avg_sst, NA)),
         overall_bt = map(data, possibly(avg_bt, NA)),
         overall_depth = map(data, possibly(mean_depth, NA)),
         overall_lat = map(data, possibly(mean_lat, NA)),
         overall_lon = map(data, possibly(mean_lon, NA)))

group2_averages<-dec_data%>%
  unnest(data)%>%
  filter(est_year %in% c(2000:2009))%>%
  select(comname, season, est_year, avg_sur_temp, avg_bot_temp, avg_depth, avg_lat, avg_lon)%>%
  drop_na()%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(overall_sst = map(data, possibly(avg_sst, NA)),
         overall_bt = map(data, possibly(avg_bt, NA)),
         overall_depth = map(data, possibly(mean_depth, NA)),
         overall_lat = map(data, possibly(mean_lat, NA)),
         overall_lon = map(data, possibly(mean_lon, NA)))

group3_averages<-dec_data%>%
  unnest(data)%>%
  filter(est_year %in% c(2010:2020))%>%
  select(comname, season, est_year, avg_sur_temp, avg_bot_temp, avg_depth, avg_lat, avg_lon)%>%
  drop_na()%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(overall_sst = map(data, possibly(avg_sst, NA)),
         overall_bt = map(data, possibly(avg_bt, NA)),
         overall_depth = map(data, possibly(mean_depth, NA)),
         overall_lat = map(data, possibly(mean_lat, NA)),
         overall_lon = map(data, possibly(mean_lon, NA)))
#decadal lat/long maps
gmri_cols()
world <- ne_countries(scale = "medium", returnclass = "sf")

#LOOPS####
#average functions####
avg_sst<-function(df){
  mean(df$avg_sur_temp, na.rm=T)
}
avg_bt<-function(df){
  mean(df$avg_bot_temp, na.rm=T)
}
mean_depth<-function(df){
  mean(df$avg_depth, na.rm=T)
}
mean_lat<-function(df){
  mean(df$avg_lat, na.rm=T)
}
mean_lon<-function(df){
  mean(df$avg_lon, na.rm=T)
}

#maps####
map_df<-dec_data%>%
  select(comname, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
nrow(map_df)
maps<-vector("list",length=56)
names(maps)=paste(unique(map_df$comname))

for(i in 1:56){
  print(i)
  loop_df<-map_df[i,]%>%
    unnest(data)%>%
    select(comname, avg_lat, avg_lon, decade)%>%
    group_by(comname)
  
  maps[[i]]<-ggplot(data=world)+
    geom_sf()+
    coord_sf(xlim=c(-80, -65), ylim=c(30,47))+
    geom_point(data=loop_df, aes(x=avg_lon,y=avg_lat))+
    theme_gmri()+
    ggtitle(toupper(names(maps)[i]))+
    ylab("Center of Latitude")+
    xlab("Center of Longitude")+
    scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
    facet_wrap(~decade, ncol=5)
}
#average lat####
lat_df<-dec_data%>%
  select(comname, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
lat<-vector("list", length = 56)
names(lat)=paste(unique(lat_df$comname))

for(i in 1:56){
  print(i)
  loop_df<-lat_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_lat)%>%
    group_by(comname)
  
  group1<-lat_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lat = map(data, possibly(mean_lat, NA)))%>%
    unnest(data)
  
  group2<-lat_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2000:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lat = map(data, possibly(mean_lat, NA)))%>%
    unnest(data)
  
  group3<-lat_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2020))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lat = map(data, possibly(mean_lat, NA)))%>%
    unnest(data)
  
  lat[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_lat))+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lat)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lat)), color="#EACA00", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_lat)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_lat)), color="#EA4F12", linewidth=0.75)
}

#lon plot####
lon_df<-dec_data%>%
  select(comname, season, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
lon<-vector("list", length = 55)
names(lon)=paste(unique(lon_df$comname))

for(i in 1:55){
  print(i)
  loop_df<-lon_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_lon)%>%
    group_by(comname)
  
  group1<-lon_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lon = map(data, possibly(mean_lon, NA)))%>%
    unnest(data)
  
  group2<-lon_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2000:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lon = map(data, possibly(mean_lon, NA)))%>%
    unnest(data)
  
  group3<-lon_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2020))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lon = map(data, possibly(mean_lon, NA)))%>%
    unnest(data)
  
  lon[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_lon))+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               axis.text.y = element_text(size=10))+
    ggtitle("Average Longitude")+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lon)), color="#EACA00", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_lon)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_lon)), color="#EA4F12", linewidth=0.75)
}

#sst plots####
st_df<-dec_data%>%
  select(comname, season, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
st<-vector("list", length = 55)
names(st)=paste(unique(st_df$comname))

for(i in 1:55){
  print(i)
  loop_df<-st_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_sur_temp)%>%
    mutate(post2010 = est_year>2010)%>%
    group_by(comname)
  
  group1<-st_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_sst = map(data, possibly(avg_sst, NA)))%>%
    unnest(data)
  
  group2<-st_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2000:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_sst = map(data, possibly(avg_sst, NA)))%>%
    unnest(data)
  
  group3<-st_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2020))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_sst = map(data, possibly(avg_sst, NA)))%>%
    unnest(data)
  
  st[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_sur_temp))+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               axis.text.y = element_text(size=10))+
    ggtitle("Average Surface Temp")+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_sst)), color="#EACA00", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_sst)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_sst)), color="#EA4F12", linewidth=0.75)
}

#bt plots####
bt_df<-dec_data%>%
  select(comname, season, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
bt<-vector("list", length = 55)
names(st)=paste(unique(bt_df$comname))

for(i in 1:55){
  print(i)
  loop_df<-bt_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_bot_temp)%>%
    group_by(comname)
  
  group1<-bt_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_bt = map(data, possibly(avg_bt, NA)))%>%
    unnest(data)
  
  group2<-bt_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2000:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_bt = map(data, possibly(avg_bt, NA)))%>%
    unnest(data)
  
  group3<-bt_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2020))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_bt = map(data, possibly(avg_bt, NA)))%>%
    unnest(data)
  
  bt[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_bot_temp))+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               axis.text.y = element_text(size=10))+
    ggtitle("Average Bottom Temp")+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_bt)), color="#EACA00", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_bt)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_bt)), color="#EA4F12", linewidth=0.75)
}

#depth####
depth_df<-dec_data%>%
  select(comname, season, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
depth<-vector("list", length = 55)
names(st)=paste(unique(depth_df$comname))

for(i in 1:55){
  print(i)
  loop_df<-depth_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_depth)%>%
    group_by(comname)
  
  group1<-depth_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_depth = map(data, possibly(mean_depth, NA)))%>%
    unnest(data)
  
  group2<-depth_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2000:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_depth = map(data, possibly(mean_depth, NA)))%>%
    unnest(data)
  
  group3<-depth_df[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2020))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_depth = map(data, possibly(mean_depth, NA)))%>%
    unnest(data)
  
  depth[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_depth))+
    geom_point(size=0.5)+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               axis.text.y = element_text(size = 10))+
    ggtitle("Average Depth")+
    scale_y_reverse()+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_depth)), color="#EACA00", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_depth)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_depth)), color="#EA4F12", linewidth=0.75)
}

#species profiles####
species_profiles<-vector("list", length=55)
names(species_profiles)=paste(unique(map_df$comname))
for(i in 1:55){
  print(i)
  loop_df<-map_df[i,]%>%
    select(comname)
  species_profiles[[i]]<-grid.arrange(maps[[i]], lat[[i]], lon[[i]], st[[i]], bt[[i]], depth[[i]], nrow=4, 
                                      layout_matrix=rbind(c(1,1), c(2,3), c(4,5), c(6,NA))) 
  
  filename = paste('species_profile', unique(loop_df$comname), sep='_')
  ggsave(species_profiles[[i]], file = paste(filename,".pdf",sep=""),
         width=9, height=16) 
}

##all species lats
species_lat_list = lat[c(1:56)]
species_lat<-marrangeGrob(species_lat_list, nrow=5, ncol=3)
ggsave("lat_multipage.pdf", species_lat, height = 11, width=9, units = "in")
