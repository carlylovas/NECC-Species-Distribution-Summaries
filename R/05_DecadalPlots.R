##PLOTS####
#testing multiplot layout####
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
install.packages("cowplot")
library(cowplot)
library(sf)
gmri_cols()

#decadal lat/long maps
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

map_data<-test%>%
  unnest(data)%>%
  select(comname, est_year, data)%>%
  unnest(data)%>%
  group_by(comname)

#maps####
map_df<-map_data%>%
  group_by(comname)%>%
  nest()
nrow(map_df)
maps<-vector("list",length=46)
names(maps)=paste(unique(map_df$comname))

for(i in 1:46){
  print(i)
  loop_df<-map_df[i,]%>%
    unnest(data)%>%
    group_by(comname)
  
  maps[[i]]<-ggplot(data=world)+
    geom_sf()+
    coord_sf(xlim=c(-80, -65), ylim=c(30,47))+
    geom_point(data=loop_df, aes(x=avg_lon,y=avg_lat, color=season))+
    theme_gmri()+
    ggtitle(toupper(names(maps)[i]))+
    ylab("Center of Latitude")+
    xlab("Center of Longitude")+
    scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
    facet_wrap(~decade, ncol=5)
}

#average lat####
lat_df<-test%>%
  select(comname, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
nrow(lat_df)
lat<-vector("list", length = 46)
names(lat)=paste(unique(lat_df$comname))

for(i in 1:46){
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
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lat)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_lat)), color="#EA4F12", linewidth=0.75)
}

#lon plot####
lon_df<-test%>%
  select(comname, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
lon<-vector("list", length = 46)
names(lon)=paste(unique(lon_df$comname))

for(i in 1:46){
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
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lon)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lon)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_lon)), color="#EA4F12", linewidth=0.75)
}

#sst plots####
st_df<-test%>%
  select(comname, data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
st<-vector("list", length = 46)
names(st)=paste(unique(st_df$comname))

for(i in 1:46){
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
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(st)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_sst)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_sst)), color="#EA4F12", linewidth=0.75)
}

#bt plots####
bt_df<-test%>%
  select(comname,data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
bt<-vector("list", length = 46)
names(st)=paste(unique(bt_df$comname))

for(i in 1:46){
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
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(bt)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_bt)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_bt)), color="#EA4F12", linewidth=0.75)
}

#depth####
depth_df<-test%>%
  select(comname,data)%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()
depth<-vector("list", length = 46)
names(depth)=paste(unique(depth_df$comname))

for(i in 1:46){
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
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(depth)[i]))+
    scale_y_reverse()+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_depth)), color="#00608A", linewidth=0.75)+
    geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_depth)), color="#EA4F12", linewidth=0.75)
}

#species profiles####
species_profiles<-vector("list", length=46)
names(species_profiles)=paste(unique(map_df$comname))
for(i in 1:46){
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
species_lat_list = lat[c(1:46)]
species_lat<-marrangeGrob(species_lat_list, nrow=6, ncol=4, top=NULL)
ggsave("lat_multipanel.pdf", species_lat, height = 15, width = 12.5, units ="in")

#all species lon
species_lon_list = lon[c(1:46)]
species_lon<-marrangeGrob(species_lon_list, nrow=6, ncol=4, top=NULL)
ggsave("lon_multipanel.pdf", species_lon, height = 15, width = 12.5, units = "in")

#all species st
species_st_list = st[c(1:46)]
species_st<-marrangeGrob(species_st_list, nrow=6, ncol=4, top=NULL)
ggsave("st_multipanel.pdf", species_st, height = 15, width = 12.5, units = "in")

#all species bt
species_bt_list = bt[c(1:46)]
species_bt<-marrangeGrob(species_bt_list, nrow=6, ncol=4, top=NULL)
ggsave("bt_multipanel.pdf", species_st, height = 15, width = 12.5, units = "in")

#all species depth
species_depth_list = depth[c(1:46)]
species_depth<-marrangeGrob(species_depth_list, nrow=6, ncol=4, top=NULL)
ggsave("depth_multipanel.pdf", species_depth, height = 15, width = 12.5, units = "in")

#decadal maps
species_maps_list = maps[c(1:46)]
species_maps<-marrangeGrob(species_maps_list, nrow= 3, ncol=1, top = NULL, padding = unit(0.75, "line"))
ggsave("species_maps.pdf", species_maps, height = 12, width = 8.5, units = "in")
