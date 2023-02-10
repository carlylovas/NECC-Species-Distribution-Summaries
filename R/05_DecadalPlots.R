##PLOTS####
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

#linear fit
dec_data%>%
  filter(comname == "atlantic herring")%>%
  unnest(data)%>%
  ggplot(aes(est_year, avg_depth))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, color="#00608A")+
  theme_gmri(axis.text.x = element_text(size = 9, angle = 90))+
  facet_wrap(~decade, ncol=5, scales="free_x")+
  scale_x_continuous(breaks = seq(1970, 2020, by=2))+
  scale_y_reverse()+
  ggtitle("AMERICAN HERRING")+
  ylab("Average Depth")+
  xlab("Year")

###bottom temp plots
bottemp_df<-dec_data%>%
  unnest(data)%>%
  select(comname, est_year, avg_bot_temp, decade)%>%
  group_by(comname)%>%
  nest()
nrow(bottemp_df)
bottemp_plots<-vector("list",length=66)
names(bottemp_plots)=paste(unique(dec_data$comname))

for(i in 1:66){
  print(i)
  loop_df<-bottemp_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_bot_temp, decade)%>%
    group_by(comname)
  
  bottemp_plots[[i]]<- ggplot(data=loop_df,aes(est_year, avg_bot_temp), na.rm=TRUE)+
    geom_point()+
    theme_gmri(axis.text.x = element_text(size = 9, angle = 90))+
    facet_wrap(~decade, ncol=5, scales="free_x")+
    scale_x_continuous(breaks = seq(1970, 2020, by=2))+
    ggtitle(ggtitle(toupper(names(bottemp_plots)[i])))+
    ylab("Average Bottom Temperature")+
    xlab("Year")
}
filename = paste('avg_bot_temp', unique(loop_df$comname), sep='_')
ggsave(bottemp_plots[[i]], file = paste(filename,".pdf",sep=""),
       width=11, height=8)

bt_list<-bottemp_plots[10]
do.call(grid.arrange, bt_list)

##surface temperature plots
surtemp_df<-dec_data%>%
  unnest(data)%>%
  select(comname, est_year, avg_sur_temp, decade)%>%
  group_by(comname)%>%
  nest()
nrow(surtemp_df)
surtemp_plots<-vector("list",length=66)
names(surtemp_plots)=paste(unique(dec_data$comname))

for(i in 1:66){
  print(i)
  loop_df<-surtemp_df[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_sur_temp, decade)%>%
    group_by(comname)
  
  surtemp_plots[[i]]<- ggplot(data=loop_df,aes(est_year, avg_sur_temp), na.rm=TRUE)+
    geom_point()+
    theme_gmri(axis.text.x = element_text(size = 9, angle = 90))+
    facet_wrap(~decade, ncol=5, scales="free_x")+
    scale_x_continuous(breaks = seq(1970, 2020, by=2))+
    ggtitle(ggtitle(toupper(names(surtemp_plots)[i])))+
    ylab("Average Surface Temperature")+
    xlab("Year")
  
  filename = paste('avg_sur_temp', unique(loop_df$comname), sep='_')
  ggsave(surtemp_plots[[i]], file = paste(filename,".pdf",sep=""),
         width=11, height=8)
}