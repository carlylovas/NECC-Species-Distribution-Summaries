##Install and load relevant packages
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggrepel")
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(ggrepel)

#plotting changes in seasonal centroid
season_dist_km%>%
  drop_na()%>%
  mutate(m = as.numeric(slope))%>%
  ggplot()+
  geom_point(aes(comname, m))+
  theme_gmri(axis.text.x=element_text(angle=90, size=8),
             axis.text.y=element_text(size=8))+
  geom_hline(yintercept=0)+
  ylab("Rate of Change")+
  xlab("Common Name")

#plot changes in center of lat####
slopes<-clean_wo_season%>%
  select(comname, est_year, slopeLat)%>%
  group_by(comname, slopeLat)%>%
  nest()%>%
  mutate(z=slopeLat>0)

slopes%>%
  ggplot()+
  geom_point(aes(comname, slopeLat))+
  theme_gmri(axis.text.x=element_text(angle=90, size=8),
             axis.text.y=element_text(size=8))+
  geom_hline(yintercept=0)+
  ylab("Rate of Change")+
  xlab("Common Name")

##no axis labels
slopes%>%
  ggplot()+
  geom_point(aes(x=comname, y=slopeLat, color=as.factor(z)))+
  geom_text_repel(aes(comname, slopeLat, label=comname), size=2.8, nudge_y=0.003)+
  theme_gmri(axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.position="none")+
  scale_color_gmri()+
  geom_hline(yintercept=0, linetype=2, linewidth=0.5, color="#00736D")+
  ylab("Rate of Change")+
  ggtitle("Changes in Center of Latitude")

#plotting biomass centroids####
yellow_flounder<-clean_wo_season%>%
  filter(comname == "yellowtail flounder")%>%
  ggplot()+
  stat_density_2d_filled(aes(est_year, COGy))+
  scale_color_gmri()+
  theme_gmri(legend.position="none")+
  ylab("Center of Latitude")
print(yellow_flounder)  

#plotting slopes pre- and post-2010
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

##plot loopz##
#without season
n_spp<-nrow(COG_wo_season)
plotlist <- vector("list", length = n_spp)
names(plotlist) <- paste(COG_wo_season$comname)
str(plotlist )

for (i in 1:n_spp) {
  print(i)
  loop_df <- COG_wo_season[i,] %>%
    unnest(data) %>%
    select(comname, est_year, COGy) %>%
    group_by(comname)
  
  plotlist[[i]] <- ggplot(loop_df, aes(est_year, COGy)) +
    geom_point() +
    ggtitle(names(plotlist)[i]) +
    geom_smooth(method = "lm")
}

list1 = plotlist[c(1:8)]
do.call(grid.arrange, c(list1, ncol = 4))


