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
