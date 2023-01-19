##Install and load relevant packages
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggrepel")
install.packages("ggspatial")
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(ggrepel)
library(ggspatial)

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

list1 = plotlist[c(1:4)]
do.call(grid.arrange, c(list1, ncol = 4))

#with season






