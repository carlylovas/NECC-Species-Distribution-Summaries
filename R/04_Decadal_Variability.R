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
    theme_gmri(axis.text.x=element_blank(),legend.position="none")+
    scale_color_gmri()+
    ggtitle(names(dist_plotlist)[i]) +
    geom_smooth(method = "lm")
}

list2 = dist_plotlist[c(1:4)]
do.call(grid.arrange, c(list2, ncol = 2))
list_3<-dist_plotlist[c("alewife", "scup", "smooth dogfish", "spiny dogfish")] #strong migrators 
do.call(grid.arrange, c(list_3, ncol=2))

###Survey data with depth, bottom temperature, and Janet Nye's method of calculating lat/lon








