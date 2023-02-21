##Mapping functions
#MEANS
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

#ANOVA
anova_sst<-function(df){
  aov(avg_sur_temp~group, data=df)
}
anova_bt<-function(df){
  aov(avg_bot_temp~group, data=df)
}
anova_depth<-function(df){
  aov(avg_depth~group, data=df)
}
anova_lat<-function(df){
  aov(avg_lat~group, data=df)
}
anova_lon<-function(df){
  aov(avg_lon~group, data=df)
}
p<-function(x) x$p.value[1]

#BARTLETT TEST
bart_sst<-function(df){
  bartlett.test(avg_sur_temp~group, data=df)
}
bart_bt<-function(df){
  bartlett.test(avg_bot_temp~group, data=df)
}
bart_depth<-function(df){
  bartlett.test(avg_depth~group, data=df)
}
bart_lat<-function(df){
  bartlett.test(avg_lat~group, data=df)
}
bart_lon<-function(df){
  bartlett.test(avg_lon~group, data=df)
}

#T.TEST
welch_sst<-function(df){
  t.test(avg_sur_temp~group, data=df)
}
welch_bt<-function(df){
  t.test(avg_bot_temp~group, data=df)
}
welch_depth<-function(df){
  t.test(avg_depth~group, data=df)
}
welch_lat<-function(df){
  t.test(avg_lat~group, data=df)
}
welch_lon<-function(df){
  t.test(avg_lon~group, data=df)
}