

#validating just the El Nino period-----------

#Temperature---- ##########
#filtering to the 2015-16 El nino period-----------

#South America
erai_elnino_SA <- erai_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
merra2_elnino_SA <- merra2_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
jra55_elnino_SA <- jra55_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
cfsr_elnino_SA <- cfsr_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
#Africa
erai_elnino_A <- erai_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
merra2_elnino_A <- merra2_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
jra55_elnino_A <- jra55_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
cfsr_elnino_A <- cfsr_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
#SE Asia
erai_elnino_SEA <- erai_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
merra2_elnino_SEA <- merra2_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
jra55_elnino_SEA <- jra55_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
cfsr_elnino_SEA <- cfsr_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))

#add dfs to a list
elnino_list=list("ERA-I Sth America" = erai_elnino_SA, "ERA-I Africa"= erai_elnino_A, "ERA-I SE Asia"=erai_elnino_SEA,
       "MERRA2 Sth America" = merra2_elnino_SA, "MERRA2 Africa" = merra2_elnino_A, "MERRA2 SE Asia"=merra2_elnino_SEA,
       "JRA-55 Sth America"=jra55_elnino_SA, "JRA-55 Africa"=jra55_elnino_A, "JRA-55 SE Asia"=jra55_elnino_SEA,
       "CFSR Sth America"=cfsr_elnino_SA, "CFSR Africa"=cfsr_elnino_A, "CFSR SE Asia"=cfsr_elnino_SEA)

#linear models-----
lm_temp_results_elnino <- list()
for (i in 1:length(elnino_list)) {
  lm_obj <- lm(TAVG_model ~ TAVG_station, data = elnino_list[[i]])
  tmp <- c(lm_obj$coefficients,
           summary(lm_obj)$r.squared,
           mae(elnino_list[[i]]$TAVG_station, elnino_list[[i]]$TAVG_model),
           # rmse(elnino_list[[i]]$TAVG_station, elnino_list[[i]]$TAVG_model),
           mse_s((coef(lm_obj)[1] + (coef(lm_obj)[2]*elnino_list[[i]]$TAVG_station)), elnino_list[[i]]$TAVG_station),
           mse_u(elnino_list[[i]]$TAVG_model, (coef(lm_obj)[1] + (coef(lm_obj)[2]*elnino_list[[i]]$TAVG_station))),
           mean(elnino_list[[i]]$TAVG_station, na.rm=T),
           sd(elnino_list[[i]]$TAVG_station, na.rm=T),
           mean(elnino_list[[i]]$TAVG_model, na.rm=T),
           sd(elnino_list[[i]]$TAVG_model, na.rm=T),
           hydroGOF::md(elnino_list[[i]]$TAVG_model, elnino_list[[i]]$TAVG_station),
           length(na.omit(elnino_list[[i]]$TAVG_station)))
  names(tmp) <- c("intercept", "model", "r.squared", "mae","mse-s", "mse-u",
                  "mean_obs", "sd_obs", "mean_model", "sd_model", "d1","n")
  lm_temp_results_elnino[[i]] <- tmp
}
lm_temp_results_elnino <- as.data.frame(do.call(rbind, lm_temp_results_elnino))
lm_temp_results_elnino <- lm_temp_results_elnino %>% mutate("mse"=(`mse-s`+`mse-u`)) %>% 
                                                     mutate("rmse"=sqrt(mse)) %>%  
                                                     mutate("mse-s/mse"=`mse-s`/mse*100) %>% 
                                                     mutate("mse-u/mse"=`mse-u`/mse*100) %>% 
                                                     mutate("rmse-s"=sqrt(`mse-s`)) %>% 
                                                     mutate("rmse-u"=sqrt(`mse-u`))  
lm_temp_results_elnino$id = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                              "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia")
write_csv(lm_temp_results_elnino, path="results/stats_temp_elnino.csv")

# scatterplots------

#combine data frames into a single df for easy facet plotting
tempScatter_all_elnino <- bind_rows(elnino_list, .id="id")
tempScatter_all_elnino <- left_join(tempScatter_all_elnino, lm_temp_results_elnino, by="id") #joining summary stats
#reorder the factors so they plot in facetwrap the way I want
tempScatter_all_elnino$id <- factor(tempScatter_all_elnino$id, 
                             levels = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                              "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia")) 

ggplot(data=tempScatter_all_elnino, aes(x=TAVG_station, y=TAVG_model, colour=id)) + 
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(alpha=0.9, size=1, fill=NA, shape=21) +
  stat_smooth(method = "lm", linetype="longdash", col="black", size=0.6)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Temp. (°C)")+
  ylab("Model Temp. (°C)")+
  theme(legend.position="none")+
  theme(strip.background =element_rect(fill="peachpuff2"))
  # geom_text(aes(label=paste("R2=", round(r.squared,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=1.2, size=3.5)+
  # geom_text(aes(label=paste("MAE=", round(mae,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=2.4, size=3.5)

# qqplots------

#South America
erai_qq_SA_elnino <- QQ.Function_temp(erai_elnino_SA) #function is in 'temp' script
merra2_qq_SA_elnino <- QQ.Function_temp(merra2_elnino_SA)
jra55_qq_SA_elnino <- QQ.Function_temp(jra55_elnino_SA)
cfsr_qq_SA_elnino <- QQ.Function_temp(cfsr_elnino_SA)

#Africa
erai_qq_A_elnino <- QQ.Function_temp(erai_elnino_A)
merra2_qq_A_elnino <- QQ.Function_temp(merra2_elnino_A)
jra55_qq_A_elnino <- QQ.Function_temp(jra55_elnino_A)
cfsr_qq_A_elnino <- QQ.Function_temp(cfsr_elnino_A)

#SE Asia
erai_qq_SEA_elnino <- QQ.Function_temp(erai_elnino_SEA)
merra2_qq_SEA_elnino <- QQ.Function_temp(merra2_elnino_SEA)
jra55_qq_SEA_elnino <- QQ.Function_temp(jra55_elnino_SEA)
cfsr_qq_SEA_elnino <- QQ.Function_temp(cfsr_elnino_SEA)

#plot
qq_list_elnino = list("ERA-I Sth America"=erai_qq_SA_elnino, "ERA-I Africa"=erai_qq_A_elnino, "ERA-I SE Asia"=erai_qq_SEA_elnino,
               "MERRA2 Sth America"=merra2_qq_SA_elnino, "MERRA2 Africa"=merra2_qq_A_elnino, "MERRA2 SE Asia"=merra2_qq_SEA_elnino,
               "JRA-55 Sth America"=jra55_qq_SA_elnino, "JRA-55 Africa"=jra55_qq_A_elnino, "JRA-55 SE Asia"=jra55_qq_SEA_elnino,
               "CFSR Sth America"=cfsr_qq_SA_elnino, "CFSR Africa"=cfsr_qq_A_elnino, "CFSR SE Asia"=cfsr_qq_SEA_elnino)

#plotting with ggplot and facet wrap--

#combine data frames into a single df for easy facet plotting
qq_all_elnino <- bind_rows(qq_list_elnino, .id="id")
#reorder the id factors so they plot in the facet wrap the way I want
qq_all_elnino$id <- factor(qq_all_elnino$id, levels = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                                        "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia")) 
qq_all_elnino$percentile <- rep(c("0.1","1","5","10", "25", "50", "75", "90", "95", "99", "99.9"), 12)

#plot
ggplot(data=qq_all_elnino, aes(x=station, y=model, label=percentile))+
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(colour="blue")+
  geom_line(colour="blue")+
  geom_text(hjust = 0,nudge_x = 0.35, size=3)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Temp. (°C)")+
  ylab("Model Temp. (°C)")+
  theme(strip.background =element_rect(fill="peachpuff2"))







#  Rainfall-----##########

#filtering to the 2015-16 El nino period-----------

#South America
trmm_elnino_SA <- trmm_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
chirps_elnino_SA <- chirps_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
persiann_elnino_SA <- persiann_validation_SA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))

#Africa
trmm_elnino_A <- trmm_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
chirps_elnino_A <- chirps_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
persiann_elnino_A <- persiann_validation_A %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))

#SE Asia
trmm_elnino_SEA <- trmm_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
chirps_elnino_SEA <- chirps_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))
persiann_elnino_SEA <- persiann_validation_SEA %>% filter(DATE >= as.Date("2015-01-01") & DATE < as.Date("2016-06-01"))

#add dfs to a list
rainScatter_list_elnino=list("TRMM Sth America" = trmm_elnino_SA, "TRMM Africa"= trmm_elnino_A, "TRMM SE Asia"=trmm_elnino_SEA,
                      "CHIRPS Sth America" = chirps_elnino_SA, "CHIRPS Africa" = chirps_elnino_A, "CHIRPS SE Asia"=chirps_elnino_SEA,
                      "PERSIANNCDR Sth America"=persiann_elnino_SA, "PERSIANNCDR Africa"=persiann_elnino_A, "PERSIANNCDR SE Asia"=persiann_elnino_SEA)

#linear models-----
lm_rain_results_elnino <- list()
for (i in 1:length(rainScatter_list_elnino)) {
  lm_obj <- lm(PRCP_model ~ PRCP_station, data = rainScatter_list_elnino[[i]])
  tmp <- c(lm_obj$coefficients,
           summary(lm_obj)$r.squared,
           mae(rainScatter_list_elnino[[i]]$PRCP_station, rainScatter_list_elnino[[i]]$PRCP_model),
           # rmse(rainScatter_list_elnino[[i]]$PRCP_station, rainScatter_list_elnino[[i]]$PRCP_model),
           mse_s((coef(lm_obj)[1] + (coef(lm_obj)[2]*rainScatter_list_elnino[[i]]$PRCP_station)), rainScatter_list_elnino[[i]]$PRCP_station),
           mse_u(rainScatter_list_elnino[[i]]$PRCP_model, (coef(lm_obj)[1] + (coef(lm_obj)[2]*rainScatter_list_elnino[[i]]$PRCP_station))),
           mean(rainScatter_list_elnino[[i]]$PRCP_station, na.rm=T),
           sd(rainScatter_list_elnino[[i]]$PRCP_station, na.rm=T),
           mean(rainScatter_list_elnino[[i]]$PRCP_model, na.rm=T),
           sd(rainScatter_list_elnino[[i]]$PRCP_model, na.rm=T),
           hydroGOF::md(rainScatter_list_elnino[[i]]$PRCP_model, rainScatter_list_elnino[[i]]$PRCP_station),
           length(na.omit(rainScatter_list_elnino[[i]]$PRCP_station)))
  names(tmp) <- c("intercept", "model", "r.squared", "mae", "mse-s", "mse-u",
                  "mean_obs", "sd_obs", "mean_model", "sd_model", "d1","n")
  lm_rain_results_elnino[[i]] <- tmp
}
lm_rain_results_elnino <- as.data.frame(do.call(rbind, lm_rain_results_elnino))
lm_rain_results_elnino <- lm_rain_results_elnino %>% mutate("mse"=(`mse-s`+`mse-u`)) %>% 
                                                     mutate("rmse"=sqrt(mse)) %>%  
                                                     mutate("mse-s/mse"=`mse-s`/mse*100) %>% 
                                                     mutate("mse-u/mse"=`mse-u`/mse*100) %>% 
                                                     mutate("rmse-s"=sqrt(`mse-s`)) %>% 
                                                     mutate("rmse-u"=sqrt(`mse-u`))  
lm_rain_results_elnino$id = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                              "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                              "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")
write_csv(lm_rain_results_elnino, path="results/stats_rain_elnino.csv")

#combine data frames into a single df for easy facet plotting
rainScatter_all_elnino <- bind_rows(rainScatter_list_elnino, .id="id")
rainScatter_all_elnino <- left_join(rainScatter_all_elnino, lm_rain_results_elnino, by="id") #joining summary stats
#reorder the factors so they plot in facetwrap the way I want
rainScatter_all_elnino$id <- factor(rainScatter_all_elnino$id, levels = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                                                                          "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                                                                          "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")) 
#plot
ggplot(data=rainScatter_all_elnino, aes(x=PRCP_station, y=PRCP_model, colour=id)) + 
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(alpha=0.9, size=1, fill=NA, shape=21) +
  stat_smooth(method = "lm", linetype="longdash", col="black", size=0.6)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Precip. (mm/month)")+
  ylab("Model Precip. (mm/month)")+
  theme(legend.position="none")+
  theme(strip.background =element_rect(fill="peachpuff2"))
  # geom_text(aes(label=paste("R2=", round(r.squared,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=1.2, size=3.5)+
  # geom_text(aes(label=paste("MAE=", round(mae,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=2.4, size=3.5)

# qqplots------

#South America
trmm_qq_SA_elnino <- QQ.Function_rain(trmm_elnino_SA) #function is in 'rain' script
chirps_qq_SA_elnino <- QQ.Function_rain(chirps_elnino_SA)
persiann_qq_SA_elnino <- QQ.Function_rain(persiann_elnino_SA)
#Africa
trmm_qq_A_elnino <- QQ.Function_rain(trmm_elnino_A)
chirps_qq_A_elnino <- QQ.Function_rain(chirps_elnino_A)
persiann_qq_A_elnino <- QQ.Function_rain(persiann_elnino_A)
#SE Asia
trmm_qq_SEA_elnino <- QQ.Function_rain(trmm_elnino_SEA)
chirps_qq_SEA_elnino <- QQ.Function_rain(chirps_elnino_SEA)
persiann_qq_SEA_elnino <- QQ.Function_rain(persiann_elnino_SEA)

#plot
qq_list_rain_elnino = list("TRMM Sth America"=trmm_qq_SA_elnino, "TRMM Africa"=trmm_qq_A_elnino, "TRMM SE Asia"=trmm_qq_SEA_elnino,
                    "CHIRPS Sth America"=chirps_qq_SA_elnino, "CHIRPS Africa"=chirps_qq_A_elnino, "CHIRPS SE Asia"=chirps_qq_SEA_elnino,
                    "PERSIANNCDR Sth America"=persiann_qq_SA_elnino, "PERSIANNCDR Africa"=persiann_qq_A_elnino, "PERSIANNCDR SE Asia"=persiann_qq_SEA_elnino)

#combine data frames into a single df for easy facet plotting
qq_all_rain_elnino <- bind_rows(qq_list_rain_elnino, .id="id")

#reorder the id factors so they plot in the facet wrap the way I want
qq_all_rain_elnino$id <- factor(qq_all_rain_elnino$id, levels = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                                                                   "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                                                                   "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")) 
qq_all_rain_elnino$percentile <- rep(c("1","25","50", "75", "90", "95", "99", "99.9"), 9)

#plot
ggplot(data=qq_all_rain_elnino, aes(x=station, y=model, label=percentile))+
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(colour="blue")+
  geom_line(colour="blue")+
  geom_text(hjust = -0.3, nudge_x = 0.5, size=3)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Precip. (mm/month)")+
  ylab("Model Precip. (mm/month)")+
  theme(strip.background =element_rect(fill="peachpuff2"))








































z = trmm_validation_SEA %>% 
  filter(DATE >= as.Date("1998-01-01"))
mean(z$PRCP_station, na.rm=T)
sd(z$PRCP_station, na.rm=T)










