
#automating qqplots

library(tidyverse)


#Function to define the percentiles you might want
QQ.Function_rain <- function(x){
  quants_station <- x %>% 
    summarise(`1%`=quantile(PRCP_station, probs=0.001, na.rm=T),
            `25%`=quantile(PRCP_station, probs=0.25, na.rm=T),
            `50%`=quantile(PRCP_station, probs=0.50, na.rm=T),
            `75%`=quantile(PRCP_station, probs=0.75, na.rm=T),
            `90%`=quantile(PRCP_station, probs=0.90, na.rm=T),
            `95%`=quantile(PRCP_station, probs=0.95, na.rm=T),
            `99%`=quantile(PRCP_station, probs=0.99, na.rm=T),
            `99.9%`=quantile(PRCP_station, probs=0.999, na.rm=T))
             # n=length(x$PRCP_station[!is.na(x$PRCP_station)]))
  quants_station <- quants_station %>% t() %>% as.data.frame()
  quants_model <- x %>% 
  summarise(`1%`=quantile(PRCP_model, probs=0.001, na.rm=T),
            `25%`=quantile(PRCP_model, probs=0.25, na.rm=T),
            `50%`=quantile(PRCP_model, probs=0.50, na.rm=T),
            `75%`=quantile(PRCP_model, probs=0.75, na.rm=T),
            `90%`=quantile(PRCP_model, probs=0.90, na.rm=T),
            `95%`=quantile(PRCP_model, probs=0.95, na.rm=T),
            `99%`=quantile(PRCP_model, probs=0.99, na.rm=T),
            `99.9%`=quantile(PRCP_model, probs=0.999, na.rm=T))
            # n=length(x$PRCP_model[!is.na(x$PRCP_model)]))
  quants_model <- quants_model %>% t() %>% as.data.frame()
  qq_df <- data.frame(quants_station$V1, quants_model$V1)
  colnames(qq_df) <- c("station", "model")
  rownames(qq_df) <- c("1%","25%","50%", "75%", "90%", "95%", "99%", "99.9%")
  qq_df
}

#apply the function to your dataframes
#South America
trmm_qq_SA <- QQ.Function_rain(trmm_validation_SA)
chirps_qq_SA <- QQ.Function_rain(chirps_validation_SA)
persiann_qq_SA <- QQ.Function_rain(persiann_validation_SA)

#Africa
trmm_qq_A <- QQ.Function_rain(trmm_validation_A)
chirps_qq_A <- QQ.Function_rain(chirps_validation_A)
persiann_qq_A <- QQ.Function_rain(persiann_validation_A)

#SE Asia
trmm_qq_SEA <- QQ.Function_rain(trmm_validation_SEA)
chirps_qq_SEA <- QQ.Function_rain(chirps_validation_SEA)
persiann_qq_SEA <- QQ.Function_rain(persiann_validation_SEA)

#plot
qq_list_rain = list("TRMM Sth America"=trmm_qq_SA, "TRMM Africa"=trmm_qq_A, "TRMM SE Asia"=trmm_qq_SEA,
                    "CHIRPS Sth America"=chirps_qq_SA, "CHIRPS Africa"=chirps_qq_A, "CHIRPS SE Asia"=chirps_qq_SEA,
                    "PERSIANNCDR Sth America"=persiann_qq_SA, "PERSIANNCDR Africa"=persiann_qq_A, "PERSIANNCDR SE Asia"=persiann_qq_SEA)

#combine data frames into a single df for easy facet plotting
qq_all_rain <- bind_rows(qq_list_rain, .id="id")

#add a percentiles column for the labels
qq_all_rain$percentile <- rep(c("1","25","50", "75", "90", "95", "99", "99.9"), 9)

#plot
ggplot(data=qq_all_rain, aes(x=station, y=model, label=percentile))+
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



