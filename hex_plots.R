#hex plots?



ggplot(data=tempScatter_all, aes(x=TAVG_station, y=TAVG_model, colour=id)) + 
  facet_wrap(~id, scale="free", ncol=3)+
  geom_hex(bins = 15, show.legend = T)+
  stat_smooth(method = "lm", linetype="longdash", col="black", size=1)+
  geom_abline(intercept = 0,slope=1, col="black", size=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Temp. (°C)")+
  ylab("Model Temp. (°C)")+
  theme(legend.position="none")+
  theme(strip.background =element_rect(fill="peachpuff2"))

  
  # 
  # geom_text(aes(label=paste("R2=", round(r.squared,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=1.2, size=3.5)+
  # geom_text(aes(label=paste("MAE=", round(mae,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=2.4, size=3.5)+
  # geom_text(aes(label=paste("RMSE=", round(rmse,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=3.6, size=3.5)
