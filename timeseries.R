
#creating timeseries of the anomalies (need to redo this script with everything put into functions)

library(tidyverse); library(pracma); library(caTools);
library(lubridate); library(scales); library(zoo)

a = "data/anomaly_timeseries/CMAP_2014_2016_anomalies_masked.nc"
cmap_2014_16_tropics = stack(a, varname="precip")
cmap_2014_16_tropics = rotate(cmap_2014_16_tropics)


#remove Inf and -Inf values from the rasters (they stuff up cellstats)  
  erai_2014_16_tropics = reclassify(erai_2014_16_tropics, cbind(Inf, NA))
  erai_2014_16_tropics = reclassify(erai_2014_16_tropics, cbind(-Inf, NA))
  merra2_2014_16_tropics = reclassify(merra2_2014_16_tropics, cbind(Inf, NA))
  merra2_2014_16_tropics = reclassify(merra2_2014_16_tropics, cbind(-Inf, NA))
  jra55_2014_16_tropics = reclassify(jra55_2014_16_tropics, cbind(Inf, NA))
  jra55_2014_16_tropics = reclassify(jra55_2014_16_tropics, cbind(-Inf, NA))
  cfsr_2014_16_tropics = reclassify(cfsr_2014_16_tropics, cbind(Inf, NA))
  cfsr_2014_16_tropics = reclassify(cfsr_2014_16_tropics, cbind(-Inf, NA))
  trmm_2014_16_tropics = reclassify(trmm_2014_16_tropics, cbind(Inf, NA))
  trmm_2014_16_tropics = reclassify(trmm_2014_16_tropics, cbind(-Inf, NA))
  chirps_2014_16_tropics = reclassify(chirps_2014_16_tropics, cbind(Inf, NA))
  chirps_2014_16_tropics = reclassify(chirps_2014_16_tropics, cbind(-Inf, NA))
  persiann_2014_16_tropics = reclassify(persiann_2014_16_tropics, cbind(Inf, NA))
  persiann_2014_16_tropics = reclassify(persiann_2014_16_tropics, cbind(-Inf, NA))

#extract regions from the anomaly timeseries
  #creating bounding boxes
  #SE Asia
  extent_SEA <- as(raster::extent(68.0, 177.0, -21.0, 21.0), "SpatialPolygons")
  proj4string(extent_SEA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #Africa
  extent_A <- as(raster::extent(-25.0, 60.0, -21.0, 21.0), "SpatialPolygons")
  proj4string(extent_A) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #sth America
  extent_SA <- as(raster::extent(-105.0, -28.0, -21.0, 21.0), "SpatialPolygons")
  proj4string(extent_SA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#extract regions from pantropics file using bounding boxes
  #Sth America
  erai_2014_16_SA = crop(erai_2014_16_tropics, extent_SA)
  merra2_2014_16_SA = crop(merra2_2014_16_tropics, extent_SA)
  jra55_2014_16_SA = crop(jra55_2014_16_tropics, extent_SA)
  cfsr_2014_16_SA = crop(cfsr_2014_16_tropics, extent_SA)
  trmm_2014_16_SA = crop(trmm_2014_16_tropics, extent_SA)
  chirps_2014_16_SA = crop(chirps_2014_16_tropics, extent_SA)
  cmap_2014_16_SA = crop(cmap_2014_16_tropics, extent_SA)
  persiann_2014_16_SA = crop(persiann_2014_16_tropics, extent_SA)

  #Africa
  erai_2014_16_A = crop(erai_2014_16_tropics, extent_A)
  merra2_2014_16_A = crop(merra2_2014_16_tropics, extent_A)
  jra55_2014_16_A = crop(jra55_2014_16_tropics, extent_A)
  cfsr_2014_16_A = crop(cfsr_2014_16_tropics, extent_A)
  trmm_2014_16_A = crop(trmm_2014_16_tropics, extent_A)
  chirps_2014_16_A = crop(chirps_2014_16_tropics, extent_A)
  cmap_2014_16_A = crop(cmap_2014_16_tropics, extent_A)
  persiann_2014_16_A = crop(persiann_2014_16_tropics, extent_A)

  #SE Asia
  erai_2014_16_SEA = crop(erai_2014_16_tropics, extent_SEA)
  merra2_2014_16_SEA = crop(merra2_2014_16_tropics, extent_SEA)
  jra55_2014_16_SEA = crop(jra55_2014_16_tropics, extent_SEA)
  cfsr_2014_16_SEA = crop(cfsr_2014_16_tropics, extent_SEA)
  trmm_2014_16_SEA = crop(trmm_2014_16_tropics, extent_SEA)
  chirps_2014_16_SEA = crop(chirps_2014_16_tropics, extent_SEA)
  cmap_2014_16_SEA = crop(cmap_2014_16_tropics, extent_SEA)
  persiann_2014_16_SEA = crop(persiann_2014_16_tropics, extent_SEA)
  
#zonal stats
  #S America
  erai_ts_SA = cellStats(erai_2014_16_SA, stat='mean', na.rm=TRUE) 
  merra2_ts_SA = cellStats(merra2_2014_16_SA, stat='mean', na.rm=TRUE) 
  jra55_ts_SA = cellStats(jra55_2014_16_SA, stat='mean', na.rm=TRUE) 
  cfsr_ts_SA = cellStats(cfsr_2014_16_SA, stat='mean', na.rm=TRUE) 
  trmm_ts_SA = cellStats(trmm_2014_16_SA, stat='mean', na.rm=TRUE) 
  chirps_ts_SA = cellStats(chirps_2014_16_SA, stat='mean', na.rm=TRUE) 
  cmap_ts_SA = cellStats(cmap_2014_16_SA, stat='mean', na.rm=TRUE)
  persiann_ts_SA = cellStats(persiann_2014_16_SA, stat='mean', na.rm=TRUE)
  
  #Africa
  erai_ts_A = cellStats(erai_2014_16_A, stat='mean', na.rm=TRUE) 
  merra2_ts_A = cellStats(merra2_2014_16_A, stat='mean', na.rm=TRUE) 
  jra55_ts_A = cellStats(jra55_2014_16_A, stat='mean', na.rm=TRUE) 
  cfsr_ts_A = cellStats(cfsr_2014_16_A, stat='mean', na.rm=TRUE) 
  trmm_ts_A = cellStats(trmm_2014_16_A, stat='mean', na.rm=TRUE) 
  chirps_ts_A = cellStats(chirps_2014_16_A, stat='mean', na.rm=TRUE)
  cmap_ts_A = cellStats(cmap_2014_16_A, stat='mean', na.rm=TRUE)
  persiann_ts_A = cellStats(persiann_2014_16_A, stat='mean', na.rm=TRUE)
  
  #SE Asia
  erai_ts_SEA = cellStats(erai_2014_16_SEA, stat='mean', na.rm=TRUE) 
  merra2_ts_SEA = cellStats(merra2_2014_16_SEA, stat='mean', na.rm=TRUE) 
  jra55_ts_SEA = cellStats(jra55_2014_16_SEA, stat='mean', na.rm=TRUE) 
  cfsr_ts_SEA = cellStats(cfsr_2014_16_SEA, stat='mean', na.rm=TRUE) 
  trmm_ts_SEA = cellStats(trmm_2014_16_SEA, stat='mean', na.rm=TRUE) 
  chirps_ts_SEA = cellStats(chirps_2014_16_SEA, stat='mean', na.rm=TRUE)
  cmap_ts_SEA = cellStats(cmap_2014_16_SEA, stat='mean', na.rm=TRUE)
  persiann_ts_SEA = cellStats(persiann_2014_16_SEA, stat='mean', na.rm=TRUE)
  
#add timeseries to a dataframe for easy plotting
  date = seq(as.Date("2014-01-01"), as.Date("2016-12-01"), by = "month")
  
  tmean_anomalies_SA = data.frame(date, erai_ts_SA, merra2_ts_SA, jra55_ts_SA, cfsr_ts_SA)
  tmean_anomalies_A = data.frame(date, erai_ts_A, merra2_ts_A, jra55_ts_A, cfsr_ts_A)
  tmean_anomalies_SEA = data.frame(date, erai_ts_SEA, merra2_ts_SEA, jra55_ts_SEA, cfsr_ts_SEA)
  
  colnames(tmean_anomalies_SA) = c("date","ERAI", "MERRA2", "JRA55", "CFSR") 
  colnames(tmean_anomalies_A) = c("date","ERAI", "MERRA2", "JRA55", "CFSR") 
  colnames(tmean_anomalies_SEA) = c("date","ERAI", "MERRA2", "JRA55", "CFSR") 
  
  Pmean_anomalies_SA = data.frame(date, trmm_ts_SA, chirps_ts_SA, cmap_ts_SA, persiann_ts_SA)
  Pmean_anomalies_A = data.frame(date, trmm_ts_A, chirps_ts_A, cmap_ts_A, persiann_ts_A)
  Pmean_anomalies_SEA = data.frame(date, trmm_ts_SEA, chirps_ts_SEA, cmap_ts_SEA, persiann_ts_SEA)
  
  colnames(Pmean_anomalies_SA) = c("date","TRMM", "CHIRPS", "CMAP", "PERSIANNCDR") 
  colnames(Pmean_anomalies_A) = c("date","TRMM", "CHIRPS", "CMAP", "PERSIANNCDR") 
  colnames(Pmean_anomalies_SEA) = c("date","TRMM", "CHIRPS", "CMAP", "PERSIANNCDR") 

#plot
#temp
p1 = ggplot(data = tmean_anomalies_SA, aes(x = date)) +
  geom_line(aes(y=tmean_anomalies_SA$ERAI, colour = "ERAI"), size = 0.75)+
  geom_line(aes(y=tmean_anomalies_SA$MERRA2, colour = "MERRA2"), size = 0.75) +
  geom_line(aes(y=tmean_anomalies_SA$JRA55, colour = "JRA55"), size = 0.75) +
  geom_line(aes(y=tmean_anomalies_SA$CFSR, colour = "CFSR"), size = 0.75) +
  scale_color_manual(values=c("red","darkorchid3", "royalblue2", "springgreen3")) +
  scale_y_continuous("Temp. Anomaly (°C)", limits = c(-1.5, 2.25), minor_breaks = seq(-1.5, 2.25, 0.25), breaks = seq(-1.5,2.0, 0.5)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

p2 = ggplot(data = tmean_anomalies_A, aes(x = date)) +
  geom_line(aes(y=tmean_anomalies_A$ERAI, colour = "ERAI"), size = 0.75)+
  geom_line(aes(y=tmean_anomalies_A$MERRA2, colour = "MERRA2"), size = 0.75) +
  geom_line(aes(y=tmean_anomalies_A$JRA55, colour = "JRA55"), size = 0.75) +
  geom_line(aes(y=tmean_anomalies_A$CFSR, colour = "CFSR"), size = 0.75) +
  scale_color_manual(values=c("red","darkorchid3", "royalblue2", "springgreen3")) +
  scale_y_continuous("Temp. Anomaly (°C)", limits = c(-1.5, 2.25), minor_breaks = seq(-1.5, 2.25, 0.25), breaks = seq(-1.5,2.0, 0.5)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

p3 = ggplot(data = tmean_anomalies_SEA, aes(x = date)) +
  geom_line(aes(y=ERAI, colour = "ERAI"), size = 0.75)+
  geom_line(aes(y=MERRA2, colour = "MERRA2"), size = 0.75) +
  geom_line(aes(y=JRA55, colour = "JRA55"), size = 0.75) +
  geom_line(aes(y=CFSR, colour = "CFSR"), size = 0.75) +
  scale_color_manual(values=c("red","darkorchid3", "royalblue2", "springgreen3")) +
  scale_y_continuous("Temp. Anomaly (°C)", limits = c(-1.5, 2.25), minor_breaks = seq(-1.5, 2.25, 0.25), breaks = seq(-1.5,2.0, 0.5)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

multiplot(p1, p2, p3, cols=1)

#rainfall
p4 = ggplot(data = Pmean_anomalies_SA, aes(x = date)) +
  geom_line(aes(y=CHIRPS, colour = "CHIRPS"), size = 0.75)+
  geom_line(aes(y=TRMM, colour = "TRMM"), size = 0.75) +
  geom_line(aes(y=CMAP, colour = "CMAP"), size = 0.75) +
  geom_line(aes(y=PERSIANNCDR, colour = "PERSIANN-CDR"), size = 0.75) +
  scale_y_continuous("Precip. Anomaly (mm/month)", limits = c(-160, 75), 
                     minor_breaks = seq(-150, 75, 25),
                     breaks = seq(-150,50, 50)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.9, 0.2), legend.box = "vertical")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

p5 = ggplot(data = Pmean_anomalies_A, aes(x = date)) +
  geom_line(aes(y=CHIRPS, colour = "CHIRPS"), size = 0.75)+
  geom_line(aes(y=TRMM, colour = "TRMM"), size = 0.75) +
  geom_line(aes(y=CMAP, colour = "CMAP"), size = 0.75) +
  geom_line(aes(y=PERSIANNCDR, colour = "PERSIANN-CDR"), size = 0.75) +
  scale_y_continuous("Precip. Anomaly (mm/month)", limits = c(-160, 75), 
                     minor_breaks = seq(-150, 75, 25),
                     breaks = seq(-150,50, 50)) + 
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none", legend.box = "vertical")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

p6 = ggplot(data = Pmean_anomalies_SEA, aes(x = date)) +
  geom_line(aes(y=CHIRPS, colour = "CHIRPS"), size = 0.75)+
  geom_line(aes(y=TRMM, colour = "TRMM"), size = 0.75) +
  geom_line(aes(y=CMAP, colour = "CMAP"), size = 0.75) +
  geom_line(aes(y=PERSIANNCDR, colour = "PERSIANN-CDR"), size = 0.75) +
  scale_y_continuous("Precip. Anomaly (mm/month)", limits = c(-160, 75), 
                     minor_breaks = seq(-150, 75, 25),
                     breaks = seq(-150,50, 50)) + 
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Year")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none", legend.box = "vertical")+
  geom_hline(yintercept = 0.0, linetype = "dotted")

multiplot(p4, p5, p6, cols=1)



 





































