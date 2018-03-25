
library(raster); library(ncdf4); library(tidyverse); library(rasterVis); library(viridis); library(maptools);
library(RColorBrewer); library(animation); library(magick); library(scales)

#------BRING IN DATA-------------------------

#mean anomaly maps
a = "data/StemResp/StemResp_Pred_1995_2016_elnino_mean_anomaly.nc" 
StemResp_mean_anomaly = raster(a, varname="variable")

a = "data/StemNPP/StemNPP_Pred_1995_2016_elnino_mean_anomaly.nc" 
StemNPP_mean_anomaly = raster(a, varname="variable")

a = "data/TotNPP/TotNPP_Pred_1995_2016_elnino_mean_anomaly.nc" 
TotNPP_mean_anomaly = raster(a, varname="variable")

a = "data/TotSoilResp/TotSoilResp_Pred_1995_2016_elnino_mean_anomaly.nc" 
TotSoilResp_mean_anomaly = raster(a, varname="variable")

a = "data/NEE_wCanopyResp/NEE_wCanopyResp_Pred_1995_2016_elnino_mean_anomaly.nc" 
NEE_wCanopyResp_mean_anomaly = raster(a, varname="variable")

a = "data/NEE_wNoCanopyResp/NEE_wNoCanopyResp_Pred_1995_2016_elnino_mean_anomaly.nc" 
NEE_wNoCanopyResp_mean_anomaly = raster(a, varname="variable")

a = "data/NEE_fixedTSR1/NEE_fixedTSR1_monmeanCanopyResp_Pred_1995_2016_elnino_mean_anomaly.nc" 
NEE_fixedTSR_mean_anomaly = raster(a, varname="variable")

a= "data/MERRA2_elnino_SW_mean_anomaly_masked.nc"
sw_mean_anomaly = raster(a, varname = "SWGDN")

#timeseries
a = "data/StemResp/StemResp_Pred_1995_2016_elnino.nc" 
StemResp = stack(a, varname="variable")

a = "data/StemNPP/StemNPP_Pred_1995_2016_elnino.nc" 
StemNPP = stack(a, varname="variable")
plot(StemNPP)

a = "data/TotNPP/TotNPP_Pred_1995_2016_elnino.nc" 
TotNPP = stack(a, varname="variable")

a = "data/TotSoilResp/TotSoilResp_Pred_1995_2016_elnino.nc" 
TotSoilResp = stack(a, varname="variable")

a = "data/NEE_wCanopyResp/NEE_wCanopyResp_Pred_1995_2016_elnino_anomalies.nc" 
NEE_wCanopyResp_anomalies = stack(a, varname="variable")

a = "data/NEE_wNoCanopyResp/NEE_wNoCanopyResp_Pred_1995_2016_elnino_anomalies.nc" 
NEE_wNoCanopyResp_anomalies = stack(a, varname="variable")

a = "data/NEE_fixedTSR1/NEE_fixedTSR1_monmeanCanopyResp_Pred_1995_2016_elnino_anomalies.nc" 
NEE_fixedTSR_anomalies = stack(a, varname="variable")

a="data/erai_elnino_vpd_mean_anomaly_masked.nc"
vpd_mean_anomaly = raster(a, varname= "vpd")


#---plotting--------------------------------------

#add some of the  maps into a stack for plotting together
mean_anomaly_stack = stack(StemNPP_mean_anomaly, TotNPP_mean_anomaly, StemResp_mean_anomaly)
  mean_stack_names = c("Stem NPP", "Total NPP", "Stem Respiration")
  mean_anomaly_stack = setNames(mean_anomaly_stack, mean_stack_names)  

NEE_stack = stack(NEE_wCanopyResp_mean_anomaly, NEE_wNoCanopyResp_mean_anomaly)
  NEE_stack_names = c("NEE w/ Canopy Respiration", "NEE w/o Canopy Respiration")
  NEE_stack = setNames(NEE_stack, NEE_stack_names)
  
NEE_stack_fixedSR = stack(NEE_wCanopyResp_mean_anomaly, NEE_fixedTSR_mean_anomaly)
  NEE_fixed_names = c("NEE w/ Canopy Respiration", "NEE w/ Fixed Total Soil Resp.")
  NEE_stack_fixedSR = setNames(NEE_stack_fixedSR, NEE_fixed_names)  

levelplot(StemNPP_mean_anomaly)
levelplot(mean_anomaly_stack)  
levelplot(NEE_stack)  
levelplot(NEE_stack_fixedSR)  

#bring in world continents shapefile
  continents = readShapeSpatial("C:/Users/Chad/Desktop/internship/land_mask/data/world_continents.shp")
  crs(continents) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


levelplot(StemNPP_mean_anomaly, col.regions = brewer.pal(11, 'RdYlGn'), margin=F,
          at=c(-Inf, seq(-0.015, 0.015, 0.005), Inf), interpolate=TRUE, 
          colorkey=list(at=seq(-0.02, 0.02, 0.005),
          labels=c('<-0.02', seq(-0.015, 0.015, 0.005), '>0.02')),
           ylab = "",xlab="", main="Stem NPP Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(TotNPP_mean_anomaly, col.regions = brewer.pal(8, 'RdYlGn'), margin=F,
          at=c(-Inf, seq(-0.15, 0.15, 0.05)), 
          colorkey=list(at=seq(-0.2, 0.2, 0.05),
          labels=c('<-0.2', seq(-0.15, 0.15, 0.05), '>0.2')),
          ylab = "",xlab="", main="Total NPP Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(StemResp_mean_anomaly, col.regions = rev(brewer.pal(11, 'RdYlGn')), margin=F,
         at=c(-Inf, seq(-0.02, 0.02, 0.005), Inf), interpolate=TRUE, 
          colorkey=list(at=seq(-0.025, 0.025, 0.005),
          labels=c('<-0.025', seq(-0.02, 0.02, 0.005), '>0.025')),
          ylab = "",xlab="", main="Stem Respiration Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(TotSoilResp_mean_anomaly, col.regions = rev(brewer.pal(8, 'RdYlGn')), margin=F,
          at=c(-Inf, seq(-0.2, 0.2, 0.1), Inf), 
          colorkey=list(at=seq(-0.3, 0.3, 0.1),
          labels=c('<-0.3', seq(-0.2, 0.2, 0.1), '>0.3')),
          ylab = "",xlab="", main="Total Soil Respiration Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

p.strip <- list(cex=0.85, lines=1)
levelplot(NEE_stack, col.regions = rev(brewer.pal(8, 'RdYlGn')), margin=F,
          at=c(-Inf, seq(-0.3, 0.3, 0.1), Inf), 
          colorkey=list(at=seq(-0.4, 0.4, 0.1),
          labels=c('<-0.4', seq(-0.3, 0.3, 0.1), '>0.4')),
          ylab = "",xlab="", main="NEE Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          names.attr= NEE_stack_names, par.strip.text=p.strip,
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(NEE_stack_fixedSR, col.regions = rev(brewer.pal(10, 'RdYlGn')), margin=F,
          at=c(-Inf, seq(-0.2, 0.2, 0.05), Inf), 
          colorkey=list(at=seq(-0.25, 0.25, 0.05),
          labels=c('<-0.25', seq(-0.2, 0.2, 0.05), '>0.25')),
          ylab = "",xlab="", main="NEE Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          names.attr= NEE_fixed_names, par.strip.text=p.strip,
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(NEE_fixedTSR_mean_anomaly, col.regions = rev(brewer.pal(10, 'RdYlGn')), margin=F,
          at=c(-Inf, seq(-0.2, 0.2, 0.05), Inf), 
          colorkey=list(at=seq(-0.25, 0.25, 0.05),
          labels=c('<-0.25', seq(-0.2, 0.2, 0.05), '>0.25')),
          ylab = "",xlab="", main="NEE w/ fixed Total Soil Resp. Mean Anomaly (MgC/ha/month), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(vpd_mean_anomaly, col.regions = rev(brewer.pal(10, 'RdBu')), margin=F,
          at=c(-Inf, seq(-0.2, 0.2, 0.05), Inf), 
          colorkey=list(at=seq(-0.25, 0.25, 0.05),
          labels=c('<-0.25', seq(-0.2, 0.2, 0.05), '>0.25')),
          ylab = "",xlab="", main="ERA-Interim Mean VPD Anomaly (kPa), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(sw_mean_anomaly, col.regions = rev(brewer.pal(8, 'RdBu')), margin=F,
          at=c(-Inf, seq(-30, 30, 10), Inf), 
          colorkey=list(at=seq(-40, 40, 10),
          labels=c('<-40', seq(-30, 30, 10), '>40')),
          ylab = "",xlab="", main="MERRA2 Mean Incident Short Wave Anomaly (W/m2), Jan. '15 - May '16",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))



#----timeseries for Yadvinder--------------------------------------

#add data
a = "data/StemResp/StemResp_Pred_1995_2016_elnino_anomalies.nc" 
StemResp_anomalies = stack(a, varname="variable")

a = "data/StemNPP/StemNPP_Pred_1995_2016_elnino_anomalies.nc" 
StemNPP_anomalies = stack(a, varname="variable")

a = "data/TotNPP/TotNPP_Pred_1995_2016_elnino_anomalies.nc" 
TotNPP_anomalies = stack(a, varname="variable")

a = "data/TotSoilResp/TotSoilResp_Pred_1995_2016_elnino_anomalies.nc" 
TotSoilResp_anomalies = stack(a, varname="variable")

a= "data/erai_elnino_vpd_anom.nc"
erai_vpd_anom = stack(a, varname="vpd")

dates <- seq(as.Date("2015/1/15"), by = "month", length.out = 17)

extraction_sites = read_csv("data/extraction.csv")

# .csv to R SpatialPointsDataFrame
projection = crs(StemResp_mean_anomaly)
extraction_sites <- SpatialPointsDataFrame(extraction_sites[,2:1], extraction_sites,proj4string = projection)   

#extract
ts_soilresp = raster::extract(TotSoilResp_anomalies,extraction_sites, method = 'bilinear',df = TRUE)           
ts_soilresp = ts_soilresp %>% t() %>% data.frame()
colnames(ts_soilresp)="TotSoilResp"
ts_soilresp = ts_soilresp[-1,]                                        
ts_soilresp = data.frame(date = dates, ts_soilresp)

ts_TotNPP = raster::extract(TotNPP_anomalies,extraction_sites,method = 'bilinear',df = TRUE)           
ts_TotNPP = ts_TotNPP %>% t() %>% data.frame()
colnames(ts_TotNPP)="TotNPP"
ts_TotNPP = ts_TotNPP[-1,]                                        
ts_TotNPP = data.frame(date = dates, ts_TotNPP)

ts_NEE = raster::extract(TotNPP_anomalies,extraction_sites, method = 'bilinear',df = TRUE)           
ts_TotNPP = ts_TotNPP %>% t() %>% data.frame()
colnames(ts_TotNPP)="TotNPP"
ts_TotNPP = ts_TotNPP[-1,]                                        
ts_TotNPP = data.frame(date = dates, ts_TotNPP)

ts_NEE = raster::extract(NEE_wCanopyResp_anomalies, extraction_sites, method = 'bilinear',df = TRUE)           
ts_NEE = ts_NEE %>% t() %>% data.frame()
ts_NEE = ts_NEE[-1,]                                        
ts_NEE = data.frame(date = dates, ts_NEE)
colnames(ts_NEE) = c("date", "NEE")

vpd = raster::extract(erai_vpd_anom,  extraction_sites, method = 'bilinear', df = TRUE)           
vpd = vpd %>% t() %>% data.frame()
vpd = vpd[-1,]                                        
vpd = data.frame(date = dates, vpd)
colnames(vpd) = c("date", "vpd")

ts_anomalies = dplyr::left_join(ts_soilresp, ts_TotNPP, by ="date")
ts_anomalies = dplyr::left_join(ts_anomalies, ts_NEE, by="date")
ts_anomalies = dplyr::left_join(ts_anomalies, vpd, by="date")

#plot
ggplot(data = ts_anomalies, aes(x = date)) +
  geom_line(aes(y=TotSoilResp, colour = "TotSoilResp"), size = 1.0) +
  geom_line(aes(y=TotNPP, colour = "TotNPP"), size = 1.0) +
  geom_line(aes(y=NEE, colour = "NEE"), size = 1.0) +
  geom_line(aes(y=vpd, colour = "VPD"), size = 1.0) +
  geom_hline(yintercept = 0.0, linetype = "dotted") +
  ylab("Anomaly (MgC/ha)")+
  theme(legend.title=element_blank()) +
  ggtitle("Model Anomalies - Northeast Amazonia Region")
  
  # scale_color_manual(values=c("red","darkorchid3", "royalblue2")) +
  # scale_y_continuous("Temp. Anomaly (°C)", limits = c(-1.0 , 2.25), minor_breaks = seq(-1.0, 2.25, 0.25), breaks = seq(-0.5,2.0, 0.5)) +
  # scale_x_date(breaks = pretty_breaks(10)) +
  # xlab("Date")+
  # theme_bw() +
  # theme(legend.title=element_blank()) +
  # theme(legend.position="bottom", legend.box = "vertical")+
  # geom_hline(yintercept = 0.0, linetype = "dotted")




#attempting to animate-----(failed due to the finickiness of IM convert, works in command line though-----

# COMMANDLINE:  convert *.png -set delay 50 -loop 0 your.gif

# names=format(seq(as.Date('2015/01/1'), as.Date('2016/05/1'), by='month'), '%Y%m')
# for (i in 1:17) {
#           png(file=paste(names[i],"png",sep="."))        
#           rasterVis::levelplot(NEE_wCanopyResp_anomalies[1], col.regions = rev(brewer.pal(8, 'RdYlGn')), margin=F,
#           at=c(-Inf, seq(-0.3, 0.3, 0.1), Inf), 
#           colorkey=list(at=seq(-0.4, 0.4, 0.1),
#           labels=c('<-0.4', seq(-0.3, 0.3, 0.1), '>0.4')),
#           ylab = "",xlab="", main="NEE Anomaly (MgC/ha/month), Jan. '15 - May '16",
#           par.strip.text=p.strip,
#           pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
#           layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))  
#           dev.off()
# }

# extent_tropics <- as(raster::extent(-105.0, 180.0, 23.0, -23.0), "SpatialPolygons")
# proj4string(extent_tropics) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# continents = crop(continents, extent_tropics)
# 
# names=format(seq(as.Date('2015/01/1'), as.Date('2016/05/1'), by='month'), '%Y%m')
# for (i in 1:17) {
#           png(file=paste(names[i],"png",sep="."))        
#           plot(continents, col="gray70")
#           plot(NEE_wCanopyResp_anomalies[[i]], col = rev(brewer.pal(8, 'RdYlGn')), main=names[i],
#           breaks = c(-1.1, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 1.1), add=TRUE, legend=F)
#           dev.off()
# }
# 
# 
# path.to.convert <- shQuote("C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe") 
# 
# saveGIF(for (i in 1:length(NEE_wCanopyResp_anomalies)) {
#           levelplot(NEE_wCanopyResp_anomalies[i], col.regions = rev(brewer.pal(8, 'RdYlGn')), margin=F,
#           at=c(-Inf, seq(-0.3, 0.3, 0.1), Inf), 
#           colorkey=list(at=seq(-0.4, 0.4, 0.1),
#           labels=c('<-0.4', seq(-0.3, 0.3, 0.1), '>0.4')),
#           ylab = "",xlab="", main="NEE Anomaly (MgC/ha/month), Jan. '15 - May '16",
#           par.strip.text=p.strip,
#           pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
#           layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))         
# 
#       }, 
#     movie.name = 'NEE_wCanopyResp_anomalies.gif', img.name="NEE Mean Anomaly (MgC/ha/month)",
#     cmd.fun = system, 
#     convert="convert",
#     ani.options(convert=path.to.convert),
#     interval = 0.1,
#     outdir = getwd())
# 
# 
# saveGIF(for (i in 1:17) {
#                       plot(NEE_wCanopyResp_anomalies[[i]],
#                       main="ERA-Interim")
#       }, 
#     movie.name = 'tmp.gif', img.name="erai",
#     cmd.fun=system, 
#     convert="convert",
#     ani.options(convert=path.to.convert),
#     interval = 0.1,
#     outdir = getwd())
# 
# 
# saveGIF(animate(NEE_wCanopyResp_anomalies), convert="convert",
#         ani.options(convert=path.to.convert), cmd.fun=system,
#         movie.name = "tmep.gif", img.name="erai", outdir = getwd())  
# 
# animate(NEE_wCanopyResp_anomalies)
