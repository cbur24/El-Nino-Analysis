library(raster); library(ncdf4); library(tidyverse); library(rasterVis); library(viridis); library(maptools);
library(RColorBrewer); library(animation)

#bring in netcdf files----------------------------------------------------
#ERAI
a = "data/ERAI_elnino_max_anomaly.nc"
erai_max = raster(a, varname = "t2m")
erai_max = rotate(erai_max)
a = "data/ERAI_elnino_mean_anomaly.nc"
erai_mean = raster(a, varname = "t2m")
erai_mean = rotate(erai_mean)

#MERRA2
a = "data/MERRA2_elnino_max_anomaly.nc"
merra2_max = raster(a, varname = "T2MMEAN")
a = "data/MERRA2_elnino_mean_anomaly.nc"
merra2_mean = raster(a, varname = "T2MMEAN")

#CRU
a = "data/CRU_elnino_max_anomaly.nc"
cru_max = raster(a, varname = "tmp")
a = "data/CRU_elnino_mean_anomaly.nc"
cru_mean = raster(a, varname = "tmp")

#JRA55
a = "data/jra55_elnino_max_anomaly.nc"
jra55_max = raster(a, varname = "TMP_GDS4_HTGL")
crs(jra55_max) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
a = "data/jra55_elnino_mean_anomaly.nc"
jra55_mean = raster(a, varname = "TMP_GDS4_HTGL")
crs(jra55_mean) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#NCEP CFSR
a = "data/cfsr_elnino_max_anomaly.nc"
cfsr_max = raster(a, varname = "TMP_L103")
a = "data/cfsr_elnino_mean_anomaly.nc"
cfsr_mean = raster(a, varname = "TMP_L103")

#TRMM
a = "data/TRMM_elnino_min_anomaly.nc"
trmm_max = raster(a, varname = "precipitation")
crs(trmm_max) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
trmm_max = t(trmm_max)
trmm_max = flip(trmm_max, direction = 1)
trmm_max = flip(trmm_max, direction = 2)

a = "data/TRMM_elnino_mean_anomaly.nc"
trmm_mean = raster(a, varname = "precipitation")
crs(trmm_mean) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
trmm_mean = t(trmm_mean)
trmm_mean = flip(trmm_mean, direction = 1)
trmm_mean = flip(trmm_mean, direction = 2)

#TRMM CWD (this is the anomaly from 1998-2016)
a = "data/TRMM_CWD_elnino_min_anomaly.nc"
trmm_CWD_max = raster(a, varname="CWD")
a= "data/TRMM_CWD_elnino_mean_anomaly.nc"
trmm_CWD_mean = raster(a, varname="CWD")

#chirps
a = "data/chirps_elnino_min_anomaly.nc"
chirps_max = raster(a, varname = "precip")
a = "data/chirps_elnino_mean_anomaly.nc"
chirps_mean = raster(a, varname = "precip")

#CHIRPS CWD (this anomaly is from 1981-2016)
a = "data/chirps_CWD_1981_2016_elnino_min_anomaly.nc"
chirps_CWD_max = raster(a, varname="CWD")
a= "data/chirps_CWD_1981_2016_elnino_mean_anomaly.nc"
chirps_CWD_mean = raster(a, varname="CWD")

#cmap (these datasets have already been masked in CDO, getting better result in CDO)
a = "data/CMAP_elnino_min_anomaly.nc"
cmap_max = raster(a, varname = "precip")
cmap_max = rotate(cmap_max)
a = "data/CMAP_elnino_mean_anomaly.nc"
cmap_mean = raster(a, varname = "precip")
cmap_mean = rotate(cmap_mean)

#persiannCDR
a = "data/persiann_elnino_mean_anomaly.nc"
persiann_mean = raster(a, varname="precip")
a= "data/persiann_elnino_min_anomaly.nc"
persiann_max = raster(a, varname="precip")

#forest masks
a = "data/forestmask_05.nc"
forest_mask_05 = raster(a, varname = "tropical_forest_mask")
a = "data/forestmask_025.nc"
forest_mask_025 = raster(a, varname = "tropical_forest_mask")

#--------raster manipulation--------------------------------------------------------------

#remap all plots to the 0.5 CRU grid and combine plots into a stack (cfsr is already at 0.5grid)
  #temp  
  erai_max = resample(erai_max, cru_max, method = "bilinear")
  merra2_max = resample(merra2_max, cru_max, method = "bilinear")
  jra55_max = resample(jra55_max, cru_max, method = "bilinear")
  erai_mean = resample(erai_mean, cru_mean, method = "bilinear")
  merra2_mean = resample(merra2_mean, cru_mean, method = "bilinear")
  jra55_mean = resample(jra55_mean, cru_mean, method = "bilinear")
 
  #rainfall (remapping chirps to match trmm)
  chirps_max = resample(chirps_max, trmm_max, method = "bilinear")
  chirps_mean = resample(chirps_mean, trmm_mean, method ="bilinear")
  
  #CWD
  chirps_CWD_max = resample(chirps_CWD_max, trmm_max, method = "bilinear")
  chirps_CWD_mean = resample(chirps_CWD_mean, trmm_max, method = "bilinear")
  
  #create differnce maps
  difference_p_max = (chirps_max - trmm_max)
  difference_p_mean = abs(chirps_mean - trmm_mean)
  
#extract tropics extent from rasters
  #creating bounding boxes (might not use them though)
  #pantropics
  extent_tropics <- as(raster::extent(-105.0, 180.0, 21.0, -21.0), "SpatialPolygons")
  proj4string(extent_tropics) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  #crop to tropics extent (should try to do this with lapply or calc)
  erai_max_tropics =crop(erai_max, extent_tropics)
  erai_mean_tropics =crop(erai_mean, extent_tropics)
  merra2_max_tropics =crop(merra2_max, extent_tropics)
  merra2_mean_tropics =crop(merra2_mean, extent_tropics)
  jra55_max_tropics =crop(jra55_max, extent_tropics)
  jra55_mean_tropics =crop(jra55_mean, extent_tropics)
  cfsr_max_tropics =crop(cfsr_max, extent_tropics)
  cfsr_mean_tropics =crop(cfsr_mean, extent_tropics)
  cru_max_tropics =crop(cru_max, extent_tropics)
  cru_mean_tropics =crop(cru_mean, extent_tropics)
  
  chirps_max_tropics = crop(chirps_max, extent_tropics)
  chirps_mean_tropics = crop(chirps_mean, extent_tropics)
  trmm_max_tropics = crop(trmm_max, extent_tropics)
  trmm_mean_tropics = crop(trmm_mean, extent_tropics)
  persiann_mean_tropics = crop(persiann_mean, extent_tropics)
  persiann_max_tropics = crop(persiann_max, extent_tropics)
  cmap_max_tropics = crop(cmap_max, extent_tropics)
  cmap_mean_tropics = crop(cmap_mean, extent_tropics)
  difference_p_max = crop(difference_p_max, extent_tropics)
  difference_p_mean = crop(difference_p_mean, extent_tropics)
  chirps_CWD_max_tropics = crop(chirps_CWD_max, extent_tropics)
  chirps_CWD_mean_tropics = crop(chirps_CWD_mean, extent_tropics)
  
  forest_mask_05 = crop(forest_mask_05, extent_tropics)
  forest_mask_025 = crop(forest_mask_025, extent_tropics)

#add Temp rasters to stack for easy plotting  
maxt2m_stack = stack(erai_max_tropics, merra2_max_tropics, jra55_max_tropics, cfsr_max_tropics)
  temp_stack_names = c("ERA Interim", "MERRA2", "JRA55", "CFSR")
  maxt2m_stack = setNames(maxt2m_stack, temp_stack_names)  
  
meant2m_stack = stack(erai_mean_tropics, merra2_mean_tropics, jra55_mean_tropics, cfsr_mean_tropics)
  temp_stack_names = c("ERA Interim", "MERRA2", "JRA55", "CFSR")
  meant2m_stack = setNames(meant2m_stack, temp_stack_names)    
  
#mask rasters to forest extent (try to do with lapply)
  maxt2m_stack = maxt2m_stack/forest_mask_05
  meant2m_stack = meant2m_stack/forest_mask_05
  chirps_max_tropics = chirps_max_tropics/forest_mask_025
  chirps_mean_tropics = chirps_mean_tropics/forest_mask_025
  trmm_max_tropics =  trmm_max_tropics/forest_mask_025 
  trmm_mean_tropics = trmm_mean_tropics/forest_mask_025
  persiann_max_tropics = persiann_max_tropics/forest_mask_025
  persiann_mean_tropics = persiann_mean_tropics/forest_mask_025
  difference_p_max = difference_p_max/forest_mask_025
  difference_p_mean = difference_p_mean/forest_mask_025
  chirps_CWD_max_tropics = chirps_CWD_max_tropics/forest_mask_025
  chirps_CWD_mean_tropics = chirps_CWD_mean_tropics/forest_mask_025

    
#------plotting anomalies-----------------------------------------------------

#bring in world continents shapefile
  continents = readShapeSpatial("C:/Users/Chad/Desktop/internship/land_mask/data/world_continents.shp")
  crs(continents) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#temp----------------(reference period 1980-2016)----------------------------------  
  p.strip <- list(cex=0.85, lines=1)
  #max anomalies
  levelplot(maxt2m_stack, col.regions = colorRampPalette(c("white","orange", "red", "black"))(12),
         at=c(-Inf, seq(0.0, 5.0, 0.5), Inf), 
          colorkey=list(at=seq(0.0, 5.5, 0.5),
                    labels=c(seq(0.0, 5.0,0.5), '> 5.5')), names.attr= temp_stack_names, par.strip.text=p.strip,
          ylab = "", xlab="", pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
 
  #mean anomalies
  levelplot(meant2m_stack, col.regions = rev(brewer.pal(10, 'RdBu'))[4:10], 
          at=c(-Inf, seq(-0.5, 2.0, 0.5), Inf), 
          colorkey=list(at=seq(-1.0, 2.5, 0.5),
          labels=c('< -1.0', seq(-0.5, 2.0,0.5), '> 2.5')), names.attr= temp_stack_names, par.strip.text=p.strip,
          ylab = "", xlab="", pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
   levelplot(meant2m_stack[[1]], col.regions = rev(brewer.pal(10, 'RdBu'))[4:10], 
          at=c(-Inf, seq(-0.5, 2.0, 0.5), Inf), 
          colorkey=list(at=seq(-1.0, 2.5, 0.5), 
          labels=c('< -1.0', seq(-0.5, 2.0,0.5), '> 2.5')), main = "ERA-Interim Mean T2M Anomaly Jan '15 - May '16",
          ylab = "", xlab="", pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
          
#rain--------------(reference period 1998-2016)------------------------------------- 
#max anomalies (export each plot and add them together in adobe illustrator)
levelplot(chirps_max_tropics, col.regions = rev(brewer.pal(7, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50)), 
          colorkey=list(at=seq(-300, 0, 50),
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
levelplot(trmm_max_tropics, col.regions = rev(brewer.pal(7, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50)), 
          colorkey=list(at=seq(-300, 0, 50),
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
 
levelplot(cmap_max_tropics, col.regions = rev(brewer.pal(7, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50)), 
          colorkey=list(at=seq(-300, 0, 50),
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3")))+
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
 
levelplot(persiann_max_tropics, col.regions = rev(brewer.pal(7, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50)), 
          colorkey=list(at=seq(-300, 0, 50),
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3")))+
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
levelplot(difference_p_max, col.regions = rev(brewer.pal(8, 'RdBu')), margin=F,
          at=c(-Inf, seq(-75, 75, 25), Inf), interpolate=TRUE, 
          colorkey=list(at=seq(-100, 100, 25),
          labels=c('<-100', seq(-75, 75, 25), '>100')), ylab = "",xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  

#mean anomalies (export each plot and add them together in adobe illustrator)
levelplot(chirps_mean_tropics, col.regions = colorRampPalette(brewer.pal(8, 'BrBG'))(10), margin=F,
          at=c(-Inf, seq(-75, 75, 25), Inf), 
          colorkey=list(at=seq(-100, 100, 25),
          labels=c('<-100', seq(-75,75, 25), '>100')), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
levelplot(trmm_mean_tropics, col.regions = colorRampPalette(brewer.pal(8, 'BrBG'))(10), margin=F,
          at=c(-Inf, seq(-75, 75, 25), Inf), 
          colorkey=list(at=seq(-100, 100, 25),
          labels=c('<-100', seq(-75,75, 25), '>100')), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
levelplot(cmap_mean_tropics, col.regions = colorRampPalette(brewer.pal(8, 'BrBG'))(10), margin=F,
          at=c(-Inf, seq(-75, 75, 25), Inf), 
          colorkey=list(at=seq(-100, 100, 25),
          labels=c('<-100', seq(-75,75, 25), '>100')), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3")))+
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))

levelplot(persiann_mean_tropics, col.regions = colorRampPalette(brewer.pal(8, 'BrBG'))(10), margin=F,
          at=c(-Inf, seq(-75, 75, 25), Inf), 
          colorkey=list(at=seq(-100, 100, 25),
          labels=c('<-100', seq(-75,75, 25), '>100')), ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3")))+
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
levelplot(difference_p_mean, col.regions = colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))(12)[5:12], margin=F,
          at=c(seq(0, 70, 10), Inf), 
          colorkey=list(at=seq(0, 80, 10),
          labels=c(seq(0, 70,10), '>80')), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
  
  #print(p1, split=c(1, 1, 1, 4), more=TRUE)
  #print(p2, split=c(1, 2, 1, 4), more=TRUE)
  #print(p3, split=c(1, 3, 1, 4), more =TRUE)
  #print(p4, split=c(1, 4, 1, 4))
  

#----------CWD plots (with locations of sites going into NEE model [for yadvinder's talk])------------

#import ites .csv
model_sites <- read_csv("data/model_sites.csv")
future_model_sites = read_csv("data/future_model_sites.csv")

projection = crs(erai_max)

# .csv to R SpatialPointsDataFrame
model_sites <- SpatialPointsDataFrame(model_sites[,3:2],
                                    model_sites,           
                                    proj4string = projection, match.ID =F) 
future_model_sites <- SpatialPointsDataFrame(future_model_sites[,3:2],
                                    future_model_sites,           
                                    proj4string = projection, match.ID =F) 


levelplot(chirps_CWD_max_tropics, col.regions = rev(brewer.pal(8, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50), Inf), 
          colorkey=list(at=seq(-300, 0, 50), sub = "CHIRPS Maximum CWD Anomaly Jan. 2015 - May 2016",
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(model_sites, pch=21, col="black", fill="limegreen", cex=0.5)) +
          layer(sp.points(future_model_sites, pch=21, col="black", fill = "royalblue", cex=0.5))


levelplot(chirps_CWD_mean_tropics, col.regions = colorRampPalette(brewer.pal(10, 'BrBG')), margin=F,
          at=c(-Inf, seq(-250, 250, 50), Inf), 
          colorkey=list(at=seq(-300, 300, 50), sub = "CHIRPS Mean CWD Anomaly Jan. 2015 - May 2016",
          labels=c('< -300', seq(-250, 250, 50), '>300')), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(model_sites, pch=21, col="black", fill="limegreen", cex=0.5)) +
          layer(sp.points(future_model_sites, pch=21, col="black", fill = "royalblue", cex=0.5))

#zoom in on each continent
#creating bounding boxes
#SE Asia
extent_SEA <- as(raster::extent(103.0, 125.0, -6.0, 9.5), "SpatialPolygons")
proj4string(extent_SEA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#Africa
extent_A <- as(raster::extent(-25.0, 60.0, -21.0, 15.0), "SpatialPolygons")
proj4string(extent_A) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#sthAmerica
extent_SA <- as(raster::extent(-120.0, -28.0, -21.0, 21.0), "SpatialPolygons")
proj4string(extent_SA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

chirps_CWD_max_SEA = crop(chirps_CWD_max_tropics, extent_SEA)
chirps_CWD_max_A = crop(chirps_CWD_max_tropics, extent_A)
chirps_CWD_max_SA = crop(chirps_CWD_max_tropics, extent_SA)

levelplot(chirps_CWD_max_SEA, col.regions = rev(brewer.pal(8, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50), Inf), 
          colorkey=list(at=seq(-300, 0, 50), sub = "CHIRPS Maximum CWD Anomaly Jan. 2015 - May 2016",
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(model_sites, pch=21, col="black", fill="limegreen", cex=1.25)) +
          layer(sp.points(future_model_sites, pch=21, col="black", fill = "royalblue", cex=1.25))

levelplot(chirps_CWD_max_A, col.regions = rev(brewer.pal(8, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50), Inf), 
          colorkey=list(at=seq(-300, 0, 50), sub = "CHIRPS Maximum CWD Anomaly Jan. 2015 - May 2016",
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(model_sites, pch=21, col="black", fill="limegreen", cex=1.25)) +
          layer(sp.points(future_model_sites, pch=21, col="black", fill = "royalblue", cex=1.25))

levelplot(chirps_CWD_max_SA, col.regions = rev(brewer.pal(8, 'YlOrRd')), margin=F,
          at=c(-Inf, seq(-250, 0, 50), Inf), 
          colorkey=list(at=seq(-300, 0, 50), sub = "CHIRPS Maximum CWD Anomaly Jan. 2015 - May 2016",
          labels=c('< -300', seq(-250, 0, 50))), ylab = "", xlab="",
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(model_sites, pch=21, col="black", fill="limegreen", cex=1.5)) +
          layer(sp.points(future_model_sites, pch=21, col="black", fill = "royalblue", cex=1.5))


#-----plot of stations used in the station analysis---------------------------------------------------------
temp_sites <- read_csv("data/extraction_sites_temp.csv")

temp_sites <- SpatialPointsDataFrame(temp_sites[,2:3],
                                    temp_sites,           
                                    proj4string = projection, match.ID =F) 

forest_mask_plotting = forest_mask_025
values(forest_mask_plotting)[values(forest_mask_plotting) == 0] = NA #set zeros to NA so zeros aren't plotted

levelplot(forest_mask_plotting, col.regions = "forestgreen", 
          margin=F, ylab = "", xlab=NULL, scales=list(x=list(draw=FALSE)),
          pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7)) +
          layer(sp.points(temp_sites, pch=21, col="black", fill="red2", cex=0.5))
  


levelplot(chirps_CWD_max_tropics)


#-----------------------------------------------------------------------------------------------------------=
#fixing 'fraction of forest' mask to remove the zero values (they make the maps for yadvinder look shit)
a = "data/FoF_GFC250m_remapped.nc"
FoF = raster(a, varname = "fraction_of_forest")

FoF_reclassify = reclassify(FoF, cbind(0, NA))

writeRaster(FoF_reclassify, filename="FoF_final", format="CDF", 
            varname = "FoF", varunit = "fraction", longname = "fraction_of_forest",
            overwrite=TRUE)











