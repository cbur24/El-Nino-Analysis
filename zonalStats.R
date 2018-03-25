#zonal stats of mean and maximum temperature and precipitation anomalies
#for each product and each continent

FUN.zonalStats = function(x, y){
  x <- reclassify(x, cbind(Inf, NA))
  x <- reclassify(x, cbind(-Inf, NA))
  x <- raster::crop(x,y)
  x <- cellStats(x, stat='mean', na.rm=TRUE)
  x
}

#mean T anomalies
FUN.zonalStats(meant2m_stack, extent_SA)
FUN.zonalStats(meant2m_stack, extent_A)
FUN.zonalStats(meant2m_stack, extent_SEA)  

#max T anomalies
FUN.zonalStats(maxt2m_stack, extent_SA)
FUN.zonalStats(maxt2m_stack, extent_A)
FUN.zonalStats(maxt2m_stack, extent_SEA)  

#---mean P anomalies
#TRMM
FUN.zonalStats(trmm_mean_tropics, extent_SA)
FUN.zonalStats(trmm_mean_tropics, extent_A)
FUN.zonalStats(trmm_mean_tropics, extent_SEA)

#chirps
FUN.zonalStats(chirps_mean_tropics, extent_SA)  
FUN.zonalStats(chirps_mean_tropics, extent_A)  
FUN.zonalStats(chirps_mean_tropics, extent_SEA)  

#perisann
FUN.zonalStats(persiann_mean_tropics, extent_SA)
FUN.zonalStats(persiann_mean_tropics, extent_A)
FUN.zonalStats(persiann_mean_tropics, extent_SEA)

#cmap
FUN.zonalStats(cmap_mean_tropics, extent_SA)
FUN.zonalStats(cmap_mean_tropics, extent_A)
FUN.zonalStats(cmap_mean_tropics, extent_SEA)

#---max P anomalies
#TRMM
FUN.zonalStats(trmm_max_tropics, extent_SA)
FUN.zonalStats(trmm_max_tropics, extent_A)
FUN.zonalStats(trmm_max_tropics, extent_SEA)

#chirps
FUN.zonalStats(chirps_max_tropics, extent_SA)  
FUN.zonalStats(chirps_max_tropics, extent_A)  
FUN.zonalStats(chirps_max_tropics, extent_SEA)  

#perisann
FUN.zonalStats(persiann_max_tropics, extent_SA)
FUN.zonalStats(persiann_max_tropics, extent_A)
FUN.zonalStats(persiann_max_tropics, extent_SEA)

#cmap
FUN.zonalStats(cmap_max_tropics, extent_SA)
FUN.zonalStats(cmap_max_tropics, extent_A)
FUN.zonalStats(cmap_max_tropics, extent_SEA)

