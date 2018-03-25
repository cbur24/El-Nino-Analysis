
#bring in data
#erai
a = "data/anomaly_timeseries/erai_elnino_2014_2016_anomalies.nc"
erai_2014_16 = stack(a, varname = "t2m")
erai_2014_16 = rotate(erai_2014_16)
#MERRA2
a = "data/anomaly_timeseries/merra2_elnino_2014_2016_anomalies.nc"
merra2_2014_16 = stack(a, varname = "T2MMEAN")
#CRU
a = "data/anomaly_timeseries/cru_elnino_2014_2016_anomalies.nc"
cru_2014_16 = stack(a, varname = "tmp")
#JRA55
a = "data/anomaly_timeseries/jra55_elnino_2014_2016_anomalies.nc"
jra55_2014_16 = stack(a, varname = "TMP_GDS4_HTGL")
crs(jra55_2014_16) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#NCEP CFSR
a = "data/anomaly_timeseries/cfsr_elnino_2014_2016_anomalies.nc"
cfsr_2014_16 = stack(a, varname = "TMP_L103")
#TRMM
a = "data/anomaly_timeseries/trmm_elnino_2014_2016_anomalies.nc"
trmm_2014_16 = stack(a, varname = "precipitation")
crs(trmm_2014_16) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
trmm_2014_16 = t(trmm_2014_16)
trmm_2014_16 = flip(trmm_2014_16, direction = 1)
trmm_2014_16 = flip(trmm_2014_16, direction = 2)
#chirps
a = "data/anomaly_timeseries/chirps_elnino_2014_2016_anomalies.nc"
chirps_2014_16 = stack(a, varname = "precip")
#persiann
a = "data/anomaly_timeseries/persiann_elnino_2014_2016_anomalies.nc"
persiann_2014_16 = stack(a, varname="precip")

#cmap
#a = "data/anomaly_timeseries/cmap_elnino_2014_2016_anomalies.nc"
#cmap_2014_16 = stack(a, varname = "precip")
#cmap_2014_16 = rotate(cmap_2014_16)

#forest masks
a = "data/forestmask_05.nc"
forest_mask_05 = raster(a, varname = "tropical_forest_mask")
a = "data/forestmask_025.nc"
forest_mask_025 = raster(a, varname = "tropical_forest_mask")

# bring in SST data and make vector
SSTs = read_csv("data/nino34_SST_anom.csv")
SSTs = SSTs$Nino34anom


#------manipulate rasters---------------------------------------------------------------------

#remap files to common 0.5 grid (0.25 for precip) for ease of plotting, analysis etc
  #temp
  erai_2014_16 = resample(erai_2014_16, cru_2014_16, method = "bilinear")
  merra2_2014_16 = resample(merra2_2014_16, cru_2014_16, method = "bilinear")
  jra55_2014_16 = resample(jra55_2014_16, cru_2014_16, method = "bilinear")

  #rainfall (remapping chirps to match trmm)
  chirps_2014_16 = resample(chirps_2014_16, trmm_2014_16, method = "bilinear", progress="text")

#extract the 'tropics extent'
  erai_2014_16_tropics = crop(erai_2014_16, extent_tropics)
  merra2_2014_16_tropics = crop(merra2_2014_16, extent_tropics)
  cru_2014_16_tropics = crop(cru_2014_16, extent_tropics)
  jra55_2014_16_tropics = crop(jra55_2014_16, extent_tropics)
  cfsr_2014_16_tropics = crop(cfsr_2014_16, extent_tropics)
  trmm_2014_16_tropics = crop(trmm_2014_16, extent_tropics)
  chirps_2014_16_tropics = crop(chirps_2014_16, extent_tropics)
  persiann_2014_16_tropics = crop(persiann_2014_16, extent_tropics)
  #cmap_2014_16_tropics = crop(cmap_2014_16, extent_tropics)  
  forest_mask_05 = crop(forest_mask_05, extent_tropics)
  forest_mask_025 = crop(forest_mask_025, extent_tropics)

#create a Nino 3.4 SSTs raster stack to allow for easy correlation analysis
#both a 0.5 grid stack and a 0.25 grid stack (temp and precip, respectively)
  
  #0.5x0.5 grid
  tmp = as.data.frame(erai_2014_16_tropics, xy=T)     #convert raster to df
  coords=tmp[1:2]                                     #extract the coordinates from the df (add them back later)
  tmp = select(tmp, -x, -y)                           #remove the coords columns
  tmp = as.data.frame(t(tmp))                         #transpose so its easy to add the SSTs to the columns
  tmp[1:47880] = SSTs                                 #add the SSTs vector to each column of the df
  tmp = as.data.frame(t(tmp))                         #transpose df back to original orientation
  tmp = bind_cols(coords,tmp)                         #add back in the coords columns
  SSTs_stack_05 = rasterFromXYZ(tmp)                  #convert from df back to rasterStack
  crs(SSTs_stack_05)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  SSTs_stack_05 = crop(SSTs_stack_05, extent_tropics)
  
  #0.25x0.25 grid
  tmp = as.data.frame(trmm_2014_16_tropics, xy=T)     
  coords=tmp[1:2]                             
  tmp = select(tmp, -x, -y)                   
  tmp = as.data.frame(t(tmp))                 
  tmp[1:191520] = SSTs                        
  tmp = as.data.frame(t(tmp))                 
  tmp = bind_cols(coords,tmp)                 
  SSTs_stack_025 = rasterFromXYZ(tmp)          
  crs(SSTs_stack_025)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  SSTs_stack_025 = crop(SSTs_stack_025, extent_tropics)
  
#mask all the rasters with the forest mask  
  erai_2014_16_tropics = erai_2014_16_tropics/forest_mask_05
  merra2_2014_16_tropics = merra2_2014_16_tropics/forest_mask_05
  cru_2014_16_tropics = cru_2014_16_tropics/forest_mask_05
  jra55_2014_16_tropics = jra55_2014_16_tropics/forest_mask_05
  cfsr_2014_16_tropics = cfsr_2014_16_tropics/forest_mask_05
  trmm_2014_16_tropics = trmm_2014_16_tropics/forest_mask_025
  chirps_2014_16_tropics = chirps_2014_16_tropics/forest_mask_025
  persiann_2014_16_tropics = persiann_2014_16_tropics/forest_mask_025
  SSTs_stack_05 = SSTs_stack_05/forest_mask_05    
  SSTs_stack_025 = SSTs_stack_025/forest_mask_025  
  
#-------correlations--------------------------------------------------------------------

#find correlation coefficients and p-values of coefficients
  #temp
  corr_erai <- corLocal(erai_2014_16_tropics, SSTs_stack_05, test=TRUE, method="pearson")
  corr_merra2 <- corLocal(merra2_2014_16_tropics, SSTs_stack_05, test=TRUE, method="pearson")
  corr_jra55 <- corLocal(jra55_2014_16_tropics, SSTs_stack_05, test=TRUE, method="pearson")
  corr_cfsr <- corLocal(cfsr_2014_16_tropics, SSTs_stack_05, test=TRUE, method="pearson")
  corr_cru <- corLocal(cru_2014_16_tropics, SSTs_stack_05, test=TRUE, method="pearson")
  
  #precip
  corr_trmm <- corLocal(trmm_2014_16_tropics, SSTs_stack_025, test=TRUE, method="pearson")
  corr_chirps <- corLocal(chirps_2014_16_tropics, SSTs_stack_025, test=TRUE, method="pearson")
  
  #mask out coefficents with p-values > 0.1
  corr_erai = mask(corr_erai[[1]], corr_erai[[2]] < 0.1, maskvalue=FALSE)
  corr_merra2 = mask(corr_merra2[[1]], corr_merra2[[2]] < 0.1, maskvalue=FALSE)
  corr_jra55 = mask(corr_jra55[[1]], corr_jra55[[2]] < 0.1, maskvalue=FALSE)
  corr_cfsr = mask(corr_cfsr[[1]], corr_cfsr[[2]] < 0.1, maskvalue=FALSE)
  corr_cru = mask(corr_cru[[1]], corr_cru[[2]] < 0.1, maskvalue=FALSE)
  corr_trmm = mask(corr_trmm[[1]], corr_trmm[[2]] < 0.1, maskvalue=FALSE)
  corr_chirps = mask(corr_trmm[[1]], corr_chirps[[2]] < 0.1, maskvalue=FALSE)
 
#create a stack of correlation maps for easy plotting
  corr_t2m_stack = stack(corr_erai, corr_merra2, corr_jra55, corr_cfsr)
  temp_stack_names = c("ERA Interim", "MERRA2", "JRA55", "CFSR")
  corr_t2m_stack = setNames(corr_t2m_stack, temp_stack_names) 
  
  corr_p_stack = stack(corr_chirps, corr_trmm)
  rain_stack_names = c("CHIRPS", "TRMM")
  corr_p_stack = setNames(corr_p_stack, rain_stack_names) 

p.strip <- list(cex=0.85, lines=1)
#plot
 levelplot(corr_t2m_stack, col.regions = rev(brewer.pal(8, 'RdBu')), margin=F,
          at=c(-Inf, seq(-0.75, 0.75, 0.25), Inf), interpolate=TRUE, 
          colorkey=list(at=seq(-1.00, 1.00, 0.25), 
          labels=c('-1.00', seq(-0.75, 0.75, 0.25), '1.00')), names.attr= temp_stack_names, par.strip.text=p.strip,
          ylab = "", xlab="", pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))
 
 levelplot(corr_p_stack, col.regions = rev(brewer.pal(8, 'RdBu')), margin=F,
          at=c(-Inf, seq(-0.75, 0.75, 0.25), Inf), interpolate=TRUE, 
          colorkey=list(at=seq(-1.00, 1.00, 0.25),
          labels=c('-1.00', seq(-0.75, 0.75, 0.25), '1.00')), names.attr= rain_stack_names, par.strip.text=p.strip,
          ylab = "", xlab="", pretty=TRUE, par.settings=list(panel.background=list(col="slategray3"))) +
          layer_(sp.polygons(continents, col = "grey33", fill="gray70", lwd = 0.7))


#attempting to animate (failed) 
# path.to.convert <- shQuote("C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe") 
# 
# saveGIF(animate(erai_2014_16_tropics), convert="convert",
#         ani.options(convert=path.to.convert), cmd.fun=system,
#         movie.name = "tmep.gif", img.name="erai", outdir = getwd())  
# 
# saveGIF(for (i in 1:36) {
#                       plot(erai_2014_16_tropics[[i]],
#                       main="ERA-Interim")
#       }, 
#     movie.name = 'tmp.gif', img.name="erai",
#     cmd.fun = system, 
#     convert="convert",
#     ani.options(convert=path.to.convert),
#     interval = 0.1,
#     outdir = getwd())



