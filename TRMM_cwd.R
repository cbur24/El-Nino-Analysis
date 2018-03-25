
#Calculating the Climatological water deficit using TRMM B42 rainfall product
#and the CHIRPS monthly rainfall product.

library(ncdf4)
library(raster)
library(RNetCDF)

# import TRMM netCDF file and convert to raster stack
a = "TRMM3B42_1998_2016_monthly.nc4"
TRMM_1998_2016 = stack(a, varname = 'precipitation')
crs(TRMM_1998_2016) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
TRMM_1998_2016 = t(TRMM_1998_2016)
TRMM_1998_2016 = flip(TRMM_1998_2016, direction = 1)
TRMM_1998_2016 = flip(TRMM_1998_2016, direction = 2)
dates <- format(seq(as.Date('1998/01/1'), as.Date('2016/12/1'), by='month'), '%Y%m')
dates[1:228]
TRMM_1998_2016<- setNames(TRMM_1998_2016, dates)
plot(TRMM_1998_2016[[1]])


#----------------------------------------------------------------------
## CWD for month n = CWDn-1 + Pn - ETn, where ET is fixed at 100mm/month
#  When CWD>0, CWD = 0
## CWDn = CWDn-1 + Pn- 100
#----------------------------------------------------------------------

# Step by step example of how CWD is calculated
# cwd_jan_00 <- TRMM_2000_2016[["X200001"]] - 100 
# cwd_feb_00 <- cwd_jan_00 + TRMM_2000_2016[["X200002"]] - 100
# cwd_mar_00 = cwd_feb_00 + TRMM_2000_2016[["X200003"]] - 100
# etc.

# loop for calculating for TRMM
TRMM_cwd <- TRMM_1998_2016              # set the cwd layer as the rasterstack of interest
TRMM_cwd[[1]] = TRMM_cwd[[1]] - 100     # intialize the first step in the loop
TRMM_cwd[[1]][TRMM_cwd[[1]]>0] = 0      # Max possible value of CWD is 0, where values higher are set to 0

N = nlayers(TRMM_1998_2016) 

for(i in 2:N){
  TRMM_cwd[[i]] <- TRMM_cwd[[i-1]] + TRMM_1998_2016[[i]] - 100
  TRMM_cwd[[i]][TRMM_cwd[[i]]>0] <- 0
} 

plot(TRMM_cwd[[5]])
## rename raster layers again
TRMM_cwd <- setNames(TRMM_cwd, dates)
names(TRMM_cwd)[1:228]

# export out as a netCDF
writeRaster(TRMM_cwd, filename="TRMM_1998_2016_CWD", format="CDF", 
            varname = "CWD", varunit = "mm/month", longname = "Climatological Water Deficit",
            zname = "time", overwrite=TRUE, force_v4=TRUE, compression=7)



##------extract time series from Bobiri to check the CWD values look sensible--------

# add coordinate system
projection = crs(TRMM_cwd)
projection

# .csv to R SpatialPointsDataFrame
GEM_sites <- SpatialPointsDataFrame(extraction_sites[,1:2],
                                    extraction_sites,           #the R object to convert
                                    proj4string = projection)   # assign a CRS 

# plot points on one of the images and check that the spatial points are in the right place
image(TRMM_cwd)
points(GEM_sites)

# extract time series from the rasterstack at the locations in the spatialpointsdataframe
TRMM_cwd_timeseries = extract(TRMM_cwd,            #raster layer
                              GEM_sites,           #spatial points
                              method = 'simple',   #just the pixel at the coordinates 
                              layer = 2,           #start at layer 2
                              nl = 204,            #run through to the last layer
                              df = TRUE)           #output result as a data frame

# write .csv and rearrange table for easy plotting
write.csv(TRMM_CWD_timeseries, file = "C:/Users/Chad/Desktop/dissertation/scale_up/MCWD/TRMM_cwd_timeseries.csv")

#import adjusted .csv and plot up CWD values from Bobiri


plot(TRMM_cwd_timeseries$Date, TRMM_cwd_timeseries$KEN01, xlab = "",
      ylab = "CWD (mm)", cex = 0, main = "TRMM CWD: Bobiri, Ghana", colour = "blue")
      lines(TRMM_cwd_timeseries$Date, TRMM_cwd_timeseries$KEN01) 
        abline(h = 0, col = "grey66")














