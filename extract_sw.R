

library(ncdf4);library(RNetCDF);library(raster);library(tidyverse)

#import in netcdf files----------------------------------------

#Incident shortwave over land
a = "data/sw/MERRA2_1980_2016_monthly_SW_land.nc"
MERRA2_SW = stack(a, varname = "SWGDN")
MERRA2_SW <- setNames(MERRA2_SW, full_dates)

#PAR direct
a = "data/sw/MERRA2_1980_2016_monthly_PARDR.nc"
MERRA2_PAR = stack(a, varname = "PARDR")
MERRA2_PAR <- setNames(MERRA2_PAR, full_dates)


#-------------------------------------------------------------
#import extraction sites .csv
extraction_sites <- read_csv("data/sw/sw_extraction_sites.csv")
site_names = extraction_sites$site

# add coordinate system
projection = crs(MERRA2_SW)

# .csv to R SpatialPointsDataFrame
extraction_sites <- SpatialPointsDataFrame(extraction_sites[,3:2],
                                    extraction_sites,           
                                    proj4string = projection, match.ID =F)    
image(MERRA2_SW)
points(extraction_sites)

#--------------------------------------------------------------
# extract time series from the each rasterstack 


#SW
MERRA2_SW_df = raster::extract(MERRA2_SW,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
MERRA2_SW_df = MERRA2_SW_df %>% t() %>% data.frame()                #transpose output and convert to df
colnames(MERRA2_SW_df)=site_names                                   #adjust the names of the columns to match station names
MERRA2_SW_df = MERRA2_SW_df[-1,]                                    #remove the 'ID' row at the top of the dataframe
MERRA2_SW_df = data.frame(date = full_dates, MERRA2_SW_df)          #add date column

plot(MERRA2_SW_df$date, MERRA2_SW_df$IVI, 'l')
write.csv(MERRA2_SW_df, file="results/sw/merra2_sw.csv", row.names = F)

#PAR
MERRA2_PAR_df = raster::extract(MERRA2_PAR,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
MERRA2_PAR_df = MERRA2_PAR_df %>% t() %>% data.frame()                   
colnames(MERRA2_PAR_df)=site_names                                        
MERRA2_PAR_df = MERRA2_PAR_df[-1,]                                
MERRA2_PAR_df = data.frame(date = full_dates, MERRA2_PAR_df) 

write.csv(MERRA2_SW_df, file="results/sw/merra2_par.csv", row.names = F)
#-------------------------------------------------------------------------------











