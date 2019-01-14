library(ncdf4)
library(RNetCDF)
library(raster)
library(zoo)
library(lattice)

#extract variables from netcdf station files
berberati <- nc_open("data/midas/berberati.nc")
time <- ncvar_get(berberati,'TIME')
temp <- ncvar_get(berberati,'AIR_TEMPERATURE')
maxtemp = ncvar_get(berberati,'MAX_AIR_TEMP')
mintemp = ncvar_get(berberati,'MIN_AIR_TEMP')
dewtemp = ncvar_get(berberati, 'DEWPOINT')
pressure <- ncvar_get(berberati,'STN_PRES')
wetbtemp <- ncvar_get(berberati, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
berberati_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(berberati_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
berberati_monthly_df = data.frame(as.Date(as.Date(monthly_mean_dewtemp$Index)), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(berberati_monthly_df)<- c("date","dewtemp","pressure", "temp")

full_dates <- seq(as.Date("1980/1/1"), by = "month", length.out = 444)
full_dates <- data.frame(date = full_dates)
berberati_monthly_df <- merge(full_dates, berberati_monthly_df, by = 1, 
                          all.x = TRUE)
write.csv(berberati_monthly_df, file="results/berberati_temp.csv")



















