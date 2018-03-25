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


#extract variables from netcdf station files
bitam <- nc_open("data/midas/bitam.nc")
time <- ncvar_get(bitam,'TIME')
temp <- ncvar_get(bitam,'AIR_TEMPERATURE')
maxtemp = ncvar_get(bitam,'MAX_AIR_TEMP')
mintemp = ncvar_get(bitam,'MIN_AIR_TEMP')
dewtemp = ncvar_get(bitam, 'DEWPOINT')
pressure <- ncvar_get(bitam,'STN_PRES')
wetbtemp <- ncvar_get(bitam, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
bitam_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(bitam_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
bitam_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(bitam_monthly_df)<- c("date","dewtemp","pressure", "temp")
bitam_monthly_df <- merge(full_dates, bitam_monthly_df, by = 1, 
                          all.x = TRUE)

write.csv(bitam_monthly_df, file="results/bitam_temp.csv")

#extract variables from netcdf station files
franceville <- nc_open("data/midas/franceville.nc")
time <- ncvar_get(franceville,'TIME')
temp <- ncvar_get(franceville,'AIR_TEMPERATURE')
maxtemp = ncvar_get(franceville,'MAX_AIR_TEMP')
mintemp = ncvar_get(franceville,'MIN_AIR_TEMP')
dewtemp = ncvar_get(franceville, 'DEWPOINT')
pressure <- ncvar_get(franceville,'STN_PRES')
wetbtemp <- ncvar_get(franceville, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
franceville_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(franceville_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
franceville_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(franceville_monthly_df)<- c("date","dewtemp","pressure", "temp")
franceville_monthly_df <- merge(full_dates, franceville_monthly_df, by = 1, all.x = TRUE)
write.csv(franceville_monthly_df, file="results/franceville_temp.csv")

#extract variables from netcdf station files
gemena <- nc_open("data/midas/gemena.nc")
time <- ncvar_get(gemena,'TIME')
temp <- ncvar_get(gemena,'AIR_TEMPERATURE')
maxtemp = ncvar_get(gemena,'MAX_AIR_TEMP')
mintemp = ncvar_get(gemena,'MIN_AIR_TEMP')
dewtemp = ncvar_get(gemena, 'DEWPOINT')
pressure <- ncvar_get(gemena,'STN_PRES')
wetbtemp <- ncvar_get(gemena, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
gemena_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(gemena_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
gemena_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(gemena_monthly_df)<- c("date","dewtemp","pressure", "temp")
gemena_monthly_df <- merge(full_dates, gemena_monthly_df, by = 1, all.x = TRUE)
write.csv(gemena_monthly_df, file="results/gemena_temp.csv")

#extract variables from netcdf station files
impfondo <- nc_open("data/midas/impfondo.nc")
time <- ncvar_get(impfondo,'TIME')
temp <- ncvar_get(impfondo,'AIR_TEMPERATURE')
maxtemp = ncvar_get(impfondo,'MAX_AIR_TEMP')
mintemp = ncvar_get(impfondo,'MIN_AIR_TEMP')
dewtemp = ncvar_get(impfondo, 'DEWPOINT')
pressure <- ncvar_get(impfondo,'STN_PRES')
wetbtemp <- ncvar_get(impfondo, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
impfondo_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(impfondo_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
impfondo_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(impfondo_monthly_df)<- c("date","dewtemp","pressure", "temp")
impfondo_monthly_df <- merge(full_dates, impfondo_monthly_df, by = 1, all.x = TRUE)
write.csv(impfondo_monthly_df, file="results/impfondo_temp.csv")

#extract variables from netcdf station files
kelle <- nc_open("data/midas/kelle.nc")
time <- ncvar_get(kelle,'TIME')
temp <- ncvar_get(kelle,'AIR_TEMPERATURE')
maxtemp = ncvar_get(kelle,'MAX_AIR_TEMP')
mintemp = ncvar_get(kelle,'MIN_AIR_TEMP')
dewtemp = ncvar_get(kelle, 'DEWPOINT')
pressure <- ncvar_get(kelle,'STN_PRES')
wetbtemp <- ncvar_get(kelle, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
kelle_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(kelle_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
kelle_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(kelle_monthly_df)<- c("date","dewtemp","pressure", "temp")
kelle_monthly_df <- merge(full_dates, kelle_monthly_df, by = 1, all.x = TRUE)
write.csv(kelle_monthly_df, file="results/kelle_temp.csv")

#extract variables from netcdf station files
lambarene <- nc_open("data/midas/lambarene.nc")
time <- ncvar_get(lambarene,'TIME')
temp <- ncvar_get(lambarene,'AIR_TEMPERATURE')
maxtemp = ncvar_get(lambarene,'MAX_AIR_TEMP')
mintemp = ncvar_get(lambarene,'MIN_AIR_TEMP')
dewtemp = ncvar_get(lambarene, 'DEWPOINT')
pressure <- ncvar_get(lambarene,'STN_PRES')
wetbtemp <- ncvar_get(lambarene, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
lambarene_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(lambarene_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
lambarene_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(lambarene_monthly_df)<- c("date","dewtemp","pressure", "temp")
lambarene_monthly_df <- merge(full_dates, lambarene_monthly_df, by = 1, all.x = TRUE)
write.csv(lambarene_monthly_df, file="results/lambarene_temp.csv")

#extract variables from netcdf station files
lastoursville <- nc_open("data/midas/lastoursville.nc")
time <- ncvar_get(lastoursville,'TIME')
temp <- ncvar_get(lastoursville,'AIR_TEMPERATURE')
maxtemp = ncvar_get(lastoursville,'MAX_AIR_TEMP')
mintemp = ncvar_get(lastoursville,'MIN_AIR_TEMP')
dewtemp = ncvar_get(lastoursville, 'DEWPOINT')
pressure <- ncvar_get(lastoursville,'STN_PRES')
wetbtemp <- ncvar_get(lastoursville, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
lastoursville_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(lastoursville_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
lastoursville_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(lastoursville_monthly_df)<- c("date","dewtemp","pressure", "temp")
lastoursville_monthly_df <- merge(full_dates, lastoursville_monthly_df, by = 1, all.x = TRUE)
write.csv(lastoursville_monthly_df, file="results/lastoursville_temp.csv")

#extract variables from netcdf station files
makokou <- nc_open("data/midas/makokou.nc")
time <- ncvar_get(makokou,'TIME')
temp <- ncvar_get(makokou,'AIR_TEMPERATURE')
maxtemp = ncvar_get(makokou,'MAX_AIR_TEMP')
mintemp = ncvar_get(makokou,'MIN_AIR_TEMP')
dewtemp = ncvar_get(makokou, 'DEWPOINT')
pressure <- ncvar_get(makokou,'STN_PRES')
wetbtemp <- ncvar_get(makokou, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
makokou_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(makokou_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
makokou_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(makokou_monthly_df)<- c("date","dewtemp","pressure", "temp")
makokou_monthly_df <- merge(full_dates, makokou_monthly_df, by = 1, all.x = TRUE)
write.csv(makokou_monthly_df, file="results/makokou_temp.csv")

#extract variables from netcdf station files
mouila <- nc_open("data/midas/mouila.nc")
time <- ncvar_get(mouila,'TIME')
temp <- ncvar_get(mouila,'AIR_TEMPERATURE')
maxtemp = ncvar_get(mouila,'MAX_AIR_TEMP')
mintemp = ncvar_get(mouila,'MIN_AIR_TEMP')
dewtemp = ncvar_get(mouila, 'DEWPOINT')
pressure <- ncvar_get(mouila,'STN_PRES')
wetbtemp <- ncvar_get(mouila, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
mouila_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(mouila_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
mouila_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(mouila_monthly_df)<- c("date","dewtemp","pressure", "temp")
mouila_monthly_df <- merge(full_dates, mouila_monthly_df, by = 1, all.x = TRUE)
write.csv(mouila_monthly_df, file="results/mouila_temp.csv")

#extract variables from netcdf station files
ouesso <- nc_open("data/midas/ouesso.nc")
time <- ncvar_get(ouesso,'TIME')
temp <- ncvar_get(ouesso,'AIR_TEMPERATURE')
maxtemp = ncvar_get(ouesso,'MAX_AIR_TEMP')
mintemp = ncvar_get(ouesso,'MIN_AIR_TEMP')
dewtemp = ncvar_get(ouesso, 'DEWPOINT')
pressure <- ncvar_get(ouesso,'STN_PRES')
wetbtemp <- ncvar_get(ouesso, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
ouesso_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(ouesso_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
ouesso_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(ouesso_monthly_df)<- c("date","dewtemp","pressure", "temp")
ouesso_monthly_df <- merge(full_dates, ouesso_monthly_df, by = 1, all.x = TRUE)
write.csv(ouesso_monthly_df, file="results/ouesso_temp.csv")

#extract variables from netcdf station files
souanke <- nc_open("data/midas/souanke.nc")
time <- ncvar_get(souanke,'TIME')
temp <- ncvar_get(souanke,'AIR_TEMPERATURE')
maxtemp = ncvar_get(souanke,'MAX_AIR_TEMP')
mintemp = ncvar_get(souanke,'MIN_AIR_TEMP')
dewtemp = ncvar_get(souanke, 'DEWPOINT')
pressure <- ncvar_get(souanke,'STN_PRES')
wetbtemp <- ncvar_get(souanke, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
souanke_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(souanke_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
souanke_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(souanke_monthly_df)<- c("date","dewtemp","pressure", "temp")
souanke_monthly_df <- merge(full_dates, souanke_monthly_df, by = 1, all.x = TRUE)
write.csv(souanke_monthly_df, file="results/souanke_temp.csv")

#extract variables from netcdf station files
tchibanga <- nc_open("data/midas/tchibanga.nc")
time <- ncvar_get(tchibanga,'TIME')
temp <- ncvar_get(tchibanga,'AIR_TEMPERATURE')
maxtemp = ncvar_get(tchibanga,'MAX_AIR_TEMP')
mintemp = ncvar_get(tchibanga,'MIN_AIR_TEMP')
dewtemp = ncvar_get(tchibanga, 'DEWPOINT')
pressure <- ncvar_get(tchibanga,'STN_PRES')
wetbtemp <- ncvar_get(tchibanga, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
tchibanga_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(tchibanga_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
tchibanga_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(tchibanga_monthly_df)<- c("date","dewtemp","pressure", "temp")
tchibanga_monthly_df <- merge(full_dates, tchibanga_monthly_df, by = 1, all.x = TRUE)
write.csv(tchibanga_monthly_df, file="results/tchibanga_temp.csv")

#extract variables from netcdf station files
yaounde <- nc_open("data/midas/yaounde.nc")
time <- ncvar_get(yaounde,'TIME')
temp <- ncvar_get(yaounde,'AIR_TEMPERATURE')
maxtemp = ncvar_get(yaounde,'MAX_AIR_TEMP')
mintemp = ncvar_get(yaounde,'MIN_AIR_TEMP')
dewtemp = ncvar_get(yaounde, 'DEWPOINT')
pressure <- ncvar_get(yaounde,'STN_PRES')
wetbtemp <- ncvar_get(yaounde, 'WETB_TEMP')
#convert time to dates
date = as.POSIXct("1900-01-01 00:00:00")+as.difftime(time,units="hours")
#create dataframe
yaounde_hourly_df = data.frame(date, temp, mintemp, maxtemp, dewtemp, pressure, wetbtemp)
#aggregate up to monthly means for each variable
z <- read.zoo(yaounde_hourly_df,  header = TRUE,  index.column = 'date', tz = "")
monthly_mean_temp <- aggregate(z$temp,  by = as.yearmon,  FUN = mean)
monthly_mean_dewtemp <- aggregate(z$dewtemp,  by = as.yearmon,  FUN = mean)
monthly_mean_pressure <- aggregate(z$pressure,  by = as.yearmon,  FUN = mean)
#create new dataframe with monthly values
monthly_mean_temp = fortify.zoo(monthly_mean_temp)
monthly_mean_dewtemp = fortify.zoo(monthly_mean_dewtemp)
monthly_mean_pressure = fortify.zoo(monthly_mean_pressure)
yaounde_monthly_df = data.frame(as.Date(monthly_mean_dewtemp$Index), monthly_mean_dewtemp$monthly_mean_dewtemp, 
                                  monthly_mean_pressure$monthly_mean_pressure, monthly_mean_temp$monthly_mean_temp)
colnames(yaounde_monthly_df)<- c("date","dewtemp","pressure", "temp")
yaounde_monthly_df <- merge(full_dates, yaounde_monthly_df, by = 1, all.x = TRUE)
write.csv(yaounde_monthly_df, file="results/yaounde_temp.csv")



















