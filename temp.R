

library(ncdf4);library(RNetCDF);library(raster);library(tidyverse);library(lubridate);library(stringr)

# some functions for the script-------
#Root Mean Squared Error
rmse <- function(obs, model){
   x <- (obs - model)
   sqrt(mean(x^2, na.rm = TRUE))
}
# Mean Absolute Error
mae <- function(obs, model){
  x <- abs(obs-pred)
  mean(x, na.rm=T)
}
# Systematic mean square error
mse_s <- function(pred, obs){ #'pred' here refers to the predicted value from the least-squares regression (see Willmott 1982)
  x <- (pred-obs)^2
  mean(x, na.rm=T)
}
# Unsystematic mean square error
mse_u <- function(model, pred){
  x <- (model-pred)^2
  mean(x, na.rm=T)
}

#--function for making point transparent in a base plot---
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}
#-------------------import in netcdf files----------------------------------------

#ERAI
a = "data/reanalysis/ERAI_1980_2016_monthly_tmean.nc"
ERAI_tmean = stack(a, varname = "t2m")
ERAI_tmean = rotate(ERAI_tmean)
ERAI_tmean = ERAI_tmean - 273.15

#MERRA2
b = "data/reanalysis/MERRA2_1980_2016_monthly_tmean.nc"
MERRA2_tmean = stack(b, varname = "T2MMEAN")
MERRA2_tmean = MERRA2_tmean - 273.15

#CRU
c = "data/reanalysis/CRU_1980_2016_monthly_tmean.nc"
CRU_tmean = stack(c, varname = "tmp")

#JRA55
d = "data/reanalysis/jra55_1980_2016_monthly_tmean.nc"
jra55_tmean = stack(d, varname = "TMP_GDS4_HTGL")
crs(jra55_tmean) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
jra55_tmean = jra55_tmean - 273.15

#cfsr
a = "data/reanalysis/cfsr_1980_2016_tmean.nc"
cfsr_tmean = stack(a, varname = "TMP_L103")
cfsr_tmean = cfsr_tmean - 273.15

#convert reanalysis to a common 0.5x0.5 grid
ERAI_tmean = resample(ERAI_tmean, CRU_tmean, method = "bilinear", progress="text")
MERRA2_tmean = resample(MERRA2_tmean, CRU_tmean, method = "bilinear", progress="text")
jra55_tmean = resample(jra55_tmean, CRU_tmean, method = "bilinear", progress="text")
cfsr_tmean = resample(cfsr_tmean, CRU_tmean, method = "bilinear", progress="text")

#crop to the same extent
ERAI_tmean = crop(ERAI_tmean, extent_tropics)
MERRA2_tmean = crop(MERRA2_tmean, extent_tropics)
jra55_tmean = crop(jra55_tmean, extent_tropics)
cfsr_tmean = crop(cfsr_tmean, extent_tropics)

#ensemble mean of the reanalysis temps
ensemble_tmean = (ERAI_tmean+MERRA2_tmean+jra55_tmean)/3

#-------------------bring in station files with observed temperatures ---------------------

#####  Sth America ################

setwd("C:/Users/Chad/Desktop/internship/station_comparisons/data/temp/SA") #setwd to where the .csv files are located
file_list <- list.files()
file_list <- file_list[!grepl("hadcru", file_list)] #this removes the folder from the list
for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, colClasses=c("NULL",NA))
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, colClasses=c("NULL",NA))
    dataset<-cbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

temp.obs = dataset                                      
remove(dataset) 
temp.obs = temp.obs[,-1]
stationnames = gsub("\\.csv$","", list.files(pattern="\\.csv$"))        
colnames(temp.obs) = stationnames                                      
temp.obs <- stack(temp.obs)
dates= rep(as.POSIXlt(seq(ymd_hms('1980-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=13)
temp_stationData_SA <- data.frame(DATE=dates,TAVG=temp.obs$values, NAME=temp.obs$ind)
temp_stationData_SA$NAME <- str_sub(temp_stationData_SA$NAME, 1, str_length(temp_stationData_SA$NAME)-5)
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")# reset wd

#bring in the HadCRU data (which is arranged diferently from the other station data above)
setwd("data/temp/SA/hadcru") 
list = list.files(getwd())
x <- do.call(rbind, lapply(list, read_csv))
x$DATE <- parse_date_time(x$DATE, "ymd",tz="UTC", truncated = 3)
x = x %>% filter(DATE>= as.Date("1980-01-01"))
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/") 

#bind the two station datasets together
temp_stationData_SA <- bind_rows(temp_stationData_SA, x)
temp_stationData_SA <- temp_stationData_SA %>% filter(DATE>= as.Date("1980-01-01")) #filter out dates before 1980
stationCount_temp_SA <- temp_stationData_SA %>% filter(!is.na(TAVG)) %>% count(NAME)
write_csv(stationCount_temp_SA, path="data/temp/stationCount_temp_SA.csv")


####  SE Asia ################

setwd("data/temp/SEA") 
list = list.files(getwd())
temp_stationData_SEA <- do.call(rbind, lapply(list, read_csv))
temp_stationData_SEA$DATE <- parse_date_time(temp_stationData_SEA$DATE, "ymd",tz="UTC", truncated = 3)

#removing some stations (too close to mountains or on the coast or on small islands)
rm_stations <- c("KOTA KINABALU INTERNATIONAL, MY", "KASIGUNCU, ID","MASAMBA ANDI JEMMA, ID", "MAJENE, ID",
                "DABO, ID", "BIAK FRANS KAISIEPO, ID", "SERUI YENDOSA, ID", "BABULLAH, ID", "AMBON PATTIMURA, ID", 
                "NAMLEA BURU ISLAND, ID", "SORONG JEFMAN, ID", "BITUNG, ID", "SANANA, ID", "LOCKHART RIVER AIRPORT, AS",
                "AMAHAI, ID", "GESER, ID", "LABUHA TALIABU, ID", "GALELA GAMARMALAMU, ID", "LOS NEGROS ADMRTY IS MOMOTE S, PP")
temp_stationData_SEA <- temp_stationData_SEA %>% filter(!NAME %in% rm_stations)
temp_stationData_SEA <- temp_stationData_SEA %>% filter(DATE>= as.Date("1980-01-01"))
#count the number of valid observations for each station
#and remove those with < a year's worth of observations
x <- temp_stationData_SEA %>%
   filter(!is.na(TAVG)) %>%
   count(NAME) %>% 
   filter(n<12)
temp_stationData_SEA <- temp_stationData_SEA %>% filter(!NAME %in% x$NAME)
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")
stationCount_temp_SEA <- temp_stationData_SEA %>% filter(!is.na(TAVG)) %>% count(NAME)
write_csv(stationCount_temp_SEA, path="data/temp/stationCount_temp_SEA.csv")

# tmp=c("DABO, ID", "BIAK FRANS KAISIEPO, ID", "LOS NEGROS ADMRTY IS MOMOTE S, PP", "SERUI YENDOSA, ID",
#       "BABULLAH, ID", "AMBON PATTIMURA, ID", "NAMLEA BURU ISLAND, ID", "SORONG JEFMAN, ID")
# 
# tmp1 <- erai_validation_SEA %>% filter(NAME %in% tmp) %>% 
#   mutate(diff=TAVG_station-TAVG_model)

sites_temp_SEA = temp_stationData_SEA %>% select(NAME, LATITUDE, LONGITUDE)
sites_temp_SEA = distinct(sites_temp_SEA)
write_csv(sites_temp_SEA, path="data/temp/sites_temp_SEA.csv")

####  Africa ################

setwd("C:/Users/Chad/Desktop/internship/station_comparisons/data/temp/A") #setwd to where the .csv files are located
file_list <- list.files()
file_list <- file_list[!grepl("hadcru", file_list)]
for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, colClasses=c("NULL",NA))
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, colClasses=c("NULL",NA))
    dataset<-cbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}
temp.obs = dataset                                      
remove(dataset) 
temp.obs = temp.obs[,-1]
stationnames = gsub("\\.csv$","", list.files(pattern="\\.csv$"))        
colnames(temp.obs) = stationnames                                      
temp.obs <- stack(temp.obs)
dates= rep(as.POSIXlt(seq(ymd_hms('1980-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=15)
temp_stationData_A <- data.frame(DATE=dates,TAVG=temp.obs$values, NAME=as.character(temp.obs$ind))
temp_stationData_A$NAME <- str_sub(temp_stationData_A$NAME, 1, str_length(temp_stationData_A$NAME)-5) #removes '_temp'
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")# reset wd

#bring in the HadCRU data 
setwd("data/temp/A/hadcru") 
list = list.files(getwd())
x <- do.call(rbind, lapply(list, read_csv))
x$DATE <- parse_date_time(x$DATE, "ymd",tz="UTC", truncated = 3)
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")

#bind the two station datasets together
temp_stationData_A <- bind_rows(temp_stationData_A, x)
temp_stationData_A <- temp_stationData_A %>% filter(DATE>= as.Date("1980-01-01"))
stationCount_temp_A <- temp_stationData_A %>% filter(!is.na(TAVG)) %>% count(NAME)
write_csv(stationCount_temp_A, path="data/temp/stationCount_temp_A.csv")

#-------------------extract time series from the each rasterstacks -----------------------------

#import extraction sites .csv
sites_temp_SA <- read_csv("data/temp/sites_temp_SA.csv")
sites_temp_A <- read_csv("data/temp/sites_temp_A.csv")
sites_temp_SEA <- read_csv("data/temp/sites_temp_SEA.csv")

# add coordinate system
projection = crs(MERRA2_tmean)

# .csv to SpatialPointsDataFrame
sites_temp_SA_spatial <- SpatialPointsDataFrame(sites_temp_SA[,2:3],sites_temp_SA,proj4string = projection)    
sites_temp_A_spatial <- SpatialPointsDataFrame(sites_temp_A[,2:3],sites_temp_A,proj4string = projection)
sites_temp_SEA_spatial <- SpatialPointsDataFrame(sites_temp_SEA[,3:2],sites_temp_SEA,proj4string = projection)

####  South America ###############
reanalysis_list <- list(MERRA2_tmean, ERAI_tmean, jra55_tmean, cfsr_tmean, ensemble_tmean)
x=c("merra2", "erai", "jra55", "cfsr", "ensemble")

results <- lapply(reanalysis_list, raster::extract, sites_temp_SA_spatial) #apply extract over the list of reanalysis products
names(results) <- x
results <- as.data.frame(results)

merra2_validation_SA <- results %>% select(contains("merra2")) #select out the results of each reanalysis extraction
erai_validation_SA <- results %>% select(contains("erai")) 
jra55_validation_SA <- results %>% select(contains("jra55")) 
cfsr_validation_SA <- results %>% select(contains("cfsr")) 
ensemble_validation_SA <- results %>% select(contains("ensemble")) 

#can also unlist a list back into seperate dataframes use these two options:
  # list2env(mylist ,.GlobalEnv) # or fr an unamaed list,
  # for (i in seq(df.list))
  # assign(paste("df", i, sep = ""), df.list[[i]])
  
#datatable wrangling
FUN.wrangle <- function(x){           #this function does the data wrangling to get the df into the format I want
  x <-  x %>% t() %>% data.frame() 
  colnames(x) <- sites_temp_SA$site 
  x <- stack(x)
  x <-  data.frame(DATE = rep(as.POSIXlt(seq(ymd_hms('1980-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=23), x) #change the 'times' to match the number of station sites for each continent
  colnames(x) <- c("DATE", "TAVG", "NAME")
  x
}

merra2_validation_SA <- FUN.wrangle(merra2_validation_SA) #apply the function
erai_validation_SA <- FUN.wrangle(erai_validation_SA)
jra55_validation_SA <- FUN.wrangle(jra55_validation_SA)
cfsr_validation_SA <- FUN.wrangle(cfsr_validation_SA)
ensemble_validation_SA <- FUN.wrangle(ensemble_validation_SA)

####### Africa #################
results <- lapply(reanalysis_list, raster::extract, sites_temp_A_spatial)
names(results) <- x
results <- as.data.frame(results)

FUN.wrangle <- function(x){  
  x <-  x %>% t() %>% data.frame() 
  colnames(x) <- sites_temp_A$site 
  x <- stack(x)
  x <-  data.frame(DATE = rep(as.POSIXlt(seq(ymd_hms('1980-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=26), x)
  colnames(x) <- c("DATE", "TAVG", "NAME")
  x
}

merra2_validation_A <- results %>% select(contains("merra2")) 
erai_validation_A <- results %>% select(contains("erai")) 
jra55_validation_A <- results %>% select(contains("jra55")) 
cfsr_validation_A <- results %>% select(contains("cfsr")) 
ensemble_validation_A <- results %>% select(contains("ensemble"))

merra2_validation_A <- FUN.wrangle(merra2_validation_A)
erai_validation_A <- FUN.wrangle(erai_validation_A)
jra55_validation_A <- FUN.wrangle(jra55_validation_A)
cfsr_validation_A <- FUN.wrangle(cfsr_validation_A)
ensemble_validation_A <- FUN.wrangle(ensemble_validation_A)

##### SE Asia ####################
results <- lapply(reanalysis_list, raster::extract, sites_temp_SEA_spatial)
names(results) <- x
results <- as.data.frame(results)

FUN.wrangle <- function(x){  
  x <-  x %>% t() %>% data.frame() 
  colnames(x) <- sites_temp_SEA$NAME 
  x <- stack(x)
  x <-  data.frame(DATE = rep(as.POSIXlt(seq(ymd_hms('1980-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=37), x)
  colnames(x) <- c("DATE", "TAVG", "NAME")
  x
}

merra2_validation_SEA <- results %>% select(contains("merra2")) 
erai_validation_SEA <- results %>% select(contains("erai")) 
jra55_validation_SEA <- results %>% select(contains("jra55")) 
cfsr_validation_SEA <- results %>% select(contains("cfsr")) 
ensemble_validation_SEA <- results %>% select(contains("ensemble"))

merra2_validation_SEA <- FUN.wrangle(merra2_validation_SEA)
erai_validation_SEA <- FUN.wrangle(erai_validation_SEA)
jra55_validation_SEA <- FUN.wrangle(jra55_validation_SEA)
cfsr_validation_SEA <- FUN.wrangle(cfsr_validation_SEA)
ensemble_validation_SEA <- FUN.wrangle(ensemble_validation_SEA)

###-----------------join station data to the reanalysis data-----------

##### South America #####
merra2_validation_SA = left_join(temp_stationData_SA, merra2_validation_SA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
erai_validation_SA = left_join(temp_stationData_SA, erai_validation_SA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
cfsr_validation_SA = left_join(temp_stationData_SA, cfsr_validation_SA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
jra55_validation_SA = left_join(temp_stationData_SA, jra55_validation_SA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
ensemble_validation_SA = left_join(temp_stationData_SA, ensemble_validation_SA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))

##### Africa #####
merra2_validation_A = left_join(temp_stationData_A, merra2_validation_A, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
erai_validation_A = left_join(temp_stationData_A, erai_validation_A, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
cfsr_validation_A = left_join(temp_stationData_A, cfsr_validation_A, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
jra55_validation_A = left_join(temp_stationData_A, jra55_validation_A, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
ensemble_validation_A = left_join(temp_stationData_A, ensemble_validation_A, by = c("DATE", "NAME"), suffix=c("_station", "_model"))

##### SE Asia #####
merra2_validation_SEA = left_join(temp_stationData_SEA, merra2_validation_SEA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
erai_validation_SEA = left_join(temp_stationData_SEA, erai_validation_SEA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
cfsr_validation_SEA = left_join(temp_stationData_SEA, cfsr_validation_SEA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
jra55_validation_SEA = left_join(temp_stationData_SEA, jra55_validation_SEA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
ensemble_validation_SEA = left_join(temp_stationData_SEA, ensemble_validation_SEA, by = c("DATE", "NAME"), suffix=c("_station", "_model"))

# ------------------data checks and tidying----------------------------------------------

#need to fix some of the station data points which have -999 values in them (redo this with a function and lapply)
merra2_validation_SA$TAVG_station[merra2_validation_SA$TAVG_station<0] = NA
erai_validation_SA$TAVG_station[erai_validation_SA$TAVG_station<0] = NA
jra55_validation_SA$TAVG_station[jra55_validation_SA$TAVG_station<0] = NA
cfsr_validation_SA$TAVG_station[cfsr_validation_SA$TAVG_station<0] = NA
ensemble_validation_SA$TAVG_station[ensemble_validation_SA$TAVG_station<0] = NA

merra2_validation_A$TAVG_station[merra2_validation_A$TAVG_station<0] = NA
erai_validation_A$TAVG_station[erai_validation_A$TAVG_station<0] = NA
jra55_validation_A$TAVG_station[jra55_validation_A$TAVG_station<0] = NA
cfsr_validation_A$TAVG_station[cfsr_validation_A$TAVG_station<0] = NA
ensemble_validation_A$TAVG_station[ensemble_validation_A$TAVG_station<0] = NA

merra2_validation_SEA$TAVG_station[merra2_validation_SEA$TAVG_station<0] = NA
erai_validation_SEA$TAVG_station[erai_validation_SEA$TAVG_station<0] = NA
jra55_validation_SEA$TAVG_station[jra55_validation_SEA$TAVG_station<0] = NA
cfsr_validation_SEA$TAVG_station[cfsr_validation_SEA$TAVG_station<0] = NA
ensemble_validation_SEA$TAVG_station[ensemble_validation_SEA$TAVG_station<0] = NA

#a site in africa has a monthly mean T of >50C, need to remove
merra2_validation_A$TAVG_station[merra2_validation_A$TAVG_station>40] = NA
erai_validation_A$TAVG_station[erai_validation_A$TAVG_station>40] = NA
jra55_validation_A$TAVG_station[jra55_validation_A$TAVG_station>40] = NA
cfsr_validation_A$TAVG_station[cfsr_validation_A$TAVG_station>40] = NA
ensemble_validation_A$TAVG_station[ensemble_validation_A$TAVG_station>40] = NA

#there are pixels in SE Asia that are v cold compared with the stations:
#select and examine where these plots are:
# cold_sites <- cfsr_validation_SEA %>% filter(TAVG_model<23)
# #ok these sites are either right on the coast or directly adjacent steep mountains 
# #--> removing them in the code for the SEA stations above 

# temp_stationData_SA %>% ggplot(data=., aes(DATE, TAVG , color=NAME))+geom_point()+viridis::scale_color_viridis() #
# merra2_validation_SEA %>% filter(NAME =="LOCKHART RIVER AIRPORT, AS") %>% ggplot(data=., aes(DATE, TAVG_station))+geom_line()
# merra2_validation_SEA %>% filter(NAME =="MADANG W.O., PP") %>% ggplot(data=., aes(DATE, TAVG_station))+geom_line()
# #Lockhart River in Australia has a much greater amplitude of seasonal temp variability and skews the plots because
# it has so many more data points than the other sites - removing.


#-------------------Scatter-plots----------------

#create a list of the dataframes to work with
y=list("ERA-I Sth America" = erai_validation_SA, "ERA-I Africa"= erai_validation_A, "ERA-I SE Asia"=erai_validation_SEA,
       "MERRA2 Sth America" = merra2_validation_SA, "MERRA2 Africa" = merra2_validation_A, "MERRA2 SE Asia"=merra2_validation_SEA,
       "JRA-55 Sth America"=jra55_validation_SA, "JRA-55 Africa"=jra55_validation_A, "JRA-55 SE Asia"=jra55_validation_SEA,
       "CFSR Sth America"=cfsr_validation_SA, "CFSR Africa"=cfsr_validation_A, "CFSR SE Asia"=cfsr_validation_SEA,
       "Ensemble Sth America"=ensemble_validation_SA, "Ensemble Africa"=ensemble_validation_A, "Ensemble SE Asia"=ensemble_validation_SEA)


#linear models
lm_temp_results <- list()
for (i in 1:length(y)) {
  lm_obj <- lm(TAVG_model ~ TAVG_station, data = y[[i]])
  tmp <- c(lm_obj$coefficients,
           summary(lm_obj)$r.squared,
           mae(y[[i]]$TAVG_station, y[[i]]$TAVG_model),
           # rmse(y[[i]]$TAVG_station, y[[i]]$TAVG_model),
           mse_s((coef(lm_obj)[1] + (coef(lm_obj)[2]*y[[i]]$TAVG_station)), y[[i]]$TAVG_station),
           mse_u(y[[i]]$TAVG_model, (coef(lm_obj)[1] + (coef(lm_obj)[2]*y[[i]]$TAVG_station))),
           mean(y[[i]]$TAVG_station, na.rm=T),
           sd(y[[i]]$TAVG_station, na.rm=T),
           mean(y[[i]]$TAVG_model, na.rm=T),
           sd(y[[i]]$TAVG_model, na.rm=T),
           hydroGOF::md(y[[i]]$TAVG_model, y[[i]]$TAVG_station),
           length(na.omit(y[[i]]$TAVG_station)))
  names(tmp) <- c("intercept", "model", "r.squared", "mae", "mse-s", "mse-u",
                  "mean_obs", "sd_obs", "mean_model", "sd_model", "d1","n")
  lm_temp_results[[i]] <- tmp
}
lm_temp_results <- as.data.frame(do.call(rbind, lm_temp_results))
lm_temp_results <- lm_temp_results %>% mutate("mse"=(`mse-s`+`mse-u`)) %>% 
                                       mutate("rmse"=sqrt(mse)) %>% 
                                       mutate("mse-s/mse"=`mse-s`/mse*100) %>% 
                                       mutate("mse-u/mse"=`mse-u`/mse*100) %>% 
                                       mutate("rmse-s"=sqrt(`mse-s`)) %>% 
                                       mutate("rmse-u"=sqrt(`mse-u`))  
lm_temp_results$id = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                      "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia",
                      "Ensemble Sth America", "Ensemble Africa", "Ensemble SE Asia")
write_csv(lm_temp_results, path="results/stats_temp.csv")

# colours = c("blue", "blue", "blue", "forestgreen", "forestgreen", "forestgreen",
#             "darkorchid4", "darkorchid4", "darkorchid4","red", "red","red")
# #plot
# par(mfrow=c(4,3), pty='s',mai = c(0.3, 0.5, 0.1, 0.2), font=2) 
# for (i in 1:length(y)) {
#   plot(y[[i]]$TAVG_station, y[[i]]$TAVG_model, 
#      xlab = "", ylab = "",
#      grid(), pch=20, col=addTrans(colours[i],80), bg = "blue")
#      abline(0,1)
#      abline(lm_temp_results$intercept[i],lm_temp_results$model[i], col = "black", lty = "dashed", lwd=1.2)
#      legend("topleft", cex=0.9, bty="n", y.intersp = 0.6, 
#             legend=c((paste("r2 =", format(lm_temp_results$r.squared[i], digits = 2))), 
#                     (paste("MAE =", format(lm_temp_results$mae[i], digits = 2))),
#                     (paste("RMSE =", format(lm_temp_results$rmse[i], digits = 2))))                                                            )
# }

#--ggplotting--

#combine data frames into a single df for easy facet plotting
tempScatter_all <- bind_rows(y, .id="id")
tempScatter_all <- left_join(tempScatter_all, lm_temp_results, by="id") #joining summary stats
#reorder the factors so they plot in facetwrap the way I want
tempScatter_all$id <- factor(tempScatter_all$id, 
                             levels = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                                        "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia",
                                        "Ensemble Sth America", "Ensemble Africa", "Ensemble SE Asia")) 
#plot
ggplot(data=tempScatter_all, aes(x=TAVG_station, y=TAVG_model, colour=id)) + 
  facet_wrap(~id, scale="free",ncol=3)+
  geom_point(alpha=0.5, size=1, fill=NA, shape=21) +
  stat_smooth(method = "lm", linetype="longdash", col="black", size=0.6)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Temp. (°C)")+
  ylab("Model Temp. (°C)")+
  theme(legend.position="none")+
  theme(strip.background =element_rect(fill="peachpuff2"))
  # geom_text(aes(label=paste("R2=", round(r.squared,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=1.2, size=3.5)+
  # geom_text(aes(label=paste("MAE=", round(mae,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=2.4, size=3.5)

#--------------------QQplots-----------

#Function to define the percentiles I want for a clean qq plot
QQ.Function_temp <- function(x){
  quants_station <- x %>% 
  summarise(`0.1%`=quantile(TAVG_station, probs=0.001, na.rm=T),
            `1%`=quantile(TAVG_station, probs=0.01, na.rm=T),
            `5%`=quantile(TAVG_station, probs=0.05, na.rm=T),
            `10%`=quantile(TAVG_station, probs=0.10, na.rm=T),
            `25%`=quantile(TAVG_station, probs=0.25, na.rm=T),
            `50%`=quantile(TAVG_station, probs=0.50, na.rm=T),
            `75%`=quantile(TAVG_station, probs=0.75, na.rm=T),
            `90%`=quantile(TAVG_station, probs=0.90, na.rm=T),
            `95%`=quantile(TAVG_station, probs=0.95, na.rm=T),
            `99%`=quantile(TAVG_station, probs=0.99, na.rm=T),
            `99.9%`=quantile(TAVG_station, probs=0.999, na.rm=T))
             # n=length(x$TAVG_station[!is.na(x$TAVG_station)]))
  quants_station <- quants_station %>% t() %>% as.data.frame()
  quants_model <- x %>% 
  summarise(`0.1%`=quantile(TAVG_model, probs=0.001, na.rm=T),
            `1%`=quantile(TAVG_model, probs=0.01, na.rm=T),
            `5%`=quantile(TAVG_model, probs=0.05, na.rm=T),
            `10%`=quantile(TAVG_model, probs=0.10, na.rm=T),
            `25%`=quantile(TAVG_model, probs=0.25, na.rm=T),
            `50%`=quantile(TAVG_model, probs=0.50, na.rm=T),
            `75%`=quantile(TAVG_model, probs=0.75, na.rm=T),
            `90%`=quantile(TAVG_model, probs=0.90, na.rm=T),
            `95%`=quantile(TAVG_model, probs=0.95, na.rm=T),
            `99%`=quantile(TAVG_model, probs=0.99, na.rm=T),
            `99.9%`=quantile(TAVG_model, probs=0.999, na.rm=T))
            # n=length(x$TAVG_model[!is.na(x$TAVG_model)]))
  quants_model <- quants_model %>% t() %>% as.data.frame()
  qq_df <- data.frame(quants_station$V1, quants_model$V1)
  colnames(qq_df) <- c("station", "model")
  rownames(qq_df) <- c("0.1%","1%","5%","10%", "25%", "50%", "75%", "90%", 
                      "95%", "99%", "99.9%")
  qq_df
}

#South America
erai_qq_SA <- QQ.Function_temp(erai_validation_SA)
merra2_qq_SA <- QQ.Function_temp(merra2_validation_SA)
jra55_qq_SA <- QQ.Function_temp(jra55_validation_SA)
cfsr_qq_SA <- QQ.Function_temp(cfsr_validation_SA)

#Africa
erai_qq_A <- QQ.Function_temp(erai_validation_A)
merra2_qq_A <- QQ.Function_temp(merra2_validation_A)
jra55_qq_A <- QQ.Function_temp(jra55_validation_A)
cfsr_qq_A <- QQ.Function_temp(cfsr_validation_A)

#SE Asia
erai_qq_SEA <- QQ.Function_temp(erai_validation_SEA)
merra2_qq_SEA <- QQ.Function_temp(merra2_validation_SEA)
jra55_qq_SEA <- QQ.Function_temp(jra55_validation_SEA)
cfsr_qq_SEA <- QQ.Function_temp(cfsr_validation_SEA)

#plot
qq_list = list("ERA-I Sth America"=erai_qq_SA, "ERA-I Africa"=erai_qq_A, "ERA-I SE Asia"=erai_qq_SEA,
               "MERRA2 Sth America"=merra2_qq_SA, "MERRA2 Africa"=merra2_qq_A, "MERRA2 SE Asia"=merra2_qq_SEA,
               "JRA-55 Sth America"=jra55_qq_SA, "JRA-55 Africa"=jra55_qq_A, "JRA-55 SE Asia"=jra55_qq_SEA,
               "CFSR Sth America"=cfsr_qq_SA, "CFSR Africa"=cfsr_qq_A, "CFSR SE Asia"=cfsr_qq_SEA)

#plotting with base R
# names=c("ERA-I SA", "ERA-I A", "ERA-I SEA", "MERRA2 SA", "MERRA2 A", "MERRA2 SEA", 
#         "JRA-55 SA", "JRA-55 A", "JRA-55 SEA","CFSR SA", "CFSR A", "CFSR SEA")
# 
# par(mfrow=c(4,3), pty='s',mai = c(0.3, 0.5, 0.1, 0.2), font=2) 
# for (i in 1:length(qq_list)) {
#   plot(qq_list[[i]]$station, qq_list[[i]]$model, 
#       pch=21, col="blue", bg = "blue", xlab = "", grid(),
#       ylab = ""); lines(qq_list[[i]]$station, qq_list[[i]]$model, col="blue")
#       abline(0,1)
#       legend("topleft", cex=1, bty="n", legend=names[i])
# }

#plotting with ggplot and facet wrap--

#combine data frames into a single df for easy facet plotting
qq_all <- bind_rows(qq_list, .id="id")
#reorder the id factors so they plot in the facet wrap the way I want
qq_all$id <- factor(qq_all$id, levels = c("ERA-I Sth America", "ERA-I Africa", "ERA-I SE Asia", "MERRA2 Sth America", "MERRA2 Africa", "MERRA2 SE Asia", 
                                        "JRA-55 Sth America", "JRA-55 Africa", "JRA-55 SE Asia","CFSR Sth America", "CFSR Africa", "CFSR SE Asia")) 
qq_all$percentile <- rep(c("0.1","1","5","10", "25", "50", "75", "90", "95", "99", "99.9"), 12)

#plot
ggplot(data=qq_all, aes(x=station, y=model, label=percentile))+
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(colour="blue")+
  geom_line(colour="blue")+
  geom_text(hjust = 0,nudge_x = 0.35, size=3)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Temp. (°C)")+
  ylab("Model Temp. (°C)")+
  theme(strip.background =element_rect(fill="peachpuff2"))
  






#----data wrangling the HADCRU station data-----------------
x <- read_csv("data/temp/A/hadcru/akim_oda.csv")
x <- x %>% select(-Year,-DJF, -MAM, -JJA, -SON,-Annual)
x <- as.data.frame(t(x))
colnames(x) <- rep(LETTERS[1:24], times=1) 
x <- stack(x)
date_CRU <- format(seq(as.Date('1967/01/1'), as.Date('1990/12/1'), by='month'), '%Y%m')
x <- data.frame(DATE=date_CRU,TAVG=x$values, NAME="akim_oda")
x$TAVG[x$TAVG<0] = NA 
write_csv(x, path="data/temp/A/hadcru/results/akim_oda.csv")














































erai_validation_SA %>% mutate(g=TAVG_station - TAVG_model) %>% summarise(mean(g, na.rm=T))

