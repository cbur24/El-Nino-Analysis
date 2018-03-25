

library(ncdf4);library(RNetCDF);library(raster);library(tidyverse);library(lubridate); 
library(maptools); library(hydroGOF)

#-------------------import and manipulate in netcdf files----------------------------------------

#TRMM
a = "data/reanalysis/TRMM3B42_1998_2016_monthly.nc4"
trmm = stack(a, varname = "precipitation")
crs(trmm) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
trmm = t(trmm)
trmm = flip(trmm, direction = 1)
trmm = flip(trmm, direction = 2)

#CHIRPS (on a 0.25 grid to match trmm)
a = "data/reanalysis/chirps_1981_2017_monthly_025.nc"
chirps = stack(a, varname = "precip")

#persiann CDR
a = "data/reanalysis/persiannCDR_1983_2016.nc"
persiann = stack(a, varname="precip")
crs(persiann) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#forest mask
a = "data/reanalysis/forestmask_025.nc"
forest = raster(a, varname = "tropical_forest_mask")

#bring in world continents shapefile
continents = readShapeSpatial("C:/Users/Chad/Desktop/internship/land_mask/data/world_continents.shp")
crs(continents) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
 
extent_tropics <- as(raster::extent(-105.0, 180, 21.0, -21.0), "SpatialPolygons")
proj4string(extent_tropics) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
trmm = crop(trmm, extent_tropics) 
chirps = crop(chirps, extent_tropics, progress="text")
persiann = crop(persiann, extent_tropics, progress = "text")
continents = crop(continents, extent_tropics)
forest = crop(forest, extent_tropics)

#------------------bind all of the data from the stations into one dataframe#####

## South America ----
setwd("data/prcp/SA/") 
list = list.files(getwd())
prcp_stationData_SA <- do.call(rbind, lapply(list, read_csv))
prcp_stationData_SA$DATE <- parse_date_time(prcp_stationData_SA$DATE, "ymd",tz="UTC", truncated = 3) #convert dates
prcp_stationData_SA = distinct(prcp_stationData_SA)
x <- prcp_stationData_SA %>%
   filter(!is.na(PRCP)) %>%
   count(NAME) %>% 
   filter(n<12)
prcp_stationData_SA <- prcp_stationData_SA %>% filter(!NAME %in% x$NAME)
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")
stationCount_SA <- prcp_stationData_SA %>% filter(!is.na(PRCP)) %>% count(NAME)
write_csv(stationCount_SA, path="data/prcp/stationCount_SA.csv")

# tmp = prcp_stationData_SA %>% filter(DATE >= as.Date("1998-01-01")) #filter out the years that TRMM doesn't have
# tmp1 = tmp %>% select(NAME, LATITUDE, LONGITUDE)
# tmp1 = distinct(tmp1)

## Africa ----
setwd("data/prcp/A/") 
list = list.files(getwd())
prcp_stationData_A <- do.call(rbind, lapply(list, read_csv))
prcp_stationData_A$DATE <- parse_date_time(prcp_stationData_A$DATE, "ymd",tz="UTC", truncated = 3) #convert dates
prcp_stationData_A = distinct(prcp_stationData_A)
x <- prcp_stationData_A %>%
   filter(!is.na(PRCP)) %>%
   count(NAME) %>% 
   filter(n<=1)
prcp_stationData_A <- prcp_stationData_A %>% filter(!NAME %in% (x$NAME))
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")
stationCount_A <- prcp_stationData_A %>% filter(!is.na(PRCP)) %>% count(NAME)
write_csv(stationCount_A, path="data/prcp/stationCount_A.csv")

# tmp = prcp_stationData_A %>% filter(DATE >= as.Date("1998-01-01")) #filter out the years that TRMM doesn't have
# tmp1 = tmp %>% select(NAME, LATITUDE, LONGITUDE)
# tmp1 = distinct(tmp1)

## SE Asia ----
setwd("data/prcp/SEA/") 
list = list.files(getwd())
prcp_stationData_SEA <- do.call(rbind, lapply(list, read_csv))
prcp_stationData_SEA$DATE <- parse_date_time(prcp_stationData_SEA$DATE, "ymd",tz="UTC", truncated = 3) #convert dates
prcp_stationData_SEA = distinct(prcp_stationData_SEA)
x <- prcp_stationData_SEA %>%
   filter(!is.na(PRCP)) %>%
   count(NAME) %>% 
   filter(n<=5)
prcp_stationData_SEA <- prcp_stationData_SEA %>% filter(!NAME %in% x$NAME)
setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")
stationCount_SEA <- prcp_stationData_SEA %>% filter(!is.na(PRCP)) %>% count(NAME)
write_csv(stationCount_SEA, path="data/prcp/stationCount_SEA.csv")

# tmp = prcp_stationData_SEA %>% filter(DATE >= as.Date("1998-01-01")) #filter out the years that TRMM doesn't have
# tmp1 = tmp %>% select(NAME, LATITUDE, LONGITUDE)
# tmp1 = distinct(tmp1)


#------------------extract time series from the each rasterstacks------------------- 

#site locations
projection = crs(chirps)

sites_SA = prcp_stationData_SA %>% select(NAME, LATITUDE, LONGITUDE)
sites_SA = distinct(sites_SA)
sites_SA_spatial = SpatialPointsDataFrame(sites_SA[,3:2], sites_SA, 
                                          proj4string = projection, match.ID =F)

sites_A = prcp_stationData_A %>% select(NAME, LATITUDE, LONGITUDE)
sites_A = distinct(sites_A)
sites_A_spatial = SpatialPointsDataFrame(sites_A[,3:2], sites_A, 
                                          proj4string = projection, match.ID =F)

sites_SEA = prcp_stationData_SEA %>% select(NAME, LATITUDE, LONGITUDE)
sites_SEA = distinct(sites_SEA)
sites_SEA_spatial = SpatialPointsDataFrame(sites_SEA[,3:2], sites_SEA, 
                                          proj4string = projection, match.ID =F)

write_csv(sites_SA, path="data/prcp/sites_prcp_SA.csv")
write_csv(sites_A, path="data/prcp/sites_prcp_A.csv")
write_csv(sites_SEA, path="data/prcp/sites_prcp_SEA.csv")


#extract timeseries(I wrote a function for this in the temp script)
#South America----
trmm_SA_df = raster::extract(trmm,sites_SA_spatial,method = 'simple',df = TRUE) #extract from the trmm using the locations of stations
trmm_SA_df = trmm_SA_df %>% t() %>% data.frame()  #transpose output and convert to df
colnames(trmm_SA_df)= sites_SA$NAME               #adjust the names of the columns to match station names
trmm_SA_df = trmm_SA_df[-1,]                      #remove the 'ID' row at the top of the dataframe
trmm_SA_df = stack(trmm_SA_df)                    #stack the columns into one row so I can join it with the station data
dates_trmm <- rep(as.POSIXlt(seq(ymd_hms('1998-01-01-00-00-00'),          #create a repeating sequnce of dates to add to the stack of 
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=248) #stations (will use the date column to join this table with the station table)
trmm_SA_df = data.frame(DATE=dates_trmm, trmm_SA_df)
colnames(trmm_SA_df) = c("DATE", "PRCP", "NAME") 

#chirps
chirps_SA_df = raster::extract(chirps,sites_SA_spatial,method = 'simple',df = TRUE) 
chirps_SA_df = chirps_SA_df %>% t() %>% data.frame() 
colnames(chirps_SA_df)= sites_SA$NAME               
chirps_SA_df = chirps_SA_df[-1,]                      
chirps_SA_df = stack(chirps_SA_df)      
dates_chirps <- rep(as.POSIXlt(seq(ymd_hms('1981-01-01-00-00-00'),  
              ymd_hms('2017-04-01-00-00-00'), by = 'months')), times=248)
chirps_SA_df = data.frame(DATE=dates_chirps, chirps_SA_df)
colnames(chirps_SA_df) = c("DATE", "PRCP", "NAME") 

#persiann
persiann_SA_df = raster::extract(persiann,sites_SA_spatial,method = 'simple',df = TRUE) 
persiann_SA_df = persiann_SA_df %>% t() %>% data.frame() 
colnames(persiann_SA_df)= sites_SA$NAME               
persiann_SA_df = persiann_SA_df[-1,]                      
persiann_SA_df = stack(persiann_SA_df)      
dates_persiann <- rep(as.POSIXlt(seq(ymd_hms('1983-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=248)
persiann_SA_df = data.frame(DATE=dates_persiann, persiann_SA_df)
colnames(persiann_SA_df) = c("DATE", "PRCP", "NAME") 
persiann_SA_df$PRCP[persiann_SA_df$PRCP<0] = NA  #change the -99.99 values to NA

#Africa----
#trmm
trmm_A_df = raster::extract(trmm,sites_A_spatial,method = 'simple',df = TRUE) 
trmm_A_df = trmm_A_df %>% t() %>% data.frame()  
colnames(trmm_A_df)= sites_A$NAME               
trmm_A_df = trmm_A_df[-1,]                      
trmm_A_df = stack(trmm_A_df)                   
dates_trmm <- rep(as.POSIXlt(seq(ymd_hms('1998-01-01-00-00-00'),          
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=29) #"times=" the number of stations extracted
trmm_A_df = data.frame(DATE=dates_trmm, trmm_A_df)
colnames(trmm_A_df) = c("DATE", "PRCP", "NAME") 

#chirps
chirps_A_df = raster::extract(chirps,sites_A_spatial,method = 'simple',df = TRUE) 
chirps_A_df = chirps_A_df %>% t() %>% data.frame() 
colnames(chirps_A_df)= sites_A$NAME               
chirps_A_df = chirps_A_df[-1,]                      
chirps_A_df = stack(chirps_A_df)      
dates_chirps <- rep(as.POSIXlt(seq(ymd_hms('1981-01-01-00-00-00'),  
              ymd_hms('2017-04-01-00-00-00'), by = 'months')), times=29)
chirps_A_df = data.frame(DATE=dates_chirps, chirps_A_df)
colnames(chirps_A_df) = c("DATE", "PRCP", "NAME") 

#persiann
persiann_A_df = raster::extract(persiann,sites_A_spatial,method = 'simple',df = TRUE) 
persiann_A_df = persiann_A_df %>% t() %>% data.frame() 
colnames(persiann_A_df)= sites_A$NAME               
persiann_A_df = persiann_A_df[-1,]                      
persiann_A_df = stack(persiann_A_df)      
dates_persiann <- rep(as.POSIXlt(seq(ymd_hms('1983-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=29)
persiann_A_df = data.frame(DATE=dates_persiann, persiann_A_df)
colnames(persiann_A_df) = c("DATE", "PRCP", "NAME") 
persiann_A_df$PRCP[persiann_A_df$PRCP<0] = NA  #change the -99.99 values to NA

#SE Asia----
#trmm
trmm_SEA_df = raster::extract(trmm,sites_SEA_spatial,method = 'simple',df = TRUE) 
trmm_SEA_df = trmm_SEA_df %>% t() %>% data.frame()  
colnames(trmm_SEA_df)= sites_SEA$NAME               
trmm_SEA_df = trmm_SEA_df[-1,]                      
trmm_SEA_df = stack(trmm_SEA_df)                   
dates_trmm <- rep(as.POSIXlt(seq(ymd_hms('1998-01-01-00-00-00'),          
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=40)
trmm_SEA_df = data.frame(DATE=dates_trmm, trmm_SEA_df)
colnames(trmm_SEA_df) = c("DATE", "PRCP", "NAME") 

#chirps
chirps_SEA_df = raster::extract(chirps,sites_SEA_spatial,method = 'simple',df = TRUE) 
chirps_SEA_df = chirps_SEA_df %>% t() %>% data.frame() 
colnames(chirps_SEA_df)= sites_SEA$NAME               
chirps_SEA_df = chirps_SEA_df[-1,]                      
chirps_SEA_df = stack(chirps_SEA_df)      
dates_chirps <- rep(as.POSIXlt(seq(ymd_hms('1981-01-01-00-00-00'),  
              ymd_hms('2017-04-01-00-00-00'), by = 'months')), times=40)
chirps_SEA_df = data.frame(DATE=dates_chirps, chirps_SEA_df)
colnames(chirps_SEA_df) = c("DATE", "PRCP", "NAME") 

#persiann
persiann_SEA_df = raster::extract(persiann,sites_SEA_spatial,method = 'simple',df = TRUE) 
persiann_SEA_df = persiann_SEA_df %>% t() %>% data.frame() 
colnames(persiann_SEA_df)= sites_SEA$NAME               
persiann_SEA_df = persiann_SEA_df[-1,]                      
persiann_SEA_df = stack(persiann_SEA_df)      
dates_persiann <- rep(as.POSIXlt(seq(ymd_hms('1983-01-01-00-00-00'),  
              ymd_hms('2016-12-01-00-00-00'), by = 'months')), times=40)
persiann_SEA_df = data.frame(DATE=dates_persiann, persiann_SEA_df)
colnames(persiann_SEA_df) = c("DATE", "PRCP", "NAME") 
persiann_SEA_df$PRCP[persiann_SEA_df$PRCP<0] = NA  #change the -99.99 values to NA


#------------------join station data with the gridded climate data by date---------- 

#south America ----
trmm_validation_SA = left_join(prcp_stationData_SA, trmm_SA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
chirps_validation_SA = left_join(prcp_stationData_SA, chirps_SA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
persiann_validation_SA = left_join(prcp_stationData_SA, persiann_SA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
# length(persiann_validation_SA$PRCP_persiann[!is.na(persiann_validation_SA$PRCP_persiann)]) #check the number of observations

#filter from 1998 to divide the analysis to the pre and post PMW 
chirps_validation_SA_98 = chirps_validation_SA %>% filter(DATE >= as.Date("1998-01-01"))
persiann_validation_SA_98 = persiann_validation_SA %>% filter(DATE >= as.Date("1998-01-01"))

#Africa-----
trmm_validation_A = left_join(prcp_stationData_A, trmm_A_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
chirps_validation_A = left_join(prcp_stationData_A, chirps_A_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
persiann_validation_A = left_join(prcp_stationData_A, persiann_A_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
# length(trmm_validation_A$PRCP_trmm[!is.na(trmm_validation_A$PRCP_trmm)]) #check the number of observations

#filter from 1998 to divide the analysis to the pre and post PMW-
chirps_validation_A_98 = chirps_validation_A %>% filter(DATE >= as.Date("1998-01-01"))
persiann_validation_A_98 = persiann_validation_A %>% filter(DATE >= as.Date("1998-01-01"))

# SE Asia-----
trmm_validation_SEA = left_join(prcp_stationData_SEA, trmm_SEA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
chirps_validation_SEA = left_join(prcp_stationData_SEA, chirps_SEA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
persiann_validation_SEA = left_join(prcp_stationData_SEA, persiann_SEA_df, by = c("DATE", "NAME"), suffix=c("_station", "_model"))
# length(trmm_validation_SEA$PRCP_trmm[!is.na(trmm_validation_SEA$PRCP_trmm)]) #check the number of observations

#filter from 1998 to divide the analysis to the pre and post PMW sensors
chirps_validation_SEA_98 = chirps_validation_SEA %>% filter(DATE >= as.Date("1998-01-01"))
persiann_validation_SEA_98 = persiann_validation_SEA %>% filter(DATE >= as.Date("1998-01-01"))




#------------------PLOTS (matching the process for temperature)
#---scatterplots------

#create a list of the dataframes to work with
rainScatter_list=list("TRMM Sth America"=trmm_validation_SA,"TRMM Africa"=trmm_validation_A,"TRMM SE Asia"=trmm_validation_SEA,
                      "CHIRPS Sth America"=chirps_validation_SA,"CHIRPS Africa"=chirps_validation_A,"CHIRPS SE Asia"=chirps_validation_SEA,
                      "PERSIANNCDR Sth America"=persiann_validation_SA, "PERSIANNCDR Africa"=persiann_validation_A,"PERSIANNCDR SE Asia"=persiann_validation_SEA)

#linear models
lm_rain_results <- list()
for (i in 1:length(rainScatter_list)) {
  lm_obj <- lm(PRCP_model ~ PRCP_station, data = rainScatter_list[[i]])
  tmp <- c(lm_obj$coefficients,
           summary(lm_obj)$r.squared,
           mae(rainScatter_list[[i]]$PRCP_station, rainScatter_list[[i]]$PRCP_model),
           # rmse(rainScatter_list[[i]]$PRCP_station, rainScatter_list[[i]]$PRCP_model),
           mse_s((coef(lm_obj)[1] + (coef(lm_obj)[2]*rainScatter_list[[i]]$PRCP_station)), rainScatter_list[[i]]$PRCP_station),
           mse_u(rainScatter_list[[i]]$PRCP_model, (coef(lm_obj)[1] + (coef(lm_obj)[2]*rainScatter_list[[i]]$PRCP_station))),
           mean(rainScatter_list[[i]]$PRCP_station, na.rm=T),
           sd(rainScatter_list[[i]]$PRCP_station, na.rm=T),
           mean(rainScatter_list[[i]]$PRCP_model, na.rm=T),
           sd(rainScatter_list[[i]]$PRCP_model, na.rm=T),
           hydroGOF::md(rainScatter_list[[i]]$PRCP_model, rainScatter_list[[i]]$PRCP_station),
           length(na.omit(rainScatter_list[[i]]$PRCP_station)))
  names(tmp) <- c("intercept", "model", "r.squared", "mae", "mse-s", "mse-u",
                  "mean_obs", "sd_obs", "mean_model", "sd_model", "d1","n")
  lm_rain_results[[i]] <- tmp
}
lm_rain_results <- as.data.frame(do.call(rbind, lm_rain_results))
lm_rain_results <- lm_rain_results %>% mutate("mse"=(`mse-s`+`mse-u`)) %>% 
                                       mutate("rmse"=sqrt(mse)) %>%  
                                       mutate("mse-s/mse"=`mse-s`/mse*100) %>% 
                                       mutate("mse-u/mse"=`mse-u`/mse*100) %>% 
                                       mutate("rmse-s"=sqrt(`mse-s`)) %>% 
                                       mutate("rmse-u"=sqrt(`mse-u`))  
lm_rain_results$id = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                       "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                      "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")
write_csv(lm_rain_results, path="results/stats_rain.csv")

#combine data frames into a single df for easy facet plotting
rainScatter_all <- bind_rows(rainScatter_list, .id="id")
rainScatter_all <- left_join(rainScatter_all, lm_rain_results, by="id") #joining summary stats
#reorder the factors so they plot in facetwrap the way I want
rainScatter_all$id <- factor(rainScatter_all$id, levels = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                                                            "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                                                            "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")) 

#plot
ggplot(data=rainScatter_all, aes(x=PRCP_station, y=PRCP_model, colour=id)) + 
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(alpha=0.5, size=1, fill=NA, shape=21) +
  stat_smooth(method = "lm", linetype="longdash", col="black", size=0.6)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Precip. (mm/month)")+
  ylab("Model Precip. (mm/month)")+
  theme(legend.position="none")+
  theme(strip.background =element_rect(fill="peachpuff2"))
  # geom_text(aes(label=paste("R2=", round(r.squared,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=1.2, size=3.5)+
  # geom_text(aes(label=paste("MAE=", round(mae,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=2.4, size=3.5)+
  # geom_text(aes(label=paste("RMSE=", round(rmse,digits=2), sep = "")),col="grey30",
  #           x=-Inf, y=Inf, hjust=-0.2, vjust=3.6, size=3.5)

#------qqplots----------
#Function to define the percentiles I want for a clean qq plot
QQ.Function_rain <- function(x){
  quants_station <- x %>% 
  summarise(`1%`=quantile(PRCP_station, probs=0.001, na.rm=T),
            `25%`=quantile(PRCP_station, probs=0.25, na.rm=T),
            `50%`=quantile(PRCP_station, probs=0.50, na.rm=T),
            `75%`=quantile(PRCP_station, probs=0.75, na.rm=T),
            `90%`=quantile(PRCP_station, probs=0.90, na.rm=T),
            `95%`=quantile(PRCP_station, probs=0.95, na.rm=T),
            `99%`=quantile(PRCP_station, probs=0.99, na.rm=T),
            `99.9%`=quantile(PRCP_station, probs=0.999, na.rm=T))
             # n=length(x$PRCP_station[!is.na(x$PRCP_station)]))
  quants_station <- quants_station %>% t() %>% as.data.frame()
  quants_model <- x %>% 
  summarise(`1%`=quantile(PRCP_model, probs=0.001, na.rm=T),
            `25%`=quantile(PRCP_model, probs=0.25, na.rm=T),
            `50%`=quantile(PRCP_model, probs=0.50, na.rm=T),
            `75%`=quantile(PRCP_model, probs=0.75, na.rm=T),
            `90%`=quantile(PRCP_model, probs=0.90, na.rm=T),
            `95%`=quantile(PRCP_model, probs=0.95, na.rm=T),
            `99%`=quantile(PRCP_model, probs=0.99, na.rm=T),
            `99.9%`=quantile(PRCP_model, probs=0.999, na.rm=T))
            # n=length(x$PRCP_model[!is.na(x$PRCP_model)]))
  quants_model <- quants_model %>% t() %>% as.data.frame()
  qq_df <- data.frame(quants_station$V1, quants_model$V1)
  colnames(qq_df) <- c("station", "model")
  rownames(qq_df) <- c("1%","25%","50%", "75%", "90%", "95%", "99%", "99.9%")
  qq_df
}

#South America
trmm_qq_SA <- QQ.Function_rain(trmm_validation_SA)
chirps_qq_SA <- QQ.Function_rain(chirps_validation_SA)
persiann_qq_SA <- QQ.Function_rain(persiann_validation_SA)

#Africa
trmm_qq_A <- QQ.Function_rain(trmm_validation_A)
chirps_qq_A <- QQ.Function_rain(chirps_validation_A)
persiann_qq_A <- QQ.Function_rain(persiann_validation_A)

#SE Asia
trmm_qq_SEA <- QQ.Function_rain(trmm_validation_SEA)
chirps_qq_SEA <- QQ.Function_rain(chirps_validation_SEA)
persiann_qq_SEA <- QQ.Function_rain(persiann_validation_SEA)

#plot
qq_list_rain = list("TRMM Sth America"=trmm_qq_SA, "TRMM Africa"=trmm_qq_A, "TRMM SE Asia"=trmm_qq_SEA,
                    "CHIRPS Sth America"=chirps_qq_SA, "CHIRPS Africa"=chirps_qq_A, "CHIRPS SE Asia"=chirps_qq_SEA,
                    "PERSIANNCDR Sth America"=persiann_qq_SA, "PERSIANNCDR Africa"=persiann_qq_A, "PERSIANNCDR SE Asia"=persiann_qq_SEA)

#plotting with ggplot and facet wrap

#combine data frames into a single df for easy facet plotting
qq_all_rain <- bind_rows(qq_list_rain, .id="id")

#reorder the id factors so they plot in the facet wrap the way I want
qq_all_rain$id <- factor(qq_all_rain$id, levels = c("TRMM Sth America", "TRMM Africa", "TRMM SE Asia", 
                                                     "CHIRPS Sth America","CHIRPS Africa","CHIRPS SE Asia", 
                                                      "PERSIANNCDR Sth America", "PERSIANNCDR Africa","PERSIANNCDR SE Asia")) 
qq_all_rain$percentile <- rep(c("1","25","50", "75", "90", "95", "99", "99.9"), 9)

#plot
ggplot(data=qq_all_rain, aes(x=station, y=model, label=percentile))+
  facet_wrap(~id, scale="free", ncol=3)+
  geom_point(colour="blue")+
  geom_line(colour="blue")+
  geom_text(hjust = -0.3, nudge_x = 0.5, size=3)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  xlab("Station Precip. (mm/month)")+
  ylab("Model Precip. (mm/month)")+
  theme(strip.background =element_rect(fill="peachpuff2"))















#old base R plots ##########
#SA----
plot(persiann_validation_SA$PRCP_station, persiann_validation_SA$PRCP_persiann, main="PersiannCDR Amazonia",
     xlab="station (mm/month)", ylab="PersiannCDR (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(persiann_validation_SA$PRCP_persiann~persiann_validation_SA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(chirps_validation_SA$PRCP_station, chirps_validation_SA$PRCP_chirps, main="CHIRPS Amazonia",
     xlab="station (mm/month)", ylab="CHIRPS (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(chirps_validation_SA$PRCP_chirps~chirps_validation_SA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(trmm_validation_SA$PRCP_station, trmm_validation_SA$PRCP_trmm, main="TRMM 3B42 V7 Amazonia",
     xlab="station (mm/month)", ylab="TRMM (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(trmm_validation_SA$PRCP_trmm~trmm_validation_SA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

#plots from 98 onwards only
plot(persiann_validation_SA_98$PRCP_station, persiann_validation_SA_98$PRCP_persiann, main="PersiannCDR Amazonia",
     xlab="station (mm/month)", ylab="PersiannCDR (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(persiann_validation_SA_98$PRCP_persiann~persiann_validation_SA_98$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(chirps_validation_SA_98$PRCP_station, chirps_validation_SA_98$PRCP_chirps, main="chirps SA '98")
abline(lm(chirps_validation_SA_98$PRCP_chirps~chirps_validation_SA_98$PRCP_station))
abline(0:1)

#-Africa----
plot(persiann_validation_A$PRCP_station, persiann_validation_A$PRCP_persiann, main="PersiannCDR Africa",
     xlab="station (mm/month)", ylab="PersiannCDR (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(persiann_validation_A$PRCP_persiann~persiann_validation_A$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(chirps_validation_A$PRCP_station, chirps_validation_A$PRCP_chirps, main="CHIRPS Africa",
     xlab="station (mm/month)", ylab="CHIRPS (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(chirps_validation_A$PRCP_chirps~chirps_validation_A$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(trmm_validation_A$PRCP_station, trmm_validation_A$PRCP_trmm, main="TRMM 3B42 V7 Africa",
     xlab="station (mm/month)", ylab="TRMM (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(trmm_validation_A$PRCP_trmm~trmm_validation_A$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

#plots from 98 onwards only
plot(persiann_validation_A_98$PRCP_station, persiann_validation_A_98$PRCP_persiann, main="persiann A '98")
abline(lm(persiann_validation_A_98$PRCP_persiann~persiann_validation_A_98$PRCP_station))
abline(0:1)

plot(chirps_validation_A_98$PRCP_station, chirps_validation_A_98$PRCP_chirps, main="chirps A '98")
abline(lm(chirps_validation_A_98$PRCP_chirps~chirps_validation_A_98$PRCP_station))
abline(0:1)

#SEA----
plot(persiann_validation_SEA$PRCP_station, persiann_validation_SEA$PRCP_persiann, main="PersiannCDR SE Asia",
     xlab="station (mm/month)", ylab="PersiannCDR (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(persiann_validation_SEA$PRCP_persiann~persiann_validation_SEA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(chirps_validation_SEA$PRCP_station, chirps_validation_SEA$PRCP_chirps, main="CHIRPS SE Asia",
     xlab="station (mm/month)", ylab="CHIRPS (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(chirps_validation_SEA$PRCP_chirps~chirps_validation_SEA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(trmm_validation_SEA$PRCP_station, trmm_validation_SEA$PRCP_trmm, main="TRMM 3B42 V7 SE Asia",
     xlab="station (mm/month)", ylab="TRMM (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(trmm_validation_SEA$PRCP_trmm~trmm_validation_SEA$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

#plots from 98 onwards only
plot(persiann_validation_SEA_98$PRCP_station, persiann_validation_SEA_98$PRCP_persiann, main="PersiannCDR SE Asia '98",
     xlab="station (mm/month)", ylab="PersiannCDR (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(persiann_validation_SEA_98$PRCP_persiann~persiann_validation_SEA_98$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

plot(chirps_validation_SEA_98$PRCP_station, chirps_validation_SEA_98$PRCP_chirps, main="CHIRPS SE Asia '98",
     xlab="station (mm/month)", ylab="CHIRPS (mm/month)", cex=0.8, col=addTrans("blue",100))
abline(lm(chirps_validation_SEA_98$PRCP_chirps~chirps_validation_SEA_98$PRCP_station), lty = "dashed", lwd=1.2)
abline(0:1)

































