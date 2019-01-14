#***** IMPORT GHANA BOB-01 MET STATION DATA ******************************************************
library(readr); library(dplyr); library(lubridate); library(ggplot2); library(tidyr)
source("R/metutils.R")
if(isNamespaceLoaded("plyr")==T){try(unloadNamespace("plyr"), silent=T)}


#new!. This data messy. date format alternates between dmy and mdy! hence I've got some operations in here to 
# fix it, but there may still be problems
bob <- readr::read_csv("data/site_data/BOB/bob_data_subset.csv") #seems to be in dmy format?
bob$date <- lubridate::parse_date_time(paste(bob$Date,bob$Time), "dmy HMS")
bob2 <- bob[is.na(bob$date)==T, ] # put the unparsed date rows in a new df
bob1 <- bob[-is.na(bob$date)==F,] # delete the bad rows and put in bob1
bob2$date <- lubridate::parse_date_time(paste(bob2$Date, bob2$Time), "mdy HMS")
bob <- bind_rows(bob1,bob2)
bob <- bob %>% arrange(date)
bob %>% ggplot(data=., aes(date, Temp))+geom_path()
# bob <- (bob %>% filter(is.na(date)==F))
# bob$time <- seconds_to_period(bob$Time)
# bob$dateTime <- as.Date.POSIXct(bob$date) + seconds(bob$time)
# bob$dateTime <- as.POSIXct(bob$dateTime)
bob <- (bob %>% mutate(year=year(date), doy=yday(date), month=month(date), hour=hour(date)))
bob <- (bob %>% select(-one_of(c("Date"))))
bob <- (bob %>% rename(temp.obs=Temp, rh.obs=`Rel Humidity`, windspeed=`Wind speed`, light=Light, precip=`Rain gauge`))
# bob <- bob %>% filter(date != as.POSIXct("2014-01-06"))
bob$vpd.obs <- get.vpd(rh=bob$rh.obs, temp=bob$temp.obs)/10


# remove unneeded columns
if(isNamespaceLoaded("plyr")==T){try(unloadNamespace("plyr"), silent=T)}
bob <- bob %>% select(-Year, -Month, -precip, -Time, -`Wind vane`)
bob <- bob %>% rename(dateTime=date)


# bob %>% ggplot(data=., aes(date, temp.obs, color=hour))+geom_point(cex=0.25)+viridis::scale_color_viridis()
# bob %>% ggplot(data=., aes(date, vpd.obs, color=hour))+geom_point(cex=0.25)+viridis::scale_color_viridis()

# #old, has problems with some months loosing days 
# bob <- read_csv("site_data/BOB/bob_data_subset.csv") #seems to be in dmy format?
# bob$date <- parse_date_time(bob$Date, "dmy")
# bob <- (bob %>% filter(is.na(date)==F))
# bob$time <- seconds_to_period(bob$Time)
# bob$dateTime <- as.Date.POSIXct(bob$date) + seconds(bob$time)
# bob$dateTime <- as.POSIXct(bob$dateTime)
# bob <- (bob %>% mutate(year=year(dateTime), doy=yday(dateTime), month=month(dateTime), hour=hour(dateTime)))
# bob <- (bob %>% select(-one_of(c("Date"))))
# bob <- (bob %>% rename(temp.obs=Temp, rh.obs=`Rel Humidity`, windspeed=`Wind speed`, light=Light, precip=`Rain gauge`))
# bob <- bob %>% filter(date != as.POSIXct("2014-01-06"))
# bob$vpd.obs <- get.vpd(rh=bob$rh.obs, temp=bob$temp.obs)/10

 bob %>% group_by(hour, month) %>% 
   dplyr::summarize(u=mean(vpd.obs, na.rm=T)) %>% 
   ggplot(data=., aes(hour, u, color=as.factor(month)))+geom_path()



# # *** Visual Checks *** #-# *** Visual Checks *** #-# *** Visual Checks *** #-# *** Visual Checks *** #
# bob %>% ggplot(data=., aes(date, light, color=hour)) + geom_point() + scale_color_viridis() # LIGHT HAS PROBLEMS
# bob %>% ggplot(data=., aes(date, temp.obs, color=hour)) + geom_point() + scale_color_viridis() # LIGHT HAS PROBLEMS
# bob %>% ggplot(data=., aes(date, rh.obs, color=hour)) + geom_point() + scale_color_viridis() # LIGHT HAS PROBLEMS
# bob %>% ggplot(data=., aes(date, vpd.obs, color=hour)) + geom_point() + scale_color_viridis() # LIGHT HAS PROBLEMS
# bob %>% ggplot(data=., aes(date, windspeed, color=hour)) + geom_point() + scale_color_viridis() # LIGHT HAS PROBLEMS
# 
# bob %>% filter(light>25 & hour < 6) %>% 
#   ggplot(data=., aes(date, light, color=as.factor(hour))) + geom_point() +
#   scale_color_viridis(discrete=T)
# 
# 
# bob %>% #filter(year>2013 & year<=2014) %>% 
#   ggplot(data=., aes(date, light, color=as.factor(hour))) + #geom_point() +
#   geom_smooth(se=F)+
#   scale_color_viridis(discrete=T)
# 
# bob %>% filter(hour==0 & year==2014 & month==1 & doy==6) %>% 
#   ggplot(data=., aes(date, light, color=as.factor(hour))) + geom_point() +
#   geom_smooth(se=F)+
#   scale_color_viridis(discrete=T)
