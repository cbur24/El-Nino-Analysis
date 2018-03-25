library(caret); library(gbm); library(tidyverse); library(lubridate); 
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
source("R/metutils.R")
source("R/Import_SAFE.R")

#--- Join the ERA-Interim with the met observations
era_safe <- read_csv("data/ERA_I/SAFE_ERAI_multiplevariables.csv")
era_safe <- era_safe %>% rename(dateTime=date) %>% select(-X1)
era_safe <- era_safe %>% mutate(year=year(dateTime), month=month(dateTime), doy=yday(dateTime), 
                              hour=hour(dateTime))
### --- validity check
# era_safe %>% mutate(hour=hour(date)) %>% group_by(hour) %>% 
# summarize(u=mean(t2m)) %>% ggplot(data=., aes(hour, u))+geom_path()
es_buck <- function(tempK){
  tempC <- tempK - 273.15
  return(0.61121*exp((18.678 - (tempC)/234.5)*(tempC/(257.14+tempC))))
}
era_safe$rh.mod <- get.rh(T = era_safe$t2m, Td = era_safe$d2m)
era_safe$vpd.mod <- (get.vpd(rh = era_safe$rh.mod, temp = (era_safe$t2m-273.15))/10)

safe_j <- left_join(safe, era_safe, by=c("dateTime","year","month","doy","hour"))
safe_j = safe_j %>% select(-par) #as there are known issues with par in the ERA-I dataset

# --- CROSS VALIDATION OPTION
# this is a method of cross validation where training samples are collected sequentially 
# using the initial window horizon steps, 
# and then tested with the sequential number of horizon steps
timeSliceFolds <- trainControl(method="timeslice",
                               initialWindow = 3000,
                               horizon = 1000,
                               fixedWindow = T,
                               verboseIter = T,
                               skip=1000)

# --- TUNING PARAMETER FOR THE GRADIENT BOOSTING MODEL 
# I've (Sami) have explored a large range of the these parameters and this combination generally 
# works well for the number of observations we are likely to have in the met record gapfilling. 
# They might not work as well for other types of data, with fewer observations. 
parGrid <- expand.grid(n.trees=c(500),interaction.depth=c(25),shrinkage=c(0.02),n.minobsinnode=c(5))

# --- Air Temperature mean, max, min -----------------------------------------------------------------
gbm_safe_Tmean <- train(temp.obs~., 
                                  data=safe_j %>% select(-dateTime, -rh.obs, -sw.obs, -vpd.obs, -doy, -year),
                                  verbose=T, method="gbm", tuneGrid=parGrid, 
                                  trControl=timeSliceFolds, 
                                  preProcess=c("scale","center"),
                                  na.action=na.omit, 
                                  # weights = abs(scale(vpd.obs)+1), 
                                  distribution=list(name="laplace", metric="mae"))
gbm_safe_Tmean
gbm_safe_Tmean %>% varImp %>% plot

gbm_safe_Tmax <- train(temp.obs~.,
                              data=safe_j %>% select(-dateTime, -rh.obs, -sw.obs, -vpd.obs, -doy, -year),
                              verbose=T, method="gbm", tuneGrid=parGrid, 
                              trControl=timeSliceFolds, 
                              preProcess=c("scale","center"),
                              na.action=na.omit, 
                              distribution=list(name="quantile",alpha=0.975))
gbm_safe_Tmax
gbm_safe_Tmax %>% varImp %>% plot

gbm_safe_Tmin <- train(temp.obs~.,
                      data=safe_j %>% select(-dateTime, -rh.obs, -sw.obs, -vpd.obs, -doy, -year),
                      verbose=T, method="gbm", tuneGrid=parGrid, 
                      trControl=timeSliceFolds, 
                      preProcess=c("scale","center"),
                      na.action=na.omit, 
                      distribution=list(name="quantile",alpha=0.025))
gbm_safe_Tmin
gbm_safe_Tmin %>% varImp %>% plot


# --- VPD mean, max -----------------------------------------------------------------
gbm_safe_VPDmean <- train(vpd.obs~., 
                       data=safe_j %>% select(-dateTime, -rh.obs, -sw.obs, -temp.obs, -doy, -year),
                       verbose=T, method="gbm", tuneGrid=parGrid, 
                       trControl=timeSliceFolds, 
                       preProcess=c("scale","center"),
                       na.action=na.omit, 
                       # weights = abs(scale(vpd.obs)+1), 
                       distribution=list(name="laplace", metric="mae"))
gbm_safe_VPDmean
gbm_safe_VPDmean %>% varImp %>% plot

gbm_safe_VPDmax <- train(vpd.obs~., 
                         data=safe_j %>% select(-dateTime, -rh.obs, -sw.obs, -temp.obs, -doy, -year),
                         verbose=T, method="gbm", tuneGrid=parGrid, 
                         trControl=timeSliceFolds, 
                         preProcess=c("scale","center"),
                         na.action=na.omit, 
                         # weights = abs(scale(vpd.obs)+1), 
                         distribution=list(name="quantile", alpha=0.975))
gbm_safe_VPDmax
gbm_safe_VPDmax %>% varImp %>% plot

# --- Shortwave mean -----------------------------------------------------------------
gbm_safe_SWmean <- train(sw.obs~., 
                         data=safe_j %>% select(-dateTime, -rh.obs, -vpd.obs, -temp.obs, -doy, -year),
                         verbose=T, method="gbm", tuneGrid=parGrid, 
                         trControl=timeSliceFolds, 
                         preProcess=c("scale","center"),
                         na.action=na.omit, 
                         # weights = abs(scale(vpd.obs)+1), 
                         distribution=list(name="laplace", metric="mae"))
gbm_safe_SWmean
gbm_safe_SWmean %>% varImp %>% plot

# ------------------------------------------------------------------------------------------ 
# Generate downscaled predictions from ERA-Interim data
era_safe <- era_safe %>% filter(is.na(dateTime)==F)
era_safe$Tmean.pred <- predict(gbm_safe_Tmean, newdata = era_safe)
era_safe$Tmax.pred <- predict(gbm_safe_Tmax, newdata = era_safe)
era_safe$Tmin.pred <- predict(gbm_safe_Tmin, newdata = era_safe)
era_safe$VPDmean.pred <- predict(gbm_safe_VPDmean, newdata = era_safe)
era_safe$VPDmax.pred <- predict(gbm_safe_VPDmax, newdata = era_safe)
era_safe$SWmean.pred <- predict(gbm_safe_SWmean, newdata = era_safe)

# ------------------------------------------------------------------------------------------ 
# Generate downscaled predictions from ERA-Interim data
if(isNamespaceLoaded("plyr")==T){try(unloadNamespace("plyr"), silent=T)} # unload the damn plyr package

# ERA-Interim record
era_safe_monthly <- era_safe %>% group_by(year,month) %>% 
  summarize(Tmean=mean(Tmean.pred), Tmax=max(Tmax.pred), Tmin=min(Tmin.pred), 
            VPDmean=mean(VPDmean.pred), VPDmax=max(VPDmax.pred), 
            SWmean=mean(SWmean.pred)) %>% 
  mutate(date=lubridate::parse_date_time(paste(year,month,15), "ymd"))

#--- met observed record
safe_monthly <- safe %>% group_by(year,month) %>% 
  summarize(Tmean=mean(temp.obs, na.rm =T), Tmax=max(temp.obs, na.rm =T), Tmin=min(temp.obs, na.rm =T), 
            VPDmean=mean(vpd.obs, na.rm =T), VPDmax=max(vpd.obs, na.rm =T), 
            SWmean=mean(sw.obs, na.rm =T), nobs=n()) %>% 
  mutate(date=lubridate::parse_date_time(paste(year,month,15), "ymd"))

#--- joined for purposes of assessing gapfilling 
safe_monthly_assess <- left_join(safe_monthly, era_safe_monthly, by="date", suffix=c(".obs",".pred"))

with(safe_monthly_assess, caret::postResample(pred=Tmean.pred, obs=Tmean.obs))
with(safe_monthly_assess, caret::postResample(pred=Tmin.pred, obs=Tmin.obs))
with(safe_monthly_assess, caret::postResample(pred=Tmax.pred, obs=Tmax.obs))
with(safe_monthly_assess, caret::postResample(pred=VPDmean.pred, obs=VPDmean.obs))
with(safe_monthly_assess, caret::postResample(pred=VPDmax.pred, obs=VPDmax.obs))
with(safe_monthly_assess, caret::postResample(pred=SWmean.pred, obs=SWmean.obs))

# write output to file 
write_csv(safe_monthly_assess, path = "outputs/assessments/SAFE_erai_assessment.csv")
write_csv(era_safe_monthly, path = "outputs/SAFE_gapfill_erai_monthly_2005_2016.csv")


# era_safe_monthly %>% 
#   ggplot(data=., aes(date, Tmean, color=month))+geom_path()+geom_smooth(method="lm")+
#   viridis::scale_color_viridis(option = "B")
# era_safe_monthly %>%
#  ggplot(data=., aes(date, VPDmax, color=month))+geom_path()+geom_smooth(method="lm")+
#  viridis::scale_color_viridis(option = "B")

