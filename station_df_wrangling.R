

library(ncdf4);library(RNetCDF);library(raster);library(tidyverse); library(modelr)

#import in netcdf files----------------------------------------

#ERAI
a = "data/reanalysis/ERAI_1980_2016_monthly_tmean.nc"
ERAI_tmean = stack(a, varname = "t2m")
ERAI_tmean = rotate(ERAI_tmean)
ERAI_tmean = setNames(ERAI_tmean, full_dates)
ERAI_tmean = ERAI_tmean - 273.15

#MERRA2
b = "data/reanalysis/MERRA2_1980_2016_monthly_tmean.nc"
MERRA2_tmean = stack(b, varname = "T2MMEAN")
MERRA2_tmean <- setNames(MERRA2_tmean, full_dates)
MERRA2_tmean = MERRA2_tmean - 273.15

#CRU
c = "data/reanalysis/CRU_1980_2016_monthly_tmean.nc"
CRU_tmean = stack(c, varname = "tmp")
CRU_tmean <- setNames(CRU_tmean, full_dates)

#JRA55
d = "data/reanalysis/jra55_1980_2016_monthly_tmean.nc"
jra55_tmean = stack(d, varname = "TMP_GDS4_HTGL")
crs(jra55_tmean) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
jra55_tmean <- setNames(jra55_tmean, full_dates)
jra55_tmean = jra55_tmean - 273.15

#cfsr
a = "data/reanalysis/cfsr_1980_2016_tmean.nc"
cfsr_tmean = stack(a, varname = "TMP_L103")
cfsr_tmean = cfsr_tmean - 273.15

#-------------------------------------------------------------
#bring in station files with observed temperatures 

setwd("C:/Users/Chad/Desktop/internship/station_comparisons/data/temp") #setwd to where the .csv files are located

file_list <- list.files() 
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

temp.obs = dataset                                                      #rename df
remove(dataset) 
temp.obs = temp.obs[,-1]                                                #remove duplicate column at start of df (no idea why it duplicated??)
stationnames = gsub("\\.csv$","", list.files(pattern="\\.csv$"))        #extract list of station names from filenames
colnames(temp.obs) = stationnames                                       #rename columns in df
full_dates <- seq(as.Date("1980/1/1"), by = "month", length.out = 444)  #create date column
temp.obs = data.frame(date = full_dates, temp.obs)                      #join 'date' df to temp.obs df 

setwd("C:/Users/Chad/Desktop/internship/station_comparisons/")# reset wd

#--------------------------------------------------------------

#import extraction sites .csv
extraction_sites_temp <- read_csv("data/extraction_sites_temp.csv")

# add coordinate system
projection = crs(MERRA2_tmean)

# .csv to R SpatialPointsDataFrame
extraction_sites <- SpatialPointsDataFrame(extraction_sites_temp[,2:3],
                                    extraction_sites_temp,           
                                    proj4string = projection, match.ID =F)    
image(MERRA2_tmean)
points(extraction_sites)

#--------------------------------------------------------------
# extract time series from the each rasterstack 
#merra2
MERRA2_tmean_df = raster::extract(MERRA2_tmean,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
MERRA2_tmean_df = MERRA2_tmean_df %>% t() %>% data.frame()                    #transpose output and convert to df
colnames(MERRA2_tmean_df)=stationnames                                        #adjust the names of the columns to match station names
MERRA2_tmean_df = MERRA2_tmean_df[-1,]                                        #remove the 'ID' row at the top of the dataframe
MERRA2_tmean_df = data.frame(date = full_dates, MERRA2_tmean_df)              #add date column

#ERA-interim
ERAI_tmean_df = raster::extract(ERAI_tmean,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
ERAI_tmean_df = ERAI_tmean_df %>% t() %>% data.frame() 
colnames(ERAI_tmean_df)=stationnames 
ERAI_tmean_df = ERAI_tmean_df[-1,] 
ERAI_tmean_df = data.frame(date = full_dates, ERAI_tmean_df) 

#CRU
CRU_tmean_df = raster::extract(CRU_tmean,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
CRU_tmean_df = CRU_tmean_df %>% t() %>% data.frame() 
colnames(CRU_tmean_df)=stationnames 
CRU_tmean_df = CRU_tmean_df[-1,] 
CRU_tmean_df = data.frame(date = full_dates, CRU_tmean_df) 

#jra55
jra55_tmean_df = raster::extract(jra55_tmean,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
jra55_tmean_df = jra55_tmean_df %>% t() %>% data.frame() 
colnames(jra55_tmean_df)=stationnames 
jra55_tmean_df = jra55_tmean_df[-1,] 
jra55_tmean_df = data.frame(date = full_dates, jra55_tmean_df)

#cfsr
cfsr_tmean_df = raster::extract(cfsr_tmean,extraction_sites,method = 'simple',
                      layer = 1, nl = 444, df = TRUE)
cfsr_tmean_df = cfsr_tmean_df %>% t() %>% data.frame() 
colnames(cfsr_tmean_df)=stationnames 
cfsr_tmean_df = cfsr_tmean_df[-1,] 
cfsr_tmean_df = data.frame(date = full_dates, cfsr_tmean_df)

#-------------------------------------------------------------------------------

#join the temp.obs with each of the different reanalysis dataframes
stationcomparison_ERAI = dplyr::left_join(temp.obs, ERAI_tmean_df, by ="date", suffix = c("_obs", "_model"))
stationcomparison_MERRA2 = dplyr::left_join(temp.obs, MERRA2_tmean_df, by ="date", suffix = c("_obs", "_model"))
stationcomparison_CRU = dplyr::left_join(temp.obs, CRU_tmean_df, by ="date", suffix = c("_obs", "_model"))
stationcomparison_jra55 = dplyr::left_join(temp.obs, jra55_tmean_df, by ="date", suffix = c("_obs", "_model"))
stationcomparison_cfsr = dplyr::left_join(temp.obs, cfsr_tmean_df, by ="date", suffix = c("_obs", "_model"))

#remove the now unnecessary date column
stationcomparison_ERAI = stationcomparison_ERAI %>% select(-date)
stationcomparison_MERRA2 = stationcomparison_MERRA2 %>% select(-date)
stationcomparison_CRU = stationcomparison_CRU %>% select(-date)
stationcomparison_jra55 = stationcomparison_jra55 %>% select(-date)
stationcomparison_cfsr = stationcomparison_cfsr %>% select(-date)

#create seperate dataframes for each continent
#south america
stationcomparison_ERAI_SA = select(stationcomparison_ERAI, belterra_temp_obs, cax_temp_obs, rochambeau_temp_obs, manaus_temp_obs, coari_temp_obs, porto_velho_temp_obs, riberalta_temp_obs, rurrenabaque_temp_obs, cobija_temp_obs, pucallpa_temp_obs, iquitos_temp_obs, sao_gabriel_temp_obs,  zanderij_temp_obs,
                                                           belterra_temp_model, cax_temp_model, rochambeau_temp_model, manaus_temp_model, coari_temp_model, porto_velho_temp_model, riberalta_temp_model, rurrenabaque_temp_model, cobija_temp_model, pucallpa_temp_model, iquitos_temp_model, sao_gabriel_temp_model, zanderij_temp_model)
stationcomparison_MERRA2_SA = select(stationcomparison_MERRA2, belterra_temp_obs, cax_temp_obs, rochambeau_temp_obs, manaus_temp_obs, coari_temp_obs, porto_velho_temp_obs, riberalta_temp_obs, rurrenabaque_temp_obs, cobija_temp_obs, pucallpa_temp_obs, iquitos_temp_obs, sao_gabriel_temp_obs,  zanderij_temp_obs,
                                                               belterra_temp_model, cax_temp_model, rochambeau_temp_model, manaus_temp_model, coari_temp_model, porto_velho_temp_model, riberalta_temp_model, rurrenabaque_temp_model, cobija_temp_model, pucallpa_temp_model, iquitos_temp_model, sao_gabriel_temp_model, zanderij_temp_model)
stationcomparison_CRU_SA = select(stationcomparison_CRU, belterra_temp_obs, cax_temp_obs, rochambeau_temp_obs, manaus_temp_obs, coari_temp_obs, porto_velho_temp_obs, riberalta_temp_obs, rurrenabaque_temp_obs, cobija_temp_obs, pucallpa_temp_obs, iquitos_temp_obs, sao_gabriel_temp_obs,  zanderij_temp_obs,
                                                         belterra_temp_model, cax_temp_model, rochambeau_temp_model, manaus_temp_model, coari_temp_model, porto_velho_temp_model, riberalta_temp_model, rurrenabaque_temp_model, cobija_temp_model, pucallpa_temp_model, iquitos_temp_model, sao_gabriel_temp_model, zanderij_temp_model)
stationcomparison_jra55_SA = select(stationcomparison_jra55, belterra_temp_obs, cax_temp_obs, rochambeau_temp_obs, manaus_temp_obs, coari_temp_obs, porto_velho_temp_obs, riberalta_temp_obs, rurrenabaque_temp_obs, cobija_temp_obs, pucallpa_temp_obs, iquitos_temp_obs, sao_gabriel_temp_obs,  zanderij_temp_obs,
                                                             belterra_temp_model, cax_temp_model, rochambeau_temp_model, manaus_temp_model, coari_temp_model, porto_velho_temp_model, riberalta_temp_model, rurrenabaque_temp_model, cobija_temp_model, pucallpa_temp_model, iquitos_temp_model, sao_gabriel_temp_model, zanderij_temp_model)
stationcomparison_cfsr_SA = select(stationcomparison_cfsr, belterra_temp_obs, cax_temp_obs, rochambeau_temp_obs, manaus_temp_obs, coari_temp_obs, porto_velho_temp_obs, riberalta_temp_obs, rurrenabaque_temp_obs, cobija_temp_obs, pucallpa_temp_obs, iquitos_temp_obs, sao_gabriel_temp_obs,  zanderij_temp_obs,
                                                           belterra_temp_model, cax_temp_model, rochambeau_temp_model, manaus_temp_model, coari_temp_model, porto_velho_temp_model, riberalta_temp_model, rurrenabaque_temp_model, cobija_temp_model, pucallpa_temp_model, iquitos_temp_model, sao_gabriel_temp_model, zanderij_temp_model)

#Africa
stationcomparison_ERAI_A = select(stationcomparison_ERAI, yaounde_temp_obs, berberati_temp_obs, gemena_temp_obs, impfondo_temp_obs, ouesso_temp_obs, souanke_temp_obs, bitam_temp_obs, makokou_temp_obs, kelle_temp_obs, makoua_temp_obs, lastoursville_temp_obs, lambarene_temp_obs, mouila_temp_obs, franceville_temp_obs, tchibanga_temp_obs,
                                                          yaounde_temp_model, berberati_temp_model, gemena_temp_model, impfondo_temp_model, ouesso_temp_model, souanke_temp_model, bitam_temp_model, makokou_temp_model, kelle_temp_model, makoua_temp_model, lastoursville_temp_model, lambarene_temp_model, mouila_temp_model, franceville_temp_model, tchibanga_temp_model)
stationcomparison_MERR2_A = select(stationcomparison_MERRA2, yaounde_temp_obs, berberati_temp_obs, gemena_temp_obs, impfondo_temp_obs, ouesso_temp_obs, souanke_temp_obs, bitam_temp_obs, makokou_temp_obs, kelle_temp_obs, makoua_temp_obs, lastoursville_temp_obs, lambarene_temp_obs, mouila_temp_obs, franceville_temp_obs, tchibanga_temp_obs,
                                                          yaounde_temp_model, berberati_temp_model, gemena_temp_model, impfondo_temp_model, ouesso_temp_model, souanke_temp_model, bitam_temp_model, makokou_temp_model, kelle_temp_model, makoua_temp_model, lastoursville_temp_model, lambarene_temp_model, mouila_temp_model, franceville_temp_model, tchibanga_temp_model)
stationcomparison_CRU_A = select(stationcomparison_CRU, yaounde_temp_obs, berberati_temp_obs, gemena_temp_obs, impfondo_temp_obs, ouesso_temp_obs, souanke_temp_obs, bitam_temp_obs, makokou_temp_obs, kelle_temp_obs, makoua_temp_obs, lastoursville_temp_obs, lambarene_temp_obs, mouila_temp_obs, franceville_temp_obs, tchibanga_temp_obs,
                                                        yaounde_temp_model, berberati_temp_model, gemena_temp_model, impfondo_temp_model, ouesso_temp_model, souanke_temp_model, bitam_temp_model, makokou_temp_model, kelle_temp_model, makoua_temp_model, lastoursville_temp_model, lambarene_temp_model, mouila_temp_model, franceville_temp_model, tchibanga_temp_model) 
stationcomparison_jra55_A = select(stationcomparison_jra55, yaounde_temp_obs, berberati_temp_obs, gemena_temp_obs, impfondo_temp_obs, ouesso_temp_obs, souanke_temp_obs, bitam_temp_obs, makokou_temp_obs, kelle_temp_obs, makoua_temp_obs, lastoursville_temp_obs, lambarene_temp_obs, mouila_temp_obs, franceville_temp_obs, tchibanga_temp_obs,
                                                          yaounde_temp_model, berberati_temp_model, gemena_temp_model, impfondo_temp_model, ouesso_temp_model, souanke_temp_model, bitam_temp_model, makokou_temp_model, kelle_temp_model, makoua_temp_model, lastoursville_temp_model, lambarene_temp_model, mouila_temp_model, franceville_temp_model, tchibanga_temp_model) 
stationcomparison_cfsr_A = select(stationcomparison_cfsr, yaounde_temp_obs, berberati_temp_obs, gemena_temp_obs, impfondo_temp_obs, ouesso_temp_obs, souanke_temp_obs, bitam_temp_obs, makokou_temp_obs, kelle_temp_obs, makoua_temp_obs, lastoursville_temp_obs, lambarene_temp_obs, mouila_temp_obs, franceville_temp_obs, tchibanga_temp_obs,
                                                          yaounde_temp_model, berberati_temp_model, gemena_temp_model, impfondo_temp_model, ouesso_temp_model, souanke_temp_model, bitam_temp_model, makokou_temp_model, kelle_temp_model, makoua_temp_model, lastoursville_temp_model, lambarene_temp_model, mouila_temp_model, franceville_temp_model, tchibanga_temp_model)

# #South-East-Asia
# stationcomparison_ERAI_SEA = select(stationcomparison_ERAI, sibu_temp_obs, safe_temp_obs, muaratewe_temp_obs, lockhart_temp_obs,
#                                                             sibu_temp_model, safe_temp_model, muaratewe_temp_model, lockhart_temp_model)
# stationcomparison_MERRA2_SEA = select(stationcomparison_MERRA2,sibu_temp_obs, safe_temp_obs, muaratewe_temp_obs, lockhart_temp_obs,
#                                                                sibu_temp_model, safe_temp_model, muaratewe_temp_model, lockhart_temp_model)
# stationcomparison_CRU_SEA = select(stationcomparison_CRU, sibu_temp_obs, safe_temp_obs, muaratewe_temp_obs, lockhart_temp_obs,
#                                                           sibu_temp_model, safe_temp_model, muaratewe_temp_model, lockhart_temp_model)
# stationcomparison_jra55_SEA = select(stationcomparison_jra55, sibu_temp_obs, safe_temp_obs, muaratewe_temp_obs, lockhart_temp_obs,
#                                                               sibu_temp_model, safe_temp_model, muaratewe_temp_model, lockhart_temp_model)
# stationcomparison_cfsr_SEA = select(stationcomparison_cfsr, sibu_temp_obs, safe_temp_obs, muaratewe_temp_obs, lockhart_temp_obs,
#                                                             sibu_temp_model, safe_temp_model, muaratewe_temp_model, lockhart_temp_model)

#reorder dataframe so columns are in alphabetical order (this will make it easier to plot everything)
#erai
stationcomparison_ERAI_SA = stationcomparison_ERAI_SA[, order(names(stationcomparison_ERAI_SA))]
stationcomparison_ERAI_A = stationcomparison_ERAI_A[, order(names(stationcomparison_ERAI_A))]
stationcomparison_ERAI_SEA = stationcomparison_ERAI_SEA[, order(names(stationcomparison_ERAI_SEA))]
#merra2
stationcomparison_MERRA2_SA = stationcomparison_MERRA2_SA[, order(names(stationcomparison_MERRA2_SA))]
stationcomparison_MERRA2_A = stationcomparison_MERRA2_A[, order(names(stationcomparison_MERRA2_A))]
stationcomparison_MERRA2_SEA = stationcomparison_MERRA2_SEA[, order(names(stationcomparison_MERRA2_SEA))]
#cru
stationcomparison_CRU_SA = stationcomparison_CRU_SA[, order(names(stationcomparison_CRU_SA))]
stationcomparison_CRU_A = stationcomparison_CRU_A[, order(names(stationcomparison_CRU_A))]
stationcomparison_CRU_SEA = stationcomparison_CRU_SEA[, order(names(stationcomparison_CRU_SEA))]
#jra55
stationcomparison_jra55_SA = stationcomparison_jra55_SA[, order(names(stationcomparison_jra55_SA))]
stationcomparison_jra55_A = stationcomparison_jra55_A[, order(names(stationcomparison_jra55_A))]
stationcomparison_jra55_SEA = stationcomparison_jra55_SEA[, order(names(stationcomparison_jra55_SEA))]
#cfsr
stationcomparison_cfsr_SA = stationcomparison_cfsr_SA[, order(names(stationcomparison_cfsr_SA))]
stationcomparison_cfsr_A = stationcomparison_cfsr_A[, order(names(stationcomparison_cfsr_A))]
stationcomparison_cfsr_SEA = stationcomparison_cfsr_SEA[, order(names(stationcomparison_cfsr_SEA))]


#combine all stations for each continent to produce an overall statistic
#era-interim
tmp = stationcomparison_ERAI_SA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() #place all obs into one long vector 
tmp1 = stationcomparison_ERAI_SA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame() #place all reanalysis values into one long vector
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)                        #mistankingly left a '-999' value in the observation dataset
allstations_erai_SA = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_ERAI_A %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_ERAI_A %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)  
allstations_erai_A = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_ERAI_SEA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_ERAI_SEA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)  
allstations_erai_SEA = data.frame(obs=tmp$., model=tmp1$.)

#merra2
tmp = stationcomparison_MERRA2_SA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_MERRA2_SA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame() 
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)          
allstations_MERRA2_SA = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_MERRA2_A %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_MERRA2_A %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
allstations_MERRA2_A = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_MERRA2_SEA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_MERRA2_SEA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. =ifelse(tmp$. < 0, NA, tmp$.)
allstations_MERRA2_SEA = data.frame(obs=tmp$., model=tmp1$.)

#cru
tmp = stationcomparison_CRU_SA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_CRU_SA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame() 
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)          
allstations_CRU_SA = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_CRU_A %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_CRU_A %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
allstations_CRU_A = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_CRU_SEA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame()
tmp1 = stationcomparison_CRU_SEA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
ifelse(tmp$. < 0, NA, tmp$.)
allstations_CRU_SEA = data.frame(obs=tmp$., model=tmp1$.)

#jra55
tmp = stationcomparison_jra55_SA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() #place all obs into one long vector 
tmp1 = stationcomparison_jra55_SA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame() #place all reanalysis values into one long vector
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)          #mistankingly left a '-999' value in the observation dataset
allstations_jra55_SA = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_jra55_A %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
tmp1 = stationcomparison_jra55_A %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
allstations_jra55_A = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_jra55_SEA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
tmp1 = stationcomparison_jra55_SEA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
allstations_jra55_SEA = data.frame(obs=tmp$., model=tmp1$.)

#cfsr
tmp = stationcomparison_cfsr_SA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() #place all obs into one long vector 
tmp1 = stationcomparison_cfsr_SA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame() #place all reanalysis values into one long vector
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)          #mistankingly left some '-999' value in the observation dataset
allstations_cfsr_SA = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_cfsr_A %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame() 
tmp1 = stationcomparison_cfsr_A %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.) 
allstations_cfsr_A = data.frame(obs=tmp$., model=tmp1$.)

tmp = stationcomparison_cfsr_SEA %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame()
tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
tmp1 = stationcomparison_cfsr_SEA %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
allstations_cfsr_SEA = data.frame(obs=tmp$., model=tmp1$.)

plot(allstations_erai_A$obs, allstations_erai_A$model)
abline(0:1)




#attempting to do the above programmatically (failed)
z= list(stationcomparison_ERAI_SA, stationcomparison_ERAI_A ,stationcomparison_ERAI_SEA, 
        stationcomparison_MERRA2_SA, stationcomparison_MERRA2_A, stationcomparison_MERRA2_SEA,
        stationcomparison_CRU_SA ,stationcomparison_CRU_A ,stationcomparison_CRU_SEA, 
        stationcomparison_jra55_SA ,stationcomparison_jra55_A,stationcomparison_jra55_SEA, 
        stationcomparison_cfsr_SA , stationcomparison_cfsr_A, stationcomparison_cfsr_SEA )

x=c("erai_SA", "erai_A", "erai_SEA", "MERRA2_SA", "MERRA2_A", "MERRA2_SEA", 
        "CRU_SA", "CRU_A", "CRU_SEA", "jra55_SA", "jra55_A", "jra55_SEA","cfsr_SA", "cfsr_A", "cfsr_SEA")

for (i in 1:length(z)) {
  tmp = z[[i]] %>% select(ends_with("_obs")) %>% unlist() %>% as.data.frame()
  tmp1 = z[[i]] %>% select(ends_with("_model")) %>% unlist() %>% as.data.frame()
  tmp$. = ifelse(tmp$. < 0, NA, tmp$.)
  tmp2 = data.frame(obs=tmp$., model=tmp1$.)
  
  for (f in 1:length(x)){
    fileName <- x[[f]]}
  
  newname[[f]] = paste("all_", fileName, sep="")
  assign(newname[[f]], tmp2)
}

tmp1 = z[[1]] %>% select(ends_with("temp_model")) %>% unlist() %>% as.data.frame()











