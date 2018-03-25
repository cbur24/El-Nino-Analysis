
#Functions required by the code: 

###############################################################

matchupenvironvar = function(RespData,f1,f2){
  
  #Assumes input dataframes RespData, f1 and f2 have a $Time column. 
  #Assumes all three Time columns have the same units.
  #For each row of RespData, findsmatching rows in f1 and f2 with same value in Time column. 
  #Takes cwd and monthPrecip values from f2 and Tmean and VPDmean values from f1 and adds them to the rows in RespData
  #Outputs RespData with additional columns named cwd, precip, temp and vpd. 
  
  #Match up environmental variables
  for (i in 1:length(RespData$Time))
  {for (j in 1:length(f2$Time))
  {if (RespData$Time[i]==f2$Time[j])
  {RespData$cwd[i] = f2$cwd[j]
  RespData$precip[i] = f2$monthPrecip[j]
  }
  }
    for (j in 1:length(f1$Time))
    {if (RespData$Time[i]==f1$Time[j])
    {RespData$temp[i] = f1$Tmean[j]
    RespData$vpd[i] = f1$VPDmean[j]
    }
    }
  }
  
  return(RespData)
  
}

###############################################################

matchupenvironvar0 = function(f1,f2){
  
  #Assumes input dataframes f1 and f2 have a $Time column. 
  #Assumes both Time columns have the same units.
  #For each row of f1, finds matching rows in f2 with same value in Time column. 
  #Outputs array containing matched Tmean and VPDmean values from f1 and cwd and monthPrecip values from f2. 
  
  #Match up environmental variables
  for (i in 1:length(f1$Time))
  {f1$precip[i] = NA
  f1$cwd[i] = NA
  for (j in 1:length(f2$Time))
  {if (f1$Time[i]==f2$Time[j])
  {f1$cwd[i] = f2$cwd[j]
  f1$precip[i] = f2$monthPrecip[j]
  }
  }
  }
  
  matchedarray = data.frame(date=as.Date(f1$MyDate,format="%Y-%m-%d",origin="1970-01-01"),Time=as.numeric(f1$Time),temp=as.numeric(f1$Tmean),vpd=as.numeric(f1$VPDmean),precip=as.numeric(f1$precip),cwd=as.numeric(f1$cwd))
  matchedarray = subset(matchedarray,(is.na(matchedarray$precip)==FALSE))
  
  return(matchedarray)
  
}

###############################################################

adddatecolumn = function(f,dayflag){
  
  #Set dayflag=0 to use day=15 for all dates.
  #Otherwise, set dayflag=1.
  
  #Get dataframe dimensions
  rc = dim(f)
  r = rc[1]
  c = rc[2]
  
  #Find index of year, month and day columns
  yj = match("year",names(f))
  mj = match("month",names(f))
  dj = match("day",names(f))
  
  #Do some checks
  if (is.na(yj)==TRUE)
  {print("No column named year")
  }
  if (is.na(mj)==TRUE)
  {print("No column named month")
  }
  if (dayflag==1)
  {if (is.na(dj)==TRUE)
  {print("No column named day")
  }
  }
  
  #Make date column
  MyDate = data.frame(MyDate=as.Date(character(r),format="%Y-%m-%d",origin="1970-01-01"))
  for (i in 1:r)
  {if (dayflag==0)
  {strdate = paste(as.character(as.integer(f[i,yj])),as.character(as.integer(f[i,mj])),toString(15),sep="-")
  }
    if (dayflag==1)
    {strdate = paste(as.character(as.integer(f[i,yj])),as.character(as.integer(f[i,mj])),as.character(as.integer(f[i,dj])),sep="-")
    }
    datedate = as.Date(strdate,format="%Y-%m-%d",origin="1970-01-01")
    MyDate[i,1] = datedate
  }
  
  #Add date column to original dataframe
  f$MyDate = MyDate$MyDate
  
  return(f)
  
}

###############################################################

addtimecolumn2 = function(f){
  
  #Assumes date column is named date and contains dates in format YYYY-MM-DD. 
  #Adds a time column in units of months since 1970 to the input array
  
  n = length(f$date)
  for (i in 1:n)
  {strdate = as.character(f$date[i])
  splitdate = strsplit(strdate,"-")[[1]]
  f$Time[i] = (as.numeric(splitdate[1])-1970.0)*12.0+as.numeric(splitdate[2])
  }
  
  return(f)
  
}

###############################################################

addstandarddatecolumn = function(f){
  
  #Assumes f2 has a column $Date with dates in format DD/MM/YYYY.
  
  date = data.frame(date=as.Date(character(length(f$Date)),format="%Y-%m-%d",origin="1970-01-01"))
  for (i in 1:length(f$Date))
  {strdate = as.character(f$Date[i])
  splitdate = strsplit(strdate,"/")[[1]]
  if (nchar(splitdate[1])==1)
  {splitdate[1] = paste('0',splitdate[1],sep="")
  }
  if (nchar(splitdate[2])==1)
  {splitdate[2] = paste('0',splitdate[2],sep="")
  }
  date$date[i] = as.Date(paste(splitdate[3],splitdate[2],splitdate[1],sep='-'),format="%Y-%m-%d",origin="1970-01-01")
  }
  
  #Add date column to original dataframe
  f$date = date$date
  
  return(f)
  
}

###############################################################

addcwd = function(f){
  
  #Assumes f is a dataframe with column named $monthPrecip.
  #Calculates cwd from precip data. 
  #Returns original dataframe with an extra column for cwd. 
  
  cwd = data.frame(cwd=numeric(length(f$monthPrecip)))
  cwd$cwd[1] = 0.0
  for (i in 2:length(f$monthPrecip))
  {cwd$cwd[i] = cwd$cwd[i-1]+f$monthPrecip[i]-100.0
  if (cwd$cwd[i]>0.0)
  {cwd$cwd[i] = 0.0
  }
  }
  
  #Add cwd column to original dataframe
  f$cwd = cwd$cwd
  
  return(f)
  
}

###############################################################


#################
# SAFE05 data
#################
#Load in environmental variable datafiles

#f1 contains T and VPD data:
f1 = read.csv("data/downscaled_SAFE_monthly_gldasv21_gbm.csv",header=TRUE,stringsAsFactors=FALSE)
#Add date column
f1 = adddatecolumn(f1,0)
#Add time column in units of months since 1970
f1$Time = (f1$year-1970.0)*12.0+f1$month
#Sort into ascending order (for some reason this file is ordered by month)
f1 = f1[with(f1,order(f1$Time)),]
#print(f1)

#f2 contains Precip
f2 = read.csv("data/danum_metstation_rainfall.csv",header=TRUE,stringsAsFactors=FALSE)
#Use Date column in D/MM/YYYY to make a standard date format column
f2 = addstandarddatecolumn(f2)
#Add time column in units of months since 1970
f2 = addtimecolumn2(f2)
#Rename precip column
colnames(f2)[2] = "monthPrecip"
#Calculate cwd from precip
f2 = addcwd(f2)
#print(f2)

###############################################################

#Load in data from response variables

#Put NPPLitterfall in dataframe L:
f_in = read.csv("data/SAFE_TowerPlot_NPP_forChad_Litterfall.csv",header=TRUE,stringsAsFactors=FALSE)
#Use Date column in D/MM/YYYY to make a standard date format column
f_in = addstandarddatecolumn(f_in)
L_SEA = data.frame(date=as.Date(f_in$date),Y=as.numeric(f_in$Litterfall),Yerr=as.numeric(f_in$StandardError))
#Add time column in units of months since 1970
L_SEA = addtimecolumn2(L_SEA)
#Get matching environmental data
L_SEA = matchupenvironvar(L_SEA,f1,f2)
#print(L)

#Put NPPRoot (from ingrowth cores) in dataframe R:
f_in = read.csv("data/SAFE_TowerPlot_NPP_forChad_NPPRoot.csv",header=TRUE,stringsAsFactors=FALSE)
#Use Date column in D/MM/YYYY to make a standard date format column
f_in = addstandarddatecolumn(f_in)
R_SEA = data.frame(date=as.Date(f_in$date),Y=as.numeric(f_in$FineRootNPP),Yerr=as.numeric(f_in$StandardError))
#Add time column in units of months since 1970
R_SEA = addtimecolumn2(R_SEA)
#Get matching environmental data
R_SEA = matchupenvironvar(R_SEA,f1,f2)
#print(R)

#Put Heterotrophic Soil Respiration in dataframe H:
f_in = read.csv("data/HetSoilRespmeanrawdata_SAF05.csv",header=FALSE,stringsAsFactors=FALSE)
H_SEA = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
H_SEA = addtimecolumn2(H_SEA)
#Get matching environmental data
H_SEA = matchupenvironvar(H_SEA,f1,f2)
#print(H)

#Put NPPWood (from dendrometers) in dataframe W:
f_in = read.csv("data/SAFE_TowerPlot_NPP_forChad_WoodyNPP.csv",header=TRUE,stringsAsFactors=FALSE)
#Rename MeanDate column as Date
colnames(f_in)[3] = 'Date'
#Use Date column in D/MM/YYYY to make a standard date format column
f_in = addstandarddatecolumn(f_in)
W_SEA = data.frame(date=as.Date(f_in$date),Y=as.numeric(f_in$AboveGroundWoodyNPP),Yerr=numeric(length(f_in$date)))
#Add time column in units of months since 1970
W_SEA = addtimecolumn2(W_SEA)
#Get matching environmental data
W_SEA = matchupenvironvar(W_SEA,f1,f2)
#print(W)


##############
# TAM05
##############
#Load in environmental variable datafiles

#f1 contains T and VPD data:
f1 = read.csv("data/downscaled_TAM_monthly_gldasv21_gbm.csv",header=TRUE,stringsAsFactors=FALSE)
#Add date column
f1 = adddatecolumn(f1,0)
#Add time column in units of months since 1970
f1$Time = (f1$year-1970.0)*12.0+f1$month
#Sort into ascending order (for some reason this file is ordered by month)
f1 = f1[with(f1,order(f1$Time)),]
#print(f1)

#f2 contains Precip and cwd data:
f2 = read.csv("data/tam_precip_cwd_trmm.csv",header=TRUE,stringsAsFactors=FALSE)
f2$date = as.Date(f2$date)
#Add time column in units of months since 1970
f2 = addtimecolumn2(f2)
#print(f2)

###############################################################

#Load in data from response variables

#Put NPPLitterfall in dataframe L:
f_in = read.csv("data/TAM-05Litterfall.csv",header=FALSE,stringsAsFactors=FALSE)
L_SA = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
L_SA = addtimecolumn2(L_SA)
#Get matching environmental data
L_SA = matchupenvironvar(L_SA,f1,f2)
#print(L)

#Put NPPRoot (from ingrowth cores) in dataframe R:
f_in = read.csv("data/TAM-05NPPRoot.csv",header=FALSE,stringsAsFactors=FALSE)
R_SA = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
R_SA = addtimecolumn2(R_SA)
#Get matching environmental data
R_SA = matchupenvironvar(R_SA,f1,f2)
#print(R)

#Put Heterotrophic Soil Respiration in dataframe H:
f_in = read.csv("data/TAM-05HetSoilResp.csv",header=FALSE,stringsAsFactors=FALSE)
H_SA = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
H_SA = addtimecolumn2(H_SA)
#Get matching environmental data
H_SA = matchupenvironvar(H_SA,f1,f2)
#print(H)

#Put NPPWood (from dendrometers) in dataframe W:
f_in = read.csv("data/ts_dendrometers_tam05_25July17.csv",header=TRUE,stringsAsFactors=FALSE)
#Add date column
f_in = adddatecolumn(f_in,0)
W_SA = data.frame(date=as.Date(f_in$MyDate),Y=as.numeric(f_in$nppacw_MgC_month),Yerr=as.numeric(f_in$nppacw_MgC_month_se))
#Add time column in units of months since 1970
W_SA = addtimecolumn2(W_SA)
#Get matching environmental data
W_SA = matchupenvironvar(W_SA,f1,f2)
#print(W)

###############################################################

##############
# Bobiri
##############

#Load in environmental variable datafiles

#f1 contains T and VPD data:
f1 = read.csv("data/downscaled_bob_monthly_gldasv21_gbm.csv",header=TRUE,stringsAsFactors=FALSE)
#Add date column
f1 = adddatecolumn(f1,0)
#Add time column in units of months since 1970
f1$Time = (f1$year-1970.0)*12.0+f1$month
#print(f1)

#f2 contains Precip and cwd data:
f2 = read.csv("data/Ghana_Kumase_FORIG_met_data_2002_2016.csv",header=TRUE,stringsAsFactors=FALSE)
f2$date = as.Date(f2$date)
#Add time column in units of months since 1970
f2 = addtimecolumn2(f2)
#print(f2)

###############################################################

#Load in data from response variables

#Put NPPLitterfall in dataframe L:
f_in = read.csv("data/BOB-02Litterfall.csv",header=FALSE,stringsAsFactors=FALSE)
L_A = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
L_A = addtimecolumn2(L_A)
#Get matching environmental data
L_A = matchupenvironvar(L_A,f1,f2)
#print(L)

#Put NPPRoot (from ingrowth cores) in dataframe R:
f_in = read.csv("data/BOB-02NPPRoot.csv",header=FALSE,stringsAsFactors=FALSE)
R_A = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
R_A = addtimecolumn2(R_A)
#Get matching environmental data
R_A = matchupenvironvar(R_A,f1,f2)
#print(R)

#Put Heterotrophic Soil Respiration in dataframe H:
f_in = read.csv("data/BOB-02HetSoilResp.csv",header=FALSE,stringsAsFactors=FALSE)
H_A = data.frame(date=as.Date(f_in$V1),Y=as.numeric(f_in$V2),Yerr=as.numeric(f_in$V3))
#Add time column in units of months since 1970
H_A = addtimecolumn2(H_A)
#Get matching environmental data
H_A = matchupenvironvar(H_A,f1,f2)
#print(H)

#Put NPPWood (from dendrometers) in dataframe W:
f_in = read.csv("data/ts_dendrometers_bob02_25July17.csv",header=TRUE,stringsAsFactors=FALSE)
#Add date column
f_in = adddatecolumn(f_in,0)
W_A = data.frame(date=as.Date(f_in$MyDate),Y=as.numeric(f_in$nppacw_MgC_month),Yerr=as.numeric(f_in$nppacw_MgC_month_se))
#Add time column in units of months since 1970
W_A = addtimecolumn2(W_A)
#Get matching environmental data
W_A = matchupenvironvar(W_A,f1,f2)
#print(W)



##################################################################################
# now bring in the reanalysis netCDF files for each of the environmental variables
##################################################################################

library(raster); library(ncdf4); library(rgdal); library(sp)

#read in and prepare ncdf files
a = "data/MERRA2_2000_2016_monthlymean_VPD_remasked.nc"
MERRA2_VPD_pantropics = stack(a, varname = "VPD")
dates <- format(seq(as.Date('2000/01/1'), as.Date('2016/12/1'), by='month'), '%Y%m')
MERRA2_VPD_pantropics <- setNames(MERRA2_VPD_pantropics, dates)

b = "data/TRMM_CWD_2000_2016_MERRA2GRID_remasked.nc"
TRMM_CWD_pantropics = stack(b, varname = "CWD")
TRMM_CWD_pantropics = setNames(TRMM_CWD_pantropics, dates)

c = "data/TRMM_2000_2016_3B42_monthly_MERRA2GRID_remasked.nc4"
TRMM_pantropics = stack(c, varname = "precipitation")
crs(TRMM_pantropics) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
TRMM_pantropics = t(TRMM_pantropics)
TRMM_pantropics = flip(TRMM_pantropics, direction = 1)
TRMM_pantropics = flip(TRMM_pantropics, direction = 2)
TRMM_pantropics <- setNames(TRMM_pantropics, dates)

d = "data/MERRA2_2000_2016_monthly_t2m_remasked.nc"
MERRA2_T2M_pantropics = stack(d, varname = "T2M")
MERRA2_T2M_pantropics <- setNames(MERRA2_T2M_pantropics, dates)
MERRA2_T2M_pantropics = MERRA2_T2M_pantropics - 273.15

#creating bounding boxes
#SE Asia
extent_SEA <- as(raster::extent(68.0, 177.0, -23.5, 23.5), "SpatialPolygons")
proj4string(extent_SEA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
extent_SEA

#Africa
extent_A <- as(raster::extent(-25.0, 60.0, -23.5, 23.5), "SpatialPolygons")
proj4string(extent_A) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
extent_A

#sthAmerica
extent_SA <- as(raster::extent(-120.0, -28.0, -23.5, 23.5), "SpatialPolygons")
proj4string(extent_SA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
extent_SA

#extract regions from pantropics file using bounding boxes
#SE Asia
MERRA2_VPD_SEA = crop(MERRA2_VPD_pantropics, extent_SEA)
MERRA2_T2M_SEA = crop(MERRA2_T2M_pantropics, extent_SEA)
TRMM_CWD_SEA = crop(TRMM_CWD_pantropics, extent_SEA)
TRMM_SEA = crop(TRMM_pantropics, extent_SEA)

#adjust rasters to match the observed data at forest site
TRMM_CWD_SEA = calc(TRMM_CWD_SEA, fun = function(x){x[x < -37] = -37; return(x)})
MERRA2_VPD_SEA = calc(MERRA2_VPD_SEA, fun = function(x){x[x > 0.9] = 0.9; return(x)})
MERRA2_VPD_SEA = calc(MERRA2_VPD_SEA, fun = function(x){x[x < 0.4] = 0.4; return(x)})
MERRA2_T2M_SEA = calc(MERRA2_T2M_SEA, fun = function(x){x[x > 27] = 27; return(x)})
MERRA2_T2M_SEA = calc(MERRA2_T2M_SEA, fun = function(x){x[x < 24] = 24; return(x)})
TRMM_SEA = calc(TRMM_SEA, fun = function(x){x[x < 63] = 63; return(x)})
TRMM_SEA = calc(TRMM_SEA, fun = function(x){x[x > 444] = 444; return(x)})

#Africa
MERRA2_VPD_A = crop(MERRA2_VPD_pantropics, extent_A)
MERRA2_T2M_A = crop(MERRA2_T2M_pantropics, extent_A)
TRMM_CWD_A = crop(TRMM_CWD_pantropics, extent_A)
TRMM_A = crop(TRMM_pantropics, extent_A)

#adjust rasters to match the observed data at forest site
TRMM_CWD_A = calc(TRMM_CWD_A, fun = function(x){x[x < -446] = -446; return(x)})
MERRA2_VPD_A = calc(MERRA2_VPD_A, fun = function(x){x[x > 1.7] = 1.7; return(x)})
MERRA2_VPD_A = calc(MERRA2_VPD_A, fun = function(x){x[x < 0.3] = 0.3; return(x)})
MERRA2_T2M_A = calc(MERRA2_T2M_A, fun = function(x){x[x > 28] = 28; return(x)})
MERRA2_T2M_A = calc(MERRA2_T2M_A, fun = function(x){x[x < 24] = 24; return(x)})
TRMM_A = calc(TRMM_A, fun = function(x){x[x > 457] = 457; return(x)})

#Sth America
MERRA2_VPD_SA = crop(MERRA2_VPD_pantropics, extent_SA)
MERRA2_T2M_SA = crop(MERRA2_T2M_pantropics, extent_SA)
TRMM_CWD_SA = crop(TRMM_CWD_pantropics, extent_SA)
TRMM_SA = crop(TRMM_pantropics, extent_SA)

#adjust rasters to match the observed data at forest site
TRMM_CWD_SA = calc(TRMM_CWD_SA, fun = function(x){x[x < -276] = -276; return(x)})
MERRA2_VPD_SA = calc(MERRA2_VPD_SA, fun = function(x){x[x > 0.8] = 0.8; return(x)})
MERRA2_VPD_SA = calc(MERRA2_VPD_SA, fun = function(x){x[x < 0.18] = 0.18; return(x)})
MERRA2_T2M_SA = calc(MERRA2_T2M_SA, fun = function(x){x[x > 26.3] = 26.3; return(x)})
MERRA2_T2M_SA = calc(MERRA2_T2M_SA, fun = function(x){x[x < 21.5] = 21.5; return(x)})
TRMM_SA = calc(TRMM_SA, fun = function(x){x[x < 4] = 4; return(x)})
TRMM_SA = calc(TRMM_SA, fun = function(x){x[x > 876] = 876; return(x)})

#convert rasters to dataframes
#SE Asia
MERRA2_VPD_SEA_df <- as.data.frame(MERRA2_VPD_SEA, xy=T) # convert rasterBrick to dataframe with xy coords
MERRA2_T2M_SEA_df <- as.data.frame(MERRA2_T2M_SEA, xy=T)
TRMM_CWD_SEA_df <- as.data.frame(TRMM_CWD_SEA, xy=T) 
TRMM_SEA_df = as.data.frame(TRMM_SEA, xy=T)

#Africa
MERRA2_VPD_A_df <- as.data.frame(MERRA2_VPD_A, xy=T)
MERRA2_T2M_A_df <- as.data.frame(MERRA2_T2M_A, xy=T)
TRMM_CWD_A_df <- as.data.frame(TRMM_CWD_A, xy=T)
TRMM_A_df = as.data.frame(TRMM_A, xy=T)

#SA
MERRA2_VPD_SA_df <- as.data.frame(MERRA2_VPD_SA, xy=T) 
MERRA2_T2M_SA_df <- as.data.frame(MERRA2_T2M_SA, xy=T)
TRMM_CWD_SA_df <- as.data.frame(TRMM_CWD_SA, xy=T)
TRMM_SA_df = as.data.frame(TRMM_SA, xy=T)


########################################################################################
# Generate model predictions for each component of NPP and Rh

library(rstanarm)
#build up each model for each component of NEE model using stan_glm
#SE Asia
model_NPP_LF_SEA = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=L_SEA)
model_NPP_W_SEA = stan_glm(Y~temp+vpd+precip+cwd,family=gaussian(link="log"),data=W_SEA)
model_NPP_R_SEA = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=R_SEA)
model_Rh_SEA = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=H_SEA)

#Africa
model_NPP_LF_A = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=L_A)
model_NPP_W_A = stan_glm(Y~temp+vpd+precip+cwd,family=gaussian(link="log"),data=W_A)
model_NPP_R_A = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=R_A)
model_Rh_A = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=H_A)   

#South America
model_NPP_LF_SA = stan_glm(Y~temp+vpd+precip+cwd,family=gaussian(link="log"),data=L_SA)
model_NPP_W_SA = stan_glm(Y~temp+vpd+precip+cwd,family=gaussian(link="log"),data=W_SA)
model_NPP_R_SA = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=R_SA)
model_Rh_SA = stan_glm(Y~temp+vpd+precip+cwd,family=Gamma(link="log"),data=H_SA)  


#generate predictions for forest sites and compare against observations
#SAFE
#NPP_LF_SEA_site_predict = model_NPP_LF_SEA %>% predict(type = "response") 
#NPP_W_SEA_site_predict = model_NPP_W_SEA %>% predict(type = "response")
#NPP_R_SEA_site_predict = model_NPP_R_SEA %>% predict(type = "response")
#Rh_SEA_site_predict = model_Rh_SEA %>% predict(type = "response")

#Bobiri
#NPP_LF_A_site_predict = model_NPP_LF_A %>% predict(type = "response") 
#NPP_W_A_site_predict = model_NPP_W_A %>% predict(type = "response")
#NPP_R_A_site_predict = model_NPP_R_A %>% predict(type = "response")
#Rh_A_site_predict = model_Rh_A %>% predict(type = "response")

#TAM
#NPP_LF_SA_site_predict = model_NPP_LF_SA %>% predict(type = "response") 
#NPP_W_SA_site_predict = model_NPP_W_SA %>% predict(type = "response")
#NPP_R_SA_site_predict = model_NPP_R_SA %>% predict(type = "response")
#Rh_SA_site_predict = model_Rh_SA %>% predict(type = "response")



##################################################################################
#--- Predict to a raster ---------------------------------------------------------
library(tidyverse); library(rstanarm)

######################
#SE Asia
######################
#---------------------------------------------------------------
#SE Asia: NPP LF
#---------------------------------------------------------------

MERRA2_T2M_SEA_df <- tbl_df(MERRA2_T2M_SEA_df)
# Declare temporary data structures to hold predictions
vec_dates <- names(MERRA2_T2M_SEA_df) # x,y,dates
out_q05 <- na.omit(MERRA2_T2M_SEA_df) # convenient way to get right shape of data frame for 5% cred interval predictions
out_q05[,3:dim(out_q05)[2]] <- NA
out_q95 <- na.omit(MERRA2_T2M_SEA_df) # convenient way to get right shape of data frame for 95% cred interval predictions
out_q95[,3:dim(out_q95)[2]] <- NA
out_q50 <- na.omit(MERRA2_T2M_SEA_df) # convenient way to get right shape of data frame for 50% cred interval predictions
out_q50[,3:dim(out_q50)[2]] <- NA

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SEA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SEA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SEA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SEA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SEA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SEA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_LF_SEA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_LF_SEA_p05 <- rasterFromXYZ(out_q05)
NPP_LF_SEA_p50 <- rasterFromXYZ(out_q50)
NPP_LF_SEA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SE Asia: NPP wood
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SEA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SEA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SEA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SEA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SEA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SEA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_W_SEA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_W_SEA_p05 <- rasterFromXYZ(out_q05)
NPP_W_SEA_p50 <- rasterFromXYZ(out_q50)
NPP_W_SEA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SE Asia: NPP roots
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SEA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SEA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SEA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SEA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SEA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SEA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_R_SEA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_R_SEA_p05 <- rasterFromXYZ(out_q05)
NPP_R_SEA_p50 <- rasterFromXYZ(out_q50)
NPP_R_SEA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SE Asia: Rh
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SEA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SEA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SEA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SEA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SEA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SEA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_Rh_SEA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
Rh_SEA_p05 <- rasterFromXYZ(out_q05)
Rh_SEA_p50 <- rasterFromXYZ(out_q50)
Rh_SEA_p95 <- rasterFromXYZ(out_q95)


######################
#Africa
######################
#---------------------------------------------------------------
#Africa: NPP LF
#---------------------------------------------------------------

MERRA2_T2M_A_df <- tbl_df(MERRA2_T2M_A_df)
# Declare temporary data structures to hold predictions
vec_dates <- names(MERRA2_T2M_A_df) # x,y,dates
out_q05 <- na.omit(MERRA2_T2M_A_df) # convenient way to get right shape of data frame for 5% cred interval predictions
out_q05[,3:dim(out_q05)[2]] <- NA
out_q95 <- na.omit(MERRA2_T2M_A_df) # convenient way to get right shape of data frame for 95% cred interval predictions
out_q95[,3:dim(out_q95)[2]] <- NA
out_q50 <- na.omit(MERRA2_T2M_A_df) # convenient way to get right shape of data frame for 50% cred interval predictions
out_q50[,3:dim(out_q50)[2]] <- NA

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_A_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_A_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_A_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_A_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_A_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_A_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_LF_A, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_LF_A_p05 <- rasterFromXYZ(out_q05)
NPP_LF_A_p50 <- rasterFromXYZ(out_q50)
NPP_LF_A_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# Africa: NPP wood
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_A_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_A_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_A_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_A_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_A_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_A_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_W_A, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_W_A_p05 <- rasterFromXYZ(out_q05)
NPP_W_A_p50 <- rasterFromXYZ(out_q50)
NPP_W_A_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# Africa: NPP roots
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_A_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_A_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_A_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_A_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_A_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_A_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_R_A, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_R_A_p05 <- rasterFromXYZ(out_q05)
NPP_R_A_p50 <- rasterFromXYZ(out_q50)
NPP_R_A_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# Africa: Rh
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_A_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_A_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_A_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_A_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_A_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_A_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_Rh_A, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
Rh_A_p05 <- rasterFromXYZ(out_q05)
Rh_A_p50 <- rasterFromXYZ(out_q50)
Rh_A_p95 <- rasterFromXYZ(out_q95)


######################
#South America
######################
#---------------------------------------------------------------
#SAmerica: NPP LF
#---------------------------------------------------------------

MERRA2_T2M_SA_df <- tbl_df(MERRA2_T2M_SA_df)
# Declare temporary data structures to hold predictions
vec_dates <- names(MERRA2_T2M_SA_df) # x,y,dates
out_q05 <- na.omit(MERRA2_T2M_SA_df) # convenient way to get right shape of data frame for 5% cred interval predictions
out_q05[,3:dim(out_q05)[2]] <- NA
out_q95 <- na.omit(MERRA2_T2M_SA_df) # convenient way to get right shape of data frame for 95% cred interval predictions
out_q95[,3:dim(out_q95)[2]] <- NA
out_q50 <- na.omit(MERRA2_T2M_SA_df) # convenient way to get right shape of data frame for 50% cred interval predictions
out_q50[,3:dim(out_q50)[2]] <- NA

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_LF_SA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_LF_SA_p05 <- rasterFromXYZ(out_q05)
NPP_LF_SA_p50 <- rasterFromXYZ(out_q50)
NPP_LF_SA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SAmerica: NPP wood
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_W_SA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_W_SA_p05 <- rasterFromXYZ(out_q05)
NPP_W_SA_p50 <- rasterFromXYZ(out_q50)
NPP_W_SA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SAmerica: NPP roots
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_NPP_R_SA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
NPP_R_SA_p05 <- rasterFromXYZ(out_q05)
NPP_R_SA_p50 <- rasterFromXYZ(out_q50)
NPP_R_SA_p95 <- rasterFromXYZ(out_q95)

#---------------------------------------------------------------
# SAmerica: Rh
#---------------------------------------------------------------

# For loop to iterate through each date 
for(i in 3:length(vec_dates)){
  print(vec_dates[i]);
  tmp <- data.frame(
    x=na.omit(MERRA2_T2M_SA_df) %>% pull(x), 
    y=na.omit(MERRA2_T2M_SA_df) %>% pull(y),
    temp=na.omit(MERRA2_T2M_SA_df) %>% pull(vec_dates[i]), 
    vpd=na.omit(MERRA2_VPD_SA_df) %>% pull(vec_dates[i]), 
    precip=na.omit(TRMM_SA_df) %>% pull(vec_dates[i]), 
    cwd = na.omit(TRMM_CWD_SA_df) %>% pull(vec_dates[i]))
  
  #draw 4000 param combos from posterior
  pre_mat <- posterior_predict(model_Rh_SA, 
                               newdata= tmp, 
                               type="response", draws = 4000)
  out_q50[,i] <- apply(pre_mat, 2, FUN=median) # take the median (50%quantile)
  out_q05[,i] <- apply(pre_mat, 2, FUN=quantile, 0.05) # 5% credible interval
  out_q95[,i] <- apply(pre_mat, 2, FUN=quantile, 0.95) # 95% credible interval
}
# converts the data frame to a raster as long as there is an x & y column
Rh_SA_p05 <- rasterFromXYZ(out_q05)
Rh_SA_p50 <- rasterFromXYZ(out_q50)
Rh_SA_p95 <- rasterFromXYZ(out_q95)

#-END------------------------------------------------------------------------------
###################################################################################

#model_rh_SEA %>% summary(digits =3); model_rh_SEA %>% plot 
#predictive_interval(model_NPP_LF_SEA, newdata=data.frame(temp=25, vpd=0.2, precip=400, cwd=-36), type="response")[,2]

#calculate regional NEE
#SEA
NEE_SEA_p50 = NPP_LF_SEA_p50 + NPP_W_SEA_p50 + NPP_R_SEA_p50 - Rh_SEA_p50
NEE_SEA_p05 = NPP_LF_SEA_p05 + NPP_W_SEA_p05 + NPP_R_SEA_p05 - Rh_SEA_p05
NEE_SEA_p95 = NPP_LF_SEA_p95 + NPP_W_SEA_p95 + NPP_R_SEA_p95 - Rh_SEA_p95

#Africa
NEE_A_p50 = NPP_LF_A_p50 + NPP_W_A_p50 + NPP_R_A_p50 - Rh_A_p50
NEE_A_p05 = NPP_LF_A_p05 + NPP_W_A_p05 + NPP_R_A_p05 - Rh_A_p05
NEE_A_p95 = NPP_LF_A_p95 + NPP_W_A_p95 + NPP_R_A_p95 - Rh_A_p95

#SAmerica
NEE_SA_p50 = NPP_LF_SA_p50 + NPP_W_SA_p50 + NPP_R_SA_p50 - Rh_SA_p50
NEE_SA_p05 = NPP_LF_SA_p05 + NPP_W_SA_p05 + NPP_R_SA_p05 - Rh_SA_p05
NEE_SA_p95 = NPP_LF_SA_p95 + NPP_W_SA_p95 + NPP_R_SA_p95 - Rh_SA_p95

#take rasters of regional NEE and convert into 2d timeseries
#SEA
NEE_SEA_p50_FLUX_perpixel = NEE_SEA_p50*384659    #multiply by the area (ha) of the pixels
NEE_SEA_p50_FLUX_PgC = ((cellStats(NEE_SEA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NEE_SEA_p05_FLUX_perpixel = NEE_SEA_p05*384659  
NEE_SEA_p05_FLUX_PgC = ((cellStats(NEE_SEA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NEE_SEA_p95_FLUX_perpixel = NEE_SEA_p95*384659  
NEE_SEA_p95_FLUX_PgC = ((cellStats(NEE_SEA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

  #plots
  #NEE
  plot.ts(NEE_SEA_p50_FLUX_PgC, main = "SEA Merra2", ylim = c(-1.5, 0.3))
  lines(NEE_SEA_p05_FLUX_PgC, col = "green")
  lines(NEE_SEA_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()
  #respiration
  Rh_SEA_p05_FLUX_perpixel = Rh_SEA_p05*384659  
  Rh_SEA_p05_FLUX_PgC = ((cellStats(Rh_SEA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  Rh_SEA_p95_FLUX_perpixel = Rh_SEA_p95*384659  
  Rh_SEA_p95_FLUX_PgC = ((cellStats(Rh_SEA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  Rh_SEA_p50_FLUX_perpixel = Rh_SEA_p50*384659  
  Rh_SEA_p50_FLUX_PgC = ((cellStats(Rh_SEA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  plot.ts(Rh_SEA_p50_FLUX_PgC, main= "respiration", ylim = c(-1.5, 0.1))
  lines(Rh_SEA_p05_FLUX_PgC, col = "green")
  lines(Rh_SEA_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()
  #LF
  NPP_LF_SEA_p95_FLUX_perpixel = NPP_LF_SEA_p95*384659  
  NPP_LF_SEA_p95_FLUX_PgC = ((cellStats(NPP_LF_SEA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_LF_SEA_p05_FLUX_perpixel = NPP_LF_SEA_p05*384659  
  NPP_LF_SEA_p05_FLUX_PgC = ((cellStats(NPP_LF_SEA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_LF_SEA_p50_FLUX_perpixel = NPP_LF_SEA_p50*384659  
  NPP_LF_SEA_p50_FLUX_PgC = ((cellStats(NPP_LF_SEA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  plot.ts(NPP_LF_SEA_p50_FLUX_PgC, main= "Litterfall SEA", ylim = c(-1.5, 0.1))
  lines(NPP_LF_SEA_p05_FLUX_PgC, col = "green")
  lines(NPP_LF_SEA_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()
  #Wood
  NPP_W_SEA_p95_FLUX_perpixel = NPP_W_SEA_p95*384659  
  NPP_W_SEA_p95_FLUX_PgC = ((cellStats(NPP_W_SEA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_W_SEA_p05_FLUX_perpixel = NPP_W_SEA_p05*384659  
  NPP_W_SEA_p05_FLUX_PgC = ((cellStats(NPP_W_SEA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_W_SEA_p50_FLUX_perpixel = NPP_W_SEA_p50*384659  
  NPP_W_SEA_p50_FLUX_PgC = ((cellStats(NPP_W_SEA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  plot.ts(NPP_W_SEA_p50_FLUX_PgC, main= "Wood SEA", ylim = c(-1.5, 0.1))
  lines(NPP_W_SEA_p05_FLUX_PgC, col = "green")
  lines(NPP_W_SEA_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()
  #Roots
  NPP_R_SEA_p95_FLUX_PgC = NPP_R_SEA_p95*384659  
  NPP_R_SEA_p95_FLUX_PgC = ((cellStats(NPP_R_SEA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_R_SEA_p05_FLUX_perpixel = NPP_R_SEA_p05*384659  
  NPP_R_SEA_p05_FLUX_PgC = ((cellStats(NPP_R_SEA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  NPP_R_SEA_p50_FLUX_perpixel = NPP_R_SEA_p50*384659  
  NPP_R_SEA_p50_FLUX_PgC = ((cellStats(NPP_R_SEA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)  

  plot.ts(NPP_R_SEA_p50_FLUX_PgC, main= "roots SEA", ylim = c(-1.5, 0.1))
  lines(NPP_R_SEA_p05_FLUX_PgC, col = "green")
  lines(NPP_R_SEA_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()
  
  
#Africa
NEE_A_p50_FLUX_perpixel = NEE_A_p50*384659   
NEE_A_p50_FLUX_PgC = ((cellStats(NEE_A_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1) 

NEE_A_p05_FLUX_perpixel = NEE_A_p05*384659  
NEE_A_p05_FLUX_PgC = ((cellStats(NEE_A_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NEE_A_p95_FLUX_perpixel = NEE_A_p95*384659  
NEE_A_p95_FLUX_PgC = ((cellStats(NEE_A_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

  #plots
  #NEE
  plot.ts(NEE_A_p50_FLUX_PgC, main = "Africa Merra2", ylim=c(-7, 0.1))
  abline(h = 0)
  lines(NEE_A_p05_FLUX_PgC, col = "green")
  lines(NEE_A_p95_FLUX_PgC, col = "red")
  grid()


#SAmerica
NEE_SA_p50_FLUX_perpixel = NEE_SA_p50*384659    
NEE_SA_p50_FLUX_PgC = ((cellStats(NEE_SA_p50_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1) 

NEE_SA_p05_FLUX_perpixel = NEE_SA_p05*384659 
NEE_SA_p05_FLUX_PgC = ((cellStats(NEE_SA_p05_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NEE_SA_p95_FLUX_perpixel = NEE_SA_p95*384659 
NEE_SA_p95_FLUX_PgC = ((cellStats(NEE_SA_p95_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

  #plots
  #NEE
  plot.ts(NEE_SA_p50_FLUX_PgC, main = "SAmerica Merra2", ylim=c(-3.5, 0.3))
  abline(h = 0)
  lines(NEE_SA_p05_FLUX_PgC, col = "green")
  lines(NEE_SA_p95_FLUX_PgC, col = "red")
  grid()


#add regional NEE togther to find pantropical NEE
NEE_pantropical_p50_FLUX_PgC = NEE_SEA_p50_FLUX_PgC + NEE_A_p50_FLUX_PgC + NEE_SA_p50_FLUX_PgC
NEE_pantropical_p05_FLUX_PgC = NEE_SEA_p05_FLUX_PgC + NEE_A_p05_FLUX_PgC + NEE_SA_p05_FLUX_PgC
NEE_pantropical_p95_FLUX_PgC = NEE_SEA_p95_FLUX_PgC + NEE_A_p95_FLUX_PgC + NEE_SA_p95_FLUX_PgC

  #plots
  #NEE pantropics
  plot.ts(NEE_pantropical_p50_FLUX_PgC, main = "Pantropics NEE (MERRRA2)", ylim=c(-9.0, 0.75))
  lines(NEE_pantropical_p05_FLUX_PgC, col = "green")
  lines(NEE_pantropical_p95_FLUX_PgC, col = "red")
  abline(h = 0)
  grid()


#--------------------------------------------------------------------------------------------------

#compare pantropical NEE against observed CGR

library(readr); library(pracma); library(caTools); library(ggplot2) ;
library(lubridate); library(scales) ; library(zoo); library(RcppRoll)

obs_CGR <- read_csv("data/obs_CGR.csv", 
                    col_types = cols(Date = col_date(format = "%d/%m/%Y")))
nino_34 <- read_csv("data/nino34.csv", 
                    col_types = cols(Date = col_date(format = "%d/%m/%Y")))

#remove linear trend from 12 month moving sum of CGR
detrended_CGR = detrend(obs_CGR$CGR_PgC, tt = 'linear')
plot(obs_CGR$Date, detrended_CGR, 'l')
##smooth the CGR with multimonth running average
detrended_CGR = runmean(detrended_CGR, 6, endrule = c("NA"), align = "right")
##smooth the Nino3.4 data with multimonth running average
smoothed_nino34 = runmean(nino_34$Nino3.4, 6, endrule = c("NA"), align = "right")

#detrend modelled monthly series and calculate 12-month moving sums to define IAV
#median
NEE_pantropical_p50_detrend = detrend(NEE_pantropical_p50_FLUX_PgC, tt= 'linear')
NEE_pantropical_p50_IAV = rollsum(NEE_pantropical_p50_detrend, 12, align = "right", fill = NA)
NEE_pantropical_p50_IAV_smooth = runmean(NEE_pantropical_p50_IAV, 6, endrule = c("NA"), align = "right")
#p05
NEE_pantropical_p05_detrend = detrend(NEE_pantropical_p05_FLUX_PgC, tt= 'linear')
NEE_pantropical_p05_IAV = rollsum(NEE_pantropical_p05_detrend, 12, align = "right", fill = NA)
NEE_pantropical_p05_IAV_smooth = runmean(NEE_pantropical_p05_IAV, 6, endrule = c("NA"), align = "right")
#p95
NEE_pantropical_p95_detrend = detrend(NEE_pantropical_p95_FLUX_PgC, tt= 'linear')
NEE_pantropical_p95_IAV = rollsum(NEE_pantropical_p95_detrend, 12, align = "right", fill = NA)
NEE_pantropical_p95_IAV_smooth = runmean(NEE_pantropical_p95_IAV, 6, endrule = c("NA"), align = "right")

  plot.ts(NEE_pantropical_p50_IAV_smooth, ylim=c(-6, 6))
  lines(NEE_pantropical_p05_IAV_smooth, col="green")
  lines(NEE_pantropical_p95_IAV_smooth, col="red")
  lines(detrended_CGR, col="blue")
  abline(h=0)

NEE_vs_CGR_df = data.frame(obs_CGR$Date, NEE_pantropical_p50_IAV_smooth, NEE_pantropical_p05_IAV_smooth,
                           NEE_pantropical_p95_IAV_smooth, detrended_CGR, smoothed_nino34)

#old GLM for MERRA2
oldglm_M2_TRMM <- read_csv("results/carbon_fluxes_MERRA2_old_glm.csv", 
                    col_types = cols(Date = col_date(format = "%d/%m/%Y")))

oldglm_M2_TRMM_detrend = detrend(oldglm_M2_TRMM$total_pantropical_fluxes, tt= 'linear')
oldglm_M2_TRMM_IAV = rollsum(oldglm_M2_TRMM_detrend, 12, align = "right", fill = NA)
oldglm_M2_TRMM_IAV_smooth = runmean(oldglm_M2_TRMM_IAV, 6, endrule = c("NA"), align = "right")
write.csv(oldglm_M2_TRMM_IAV_smooth, file = "results/oldglm_M2_TRMM_IAV_smooth.csv")

#calculate min and max NEE in excel
write.csv(NEE_vs_CGR_df, file = "results/NEE_vs_CGR_df.csv")
NEE_vs_CGR_df <- read_csv("results/NEE_vs_CGR_df.csv", 
                    col_types = cols(Date = col_date(format = "%d/%m/%Y")))


#plot full uncertainty of new model and the observed CGR
ggplot(data = NEE_vs_CGR_df, aes(x = Date)) +
  geom_line(aes(y=detrended_CGR, colour = "Obs. CGR")) +
  geom_line(aes(y=smoothed_nino34, colour = "Nino 3.4")) +
  geom_line(aes(y=NEE_pantropical_p50_IAV_smooth, colour = "Tropical NEE median"), colour= "black", size = 1) +
  geom_ribbon(aes(ymin = min_NEE, ymax = max_NEE), alpha=0.5) +
  scale_y_continuous("CGR (PgC/year) & Nino 3.4 Anomaly", limits = c(-6 , 5.5),
                     minor_breaks = seq(-6, 5.5, 0.5), breaks = seq(-6,5, 1.0)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Date")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted") +
  ggtitle("Tropical NEE IAV vs Obs. CGR")

#plot just the median of the new model
ggplot(data = NEE_vs_CGR_df, aes(x = Date)) +
  geom_line(aes(y=detrended_CGR, colour = "Obs. CGR"), size = 0.8, colour = "grey45") +
  geom_line(aes(y=NEE_pantropical_p50_IAV_smooth, colour = "Tropical NEE median")) +
  scale_y_continuous("CGR (PgC/year) & Nino 3.4 Anomaly", limits = c(-1.75, 2.35),
                     minor_breaks = seq(-1.75, 2.25, 0.25), breaks = seq(-1.5,2.0, 0.5)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Date")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted") +
  ggtitle("just the median of new model")

#old but updated GLM plot (MERRA2-TRMM) 
ggplot(data = NEE_vs_CGR_df, aes(x = Date)) +
  geom_line(aes(y=detrended_CGR, colour = "Obs. CGR"), size = 0.8, colour = "grey45") +
  geom_line(aes(y=old_glm_merra2, colour = "Tropical NEE (MERRA2-TRMM)")) +
  scale_y_continuous("CGR (PgC/year) & Nino 3.4 Anomaly", limits = c(-1.75 , 2.35),
                     minor_breaks = seq(-1.75, 2.25, 0.25), breaks = seq(-1.5,2.0, 0.5)) +
  scale_x_date(breaks = pretty_breaks(10)) +
  xlab("Date")+
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  geom_hline(yintercept = 0.0, linetype = "dotted") +
  ggtitle("old (but updated) GLM vs Obs. CGR")
















