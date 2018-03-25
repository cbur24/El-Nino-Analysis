
#carboon flux scale up using flux equations defined using GEM plot data and
#environmental parameters from the various reanalysis packages.

library(raster)
library(ncdf4)
library(rgdal)
library(sp)
library(caTools)
library(RcppRoll)
library(pracma)

#read in and prepare ncdf files
  a = "ERAI_2000_2016_Mmean_VPD_masked.nc"
  ERAI_VPD_pantropics = stack(a, varname = "variable")
  ERAI_VPD_pantropics = rotate(ERAI_VPD_pantropics)
  dates <- format(seq(as.Date('2000/01/1'), as.Date('2016/12/1'), by='month'), '%Y%m')
  ERAI_VPD_pantropics <- setNames(ERAI_VPD_pantropics, dates)
  
  b = "TRMM_CWD_2000_2016_ERAIGRID_masked.nc"
  TRMM_CWD_pantropics = stack(b, varname = "CWD")
  TRMM_CWD_pantropics = setNames(TRMM_CWD_pantropics, dates)
  TRMM_CWD_pantropics = rotate(TRMM_CWD_pantropics)
  
  c = "TRMM_2000_2016_3B42_monthly_ERAIGRID_masked.nc4"
  TRMM_pantropics = stack(c, varname = "precipitation")
  crs(TRMM_pantropics) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  TRMM_pantropics = t(TRMM_pantropics)
  TRMM_pantropics = flip(TRMM_pantropics, direction = 1)
  TRMM_pantropics <- setNames(TRMM_pantropics, dates)
  TRMM_pantropics = rotate(TRMM_pantropics)
  
  d = "erai_2000_2016_monthlymeans_t2m_tropics_masked.nc"
  ERAI_T2M_pantropics = stack(d, varname = "t2m")
  ERAI_T2M_pantropics <- setNames(ERAI_T2M_pantropics, dates)
  ERAI_T2M_pantropics = rotate(ERAI_T2M_pantropics)
  ERAI_T2M_pantropics = ERAI_T2M_pantropics - 273.15
  
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
  ERAI_VPD_SEA = crop(ERAI_VPD_pantropics, extent_SEA)
  
  ERAI_VPD_SEA = calc(ERAI_VPD_SEA, fun = function(x){x[x > 0.9] = 0.9; return(x)})
  ERAI_VPD_SEA = calc(ERAI_VPD_SEA, fun = function(x){x[x < 0.4] = 0.4; return(x)})
  
  #Africa
  ERAI_VPD_A = crop(ERAI_VPD_pantropics, extent_A)
  TRMM_CWD_A = crop(TRMM_CWD_pantropics, extent_A)
  TRMM_A = crop(TRMM_pantropics, extent_A)
  
  ERAI_VPD_A = calc(ERAI_VPD_A, fun = function(x){x[x > 1.7] = 1.7; return(x)})
  ERAI_VPD_A = calc(ERAI_VPD_A, fun = function(x){x[x < 0.3] = 0.3; return(x)})
  TRMM_CWD_A = calc(TRMM_CWD_A, fun = function(x){x[x < -446] = -446; return(x)})
  TRMM_A = calc(TRMM_A, fun = function(x){x[x > 457] = 457; return(x)})
  
  #Sth America
  ERAI_VPD_SAmerica = crop(ERAI_VPD_pantropics, extent_SA)
  ERAI_T2M_SAmerica = crop(ERAI_T2M_pantropics, extent_SA)
  TRMM_CWD_SAmerica = crop(TRMM_CWD_pantropics, extent_SA)

  TRMM_CWD_SAmerica = calc(TRMM_CWD_SAmerica, fun = function(x){x[x < -276] = -276; return(x)})
  ERAI_VPD_SAmerica = calc(ERAI_VPD_SAmerica, fun = function(x){x[x > 0.8] = 0.8; return(x)})
  ERAI_VPD_SAmerica = calc(ERAI_VPD_SAmerica, fun = function(x){x[x < 0.18] = 0.18; return(x)})
  ERAI_T2M_SAmerica = calc(ERAI_T2M_SAmerica, fun = function(x){x[x > 26.3] = 26.3; return(x)})
  ERAI_T2M_SAmerica = calc(ERAI_T2M_SAmerica, fun = function(x){x[x < 21.5] = 21.5; return(x)})



#calculate NEP for each region ####################################################

#NEP = NPP(wood) + NPP(LF) + NPP(root) - Rh

#equations have been defined for the SAFE, TAM, and Bobiri sites.

#SE ASIA ############################################################################

#component equations
#NPP_wood_SAFE = 10.0^(-1.19123*(log10(VPD))-1.0032)
#NPP_LF_SAFE = 10.0^(0.67690*(log10(VPD)) - 0.51262)
#NPP_roots_SAFE = 10.0^-0.97645
#R_het_SAFE = 10.0^-0.14162


#define functions
NPP_wood_SEA_fun <- function(VPD){
    NPP_wood = 10.0**(-1.9123*log(VPD,10)-1.0032)
return(NPP_wood)
}

NPP_LF_SEA_fun = function(VPD){
    NPP_LF = 10.0**(0.67690*log(VPD,10)-0.51262)
return(NPP_LF)
}

#calculate NEP
NPP_wood_SEA = NPP_wood_SEA_fun(ERAI_VPD_SEA)
NPP_LF_SEA = NPP_LF_SEA_fun(ERAI_VPD_SEA)

NEP_SEA_ERAI = NPP_wood_SEA + NPP_LF_SEA + 0.1055723  - 0.7217387

#plot up the monhtly timeseries of each component of NEE
NPP_LF_SEA_flux_perpixel = NPP_LF_SEA*692456
NPP_LF_SEA_flux_PgC = ((cellStats(NPP_LF_SEA_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NPP_wood_SEA_flux_perpixel = NPP_wood_SEA*692456
NPP_wood_SEA_flux_PgC = ((cellStats(NPP_wood_SEA_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

yaxis1 = c("-1.25","-1.0","-0.75","-0.50","-0.25","0.0")
plot.ts(NPP_LF_SEA_flux_PgC, xaxt = 'n', col = "blue", main = "NPP SEA", ylim = c(-1.25, 0.0), yaxt ='n',
                      ylab = "PgC/month", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
abline(h=yaxis1, lty = "dotted", col = "grey66")
abline(v=xaxis, lty = "dotted", col = "grey66")
axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
axis(2,at=yaxis1,labels=yaxis1,tick=TRUE,cex.axis=1)
lines(NPP_wood_SEA_flux_PgC, col ="forestgreen")
legend("bottomleft", c("NPPlf", "NPPwood"),
       cex=0.85, col=c("blue", "forestgreen"), pch = c(16,16), bty ='n')

#change these NEE values to PgC/year to plot up and check out
NPP_LF_SEA_flux_PgC_annual = rollsum(NPP_LF_SEA_flux_PgC, 12, align = "right", fill = NA)
NPP_wood_SEA_flux_PgC_annual = rollsum(NPP_wood_SEA_flux_PgC, 12, align = "right", fill = NA)

xaxis <- c("24","48","72","96","120","144","168", "192")
plot.ts(NPP_LF_SEA_flux_PgC_annual, xaxt = 'n', col = "blue", main = "NPP SEA", ylim = c(-4.5, -1),
                      ylab = "PgC/year", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
lines(NPP_wood_SEA_flux_PgC_annual, col ="forestgreen")
grid()
axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
legend("bottomleft", c("NPPlf", "NPPwood"),
       cex=0.85, col=c("blue", "forestgreen"), pch = c(16,16), bty ='n')

#calculate carbon flux
ERAI_SEA_FLUX_perpixel = NEP_SEA_ERAI*692456    #multiply by the area (ha) of the pixels
ERAI_SEA_FLUX_PgC = ((cellStats(ERAI_SEA_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)   #sum all the pixels in each raster,
                                                                                              #then convert to PgC, then reverse sign
#plot time series
plot.ts(ERAI_SEA_FLUX_PgC, col = "blue", main = "Carbon Flux - SE Asia")
abline(h= mean.default(ERAI_SEA_FLUX_PgC))

###creating timeseries for each component flux
#NPP
NPP_SEA = NPP_wood_SEA + NPP_LF_SEA + 0.1055723
NPP_SEA_perpixel = NPP_SEA*692456
NPP_SEA_PgC = ((cellStats(NPP_SEA_perpixel, stat='sum', na.rm=TRUE))/1e+9)
plot.ts(NPP_SEA_PgC)
#create and export dataframe
component_NEE_SEA_df = data.frame(NPP_SEA_PgC) 
write.csv(component_NEE_SEA_df, file = "component_NEE_SEA_ERAI_TRMM.csv")

#AFRICA-#####################################################################

#components
#NPP_wood_TAM = 0.2943754
#NPP_LF_TAM = 10.0^((2.2259*(log10(CWD+1000))) + (1.2916*(log10(VPD))) - 6.6778)
#NPP_roots_TAM = 10.0^((-1.9251(log10(VPD))) - 0.8657)
#R_het_TAM = 10.0^((0.33890(log10(PRCP+10))) - 0.63451)

#define functions
NPP_LF_A_fun = function(CWD, VPD){
    NPP_LF = 10.0**(2.2259*log((CWD+1000.0),10)+1.2916*log(VPD,10)-6.6778)
return(NPP_LF)
}

NPP_roots_A_fun = function(VPD){
    NPP_roots = 10.0**(-1.9251*log(VPD,10)-0.8657)
return(NPP_roots)
}

Rh_A_fun = function(PRCP){
    Rh = 10.0**(0.33890*log((PRCP+10.0),10)-0.63451)
return(Rh)
}

#calculate components of NEP and NEP
NPP_LF_A = NPP_LF_A_fun(TRMM_CWD_A, ERAI_VPD_A)
NPP_roots_A = NPP_roots_A_fun(ERAI_VPD_A)
Rh_A = Rh_A_fun(TRMM_A)

NEP_A_ERAI = 0.2943754 + NPP_LF_A + NPP_roots_A - Rh_A

#plot up the timeseries of each component of NPP
NPP_LF_A_flux_perpixel = NPP_LF_A*692307
NPP_LF_A_flux_PgC = ((cellStats(NPP_LF_A_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NPP_roots_A_flux_perpixel = NPP_roots_A*692307
NPP_roots_A_flux_PgC = ((cellStats(NPP_roots_A_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

plot.ts(NPP_LF_A_flux_PgC, xaxt = 'n', col = "blue", main = "NPP Africa", ylim = c(-1.25, 0.0), yaxt ='n',
                      ylab = "PgC/month", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
abline(h=yaxis1, lty = "dotted", col = "grey66")
abline(v=xaxis, lty = "dotted", col = "grey66")
axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
axis(2,at=yaxis1,labels=yaxis1,tick=TRUE,cex.axis=1)
lines(NPP_roots_A_flux_PgC, col ="forestgreen")
lines(Rh_A_PgC, col = "red")
legend("bottomright", c("NPPlf", "NPProots"),
       cex=0.85, col=c("blue", "forestgreen"), pch = c(16,16), bty ='n')

#plot up annual timeseries of componebts of NEE
NPP_LF_A_flux_PgC_annual = rollsum(NPP_LF_A_flux_PgC, 12, align = "right", fill = NA)
NPP_roots_A_flux_PgC_annual = rollsum(NPP_roots_A_flux_PgC, 12, align = "right", fill = NA)
Rh_A_PgC_annual = rollsum(Rh_A_PgC, 12, align = "right", fill = NA)

plot.ts(NPP_LF_A_flux_PgC_annual, xaxt = 'n', col = "blue", main = "NPP Africa", ylim = c(-10, -1),
                      ylab = "PgC/year", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
lines(NPP_roots_A_flux_PgC_annual, col ="forestgreen")
grid(); axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
legend("bottomright", c("NPPlf", "NPProots"),
       cex=0.85, col=c("blue", "forestgreen"), pch = c(16,16), bty ='n')

#calculate carbon flux
ERAI_A_FLUX_perpixel = NEP_A_ERAI*692307    #multiply by the area (ha) of the pixels
ERAI_A_FLUX_PgC = ((cellStats(ERAI_A_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)   #sum all the pixels in each raster,
                                                                                          #then convert to PgC, then reverse sign
#plot and export flux time series
plot.ts(ERAI_A_FLUX_PgC, col = "blue", main = "Carbon Flux - Africa")
abline(h= mean.default(ERAI_A_FLUX_PgC))

###creating timeseries for each component flux
#Heterotrophic respiration
Rh_A_perpixel = Rh_A*692307   
Rh_A_PgC = ((cellStats(Rh_A_perpixel, stat='sum', na.rm=TRUE))/1e+9)
#NPP
NPP_A = 0.2943754 + NPP_LF_A + NPP_roots_A
NPP_A_perpixel = NPP_A*692307
NPP_A_PgC = ((cellStats(NPP_A_perpixel, stat='sum', na.rm=TRUE))/1e+9)
plot.ts(NPP_A_PgC)
#plot
plot.ts((ERAI_A_FLUX_PgC*-1.0), col = "blue", main = "Carbon Flux - A", ylim =c(0,1.5))
lines(Rh_A_PgC, col = "red")
lines(NPP_A_PgC, col = "green")
#create and export dataframe
component_NEE_A_df = data.frame(Rh_A_PgC, NPP_A_PgC, (ERAI_A_FLUX_PgC*-1)) 
write.csv(component_NEE_A_df, file = "component_NEE_Africa_ERAI_TRMM.csv")


#STH AMERICA ######################################################################################

#components
#NPP_wood_TAM = 10.0^((-0.103522*log10(CWD+1000)) - (0.029851*log10(VPD)) + 1.309017) - 10
#NPP_LF_TAM = 10.0^(0.76073*(log10(VPD)) - 0.09611)
#NPP_roots_TAM = 10.0^(-1.1087*log10(VPD) - 1.0209)
#R_het_TAM = 10.0^((1.8355*log10(CWD + 1000)) + (2.4838*log10(Temp)) - 9.0014)  
  
#define functions
NPP_wood_SAmerica_fun = function(CWD, VPD){
    NPP_roots = 10.0**(-0.103522*log((CWD+1000.0),10)-0.029851*log(VPD,10)+1.309017)-10.0
return(NPP_roots)
}

NPP_LF_SAmerica_fun = function(VPD){
    NPP_LF = 10.0**(0.76073*log(VPD,10)-0.09611)
return(NPP_LF)
}

NPP_roots_SAmerica_fun = function(VPD){
    NPP_roots = 10.0**(-1.1087*log(VPD,10)-1.0209)
return(NPP_roots)
}

Rh_SAmerica_fun = function(CWD, Temp){
    Rh = 10.0**(1.8355*log((CWD+1000.0),10)+2.4838*log(Temp,10)-9.0014) 
return(Rh)
}

#calculate NEP
NPP_wood_SAmerica = NPP_wood_SAmerica_fun(TRMM_CWD_SAmerica, ERAI_VPD_SAmerica)
NPP_LF_SAmerica = NPP_LF_SAmerica_fun(ERAI_VPD_SAmerica)
NPP_roots_SAmerica = NPP_roots_SAmerica_fun(ERAI_VPD_SAmerica)
Rh_SAmerica = Rh_SAmerica_fun(TRMM_CWD_SAmerica, ERAI_T2M_SAmerica)

NEP_SAmerica_ERAI = NPP_wood_SAmerica + NPP_LF_SAmerica + NPP_roots_SAmerica - Rh_SAmerica

#plot up the timeseries of each component of NPP
NPP_wood_SAmerica_flux_perpixel = NPP_wood_SAmerica*686565
NPP_wood_SAmerica_flux_PgC = ((cellStats(NPP_wood_SAmerica_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NPP_LF_SAmerica_flux_perpixel = NPP_LF_SAmerica*686565
NPP_LF_SAmerica_flux_PgC = ((cellStats(NPP_LF_SAmerica_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

NPP_roots_SAmerica_flux_perpixel = NPP_roots_SAmerica*686565
NPP_roots_SAmerica_flux_PgC = ((cellStats(NPP_roots_SAmerica_flux_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)

plot.ts(NPP_LF_SAmerica_flux_PgC, xaxt = 'n', col = "blue", main = "NPP SAmerica", ylim = c(-1.25, 0.0), yaxt ='n',
                      ylab = "PgC/month", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
abline(h=yaxis1, lty = "dotted", col = "grey66")
abline(v=xaxis, lty = "dotted", col = "grey66")
axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
axis(2,at=yaxis1,labels=yaxis1,tick=TRUE,cex.axis=1)
lines(NPP_roots_SAmerica_flux_PgC, col ="forestgreen")
lines(NPP_wood_SAmerica_flux_PgC, col ="purple")
lines(Rh_SAmerica_PgC, col = "red")
legend("bottomright", c("NPPlf", "NPProots", "NPPwood"),
       cex=0.85, col=c("blue", "forestgreen","purple"), pch = c(16,16), bty ='n')

#plot up timeseries of componts of NEE but annual growth rates
NPP_LF_SAmerica_flux_PgC_annual = rollsum(NPP_LF_SAmerica_flux_PgC, 12, align = "right", fill = NA)
NPP_roots_SAmerica_flux_PgC_annual = rollsum(NPP_roots_SAmerica_flux_PgC, 12, align = "right", fill = NA)
NPP_wood_SAmerica_flux_PgC_annual = rollsum(NPP_wood_SAmerica_flux_PgC, 12, align = "right", fill = NA)
Rh_SAmerica_PgC_annual = rollsum(Rh_SAmerica_PgC, 12, align = "right", fill = NA)


plot.ts(NPP_LF_SAmerica_flux_PgC_annual,xaxt = 'n', col = "blue", main = "NPP SAmerica", ylim = c(-7, -2),
                      ylab = "PgC/year", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
lines(NPP_roots_SAmerica_flux_PgC_annual, col ="forestgreen")
lines(NPP_wood_SAmerica_flux_PgC_annual, col ="purple")
grid(); axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
legend("bottomleft", c("NPPlf", "NPProots", "NPPwood"),
       cex=0.85, col=c("blue", "forestgreen","purple"), pch = c(16,16), bty ='n')

plot(Rh_SAmerica_PgC_annual, col = "red", ylim = c(5, 9.5), main = "H. Respiration")
lines(Rh_A_PgC_annual, col = "black")
legend("bottomleft", c("Rh SAmerica", "Rh Africa"),
       cex=0.85, col=c("red", "black"), pch = c(16,16), bty ='n')


#calculate net carbon flux over South America
ERAI_SAmerica_FLUX_perpixel = NEP_SAmerica_ERAI*686565    #multiply by the area (ha) of the pixels
ERAI_SAmerica_FLUX_PgC = ((cellStats(ERAI_SAmerica_FLUX_perpixel, stat='sum', na.rm=TRUE))/1e+9)*(-1)   #sum all the pixels in each raster,
                                                                                                        #then convert to PgC, then reverse sign
#plot net carbon flux series
plot.ts(ERAI_SAmerica_FLUX_PgC, col = "blue", main = "Carbon Flux - SAmerica")
abline(h= mean.default(ERAI_SAmerica_FLUX_PgC))

###creating timeseries for each component flux
#Heterotrophic respiration
Rh_SAmerica_perpixel = Rh_SAmerica*686565   
Rh_SAmerica_PgC = ((cellStats(Rh_SAmerica_perpixel, stat='sum', na.rm=TRUE))/1e+9)
#NPP
NPP_SAmerica = NPP_wood_SAmerica + NPP_LF_SAmerica + NPP_roots_SAmerica
NPP_SAmerica_perpixel = NPP_SAmerica*686565
NPP_SAmerica_PgC = ((cellStats(NPP_SAmerica_perpixel, stat='sum', na.rm=TRUE))/1e+9)
#plot
plot.ts((ERAI_SAmerica_FLUX_PgC*-1.0), col = "blue", main = "Carbon Flux - SAmerica", ylim =c(0,1.5))
lines(Rh_SAmerica_PgC, col = "red")
lines(NPP_SAmerica_PgC, col = "green")
#create and export dataframe
component_NEE_SAmerica_df = data.frame(Rh_SAmerica_PgC, NPP_SAmerica_PgC, (ERAI_SAmerica_FLUX_PgC*-1)) 
write.csv(component_NEE_SAmerica_df, file = "component_NEE_SAmerica_ERAI_TRMM.csv")


#### add all the regional totals together to find the pantropical carbon flux####################
total_pantropical_fluxes = ERAI_SEA_FLUX_PgC + ERAI_A_FLUX_PgC + ERAI_SAmerica_FLUX_PgC
All_fluxes_df = data.frame(ERAI_SEA_FLUX_PgC, ERAI_A_FLUX_PgC, ERAI_SAmerica_FLUX_PgC, total_pantropical_fluxes)  

#plot
plot.ts(total_pantropical_fluxes, main = "Carbon Flux - Pantropics, ERAI and TRMM"
        , ylim =c(-1.5,0.2))
lines(All_fluxes_df$ERAI_SEA_FLUX_PgC, col = "green")
lines(All_fluxes_df$ERAI_A_FLUX_PgC, col = "red")
lines(All_fluxes_df$ERAI_SAmerica_FLUX_PgC, col = "blue")
abline(h = 0)
legend("bottomleft", c("Pantropical Flux","SEAsia", "Africa", "SAmerica")
       ,cex=0.85, col=c("black", "green", "red", "blue"), pch = c(16,16), bty ='n')


#plot the running mean of the pantropical carbon flux
plot.ts(runmean(total_pantropical_fluxes, 6, endrule = c("keep")), ylim = c(-1.5,0.35) )
abline(h = 0)
write.csv(All_fluxes_df, file ="carbon_fluxes_ERAI.csv")




############################################################################################
#extract timeseries from GEM sites to compare against model predictions from site observation

library(tibble)
GEM_sites_SAFE <- SpatialPointsDataFrame(extraction_sites_GEM[,2:3],
                      extraction_sites_GEM,           
                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))   

GEM_sites_BOB <- SpatialPointsDataFrame(extraction_sites_GEM[,2:3],
                      extraction_sites_GEM,           
                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

GEM_sites_TAM <- SpatialPointsDataFrame(extraction_sites_GEM[,2:3],
                      extraction_sites_GEM,           
                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  


GEM_sites_SAFE = spTransform(GEM_sites_SAFE, CRSobj = AEA_asia)
GEM_sites_BOB = spTransform(GEM_sites_SAFE, CRSobj = AEA_africa)
GEM_sites_TAM = spTransform(GEM_sites_SAFE, CRSobj = AEA_SAmerica)

# extract time series from the rasterstack 
SAFE_flux = extract(NEP_SEA_ERAI, GEM_sites_SAFE, method = 'simple',
                      layer = 1, nl = 204, df = TRUE)

BOB_flux = extract(NEP_A_ERAI, GEM_sites_BOB, method = 'simple',
                        layer = 1, nl = 204, df = TRUE)

TAM_flux = extract(NEP_SAmerica_ERAI,GEM_sites_TAM,method = 'simple',
                      layer = 1, nl = 204, df = TRUE)

SAFE_flux = t(SAFE_flux)
BOB_flux = t(BOB_flux)
TAM_flux = t(TAM_flux)

SAFE_flux = SAFE_flux *-1
BOB_flux = BOB_flux * -1
TAM_flux = TAM_flux * -1


# write .csv and rearrange table for easy plotting
GEM_site_fluxes_MgC_ha = data.frame(SAFE_flux, BOB_flux, TAM_flux)
write.csv(GEM_site_fluxes_MgC_ha, file = "GEM_sites_fluxes_MgCha_ERAI.csv")


#checking total NPP against total Rh
NPP_pantropics = NPP_A_PgC + NPP_SEA_PgC + NPP_SAmerica_PgC
NPP_pantropics_detrend = detrend(NPP_pantropics, tt= 'linear')
NPP_pantropics_annual_detrend = roll_sum(NPP_pantropics_detrend, 12, align = "right", fill = NA)

RH_pantropics = Rh_A_PgC + Rh_SAmerica_PgC
RH_pantropics_detrend = detrend(RH_pantropics, tt= 'linear')
RH_pantropics_annual_detrend = roll_sum(RH_pantropics_detrend, 12, align = "right", fill = NA)

yaxis = c("-1.5","-1.0","-0.5","0.0","0.5","1.0","1.5")
plot.ts(NPP_pantropics_annual_detrend,  xaxt = 'n', yaxt = 'n', col = "forestgreen", main = "ERAI-TRMM", ylim = c(-1.5, 1.5),
                      ylab = "PgC/year", xlab = "Months since Jan 2000", mgp=c(2.3,1,0))
abline(h=yaxis, lty = "dotted", col = "grey66")
abline(v=xaxis, lty = "dotted", col = "grey66")
lines(NPP_roots_A_flux_PgC_annual, col ="forestgreen")
axis(1,at=xaxis,labels=xaxis,tick=TRUE,cex.axis=1)
axis(2,at=yaxis,labels=yaxis,tick=TRUE,cex.axis=1)
lines(RH_pantropics_annual_detrend, col = "red")
legend("bottomleft", c("NPP","Rh")
       ,cex=0.85, col=c("forestgreen", "red"), pch = c(16,16), bty ='n')
abline(h = 0)





  
  