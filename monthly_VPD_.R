
## Calculate vapor pressure deficit from relative humidity and temperature.

library(ncdf4)
library(raster)
library(RNetCDF)
library(sp)

# ---------------------------------------------------------------------------------------------------
# NCEP DOE Reanalysis II
# ----------------------------------------------------------------------------------------------------
## bring in netCDF files and convert to raster stack

# first bring in specific humidity
a = "NCEP_DOE_2000_2016_SH_monmean.nc"
NCEP_SH = brick(a, varname = "shum")
NCEP_SH

#now bring in temperature (its in K)
b = "NCEP_DOE_2000_2016_t2m_monthly.nc"
NCEP_T2M = brick(b, varname = "air")
#convert to degrees celsius
NCEP_T2M = NCEP_T2M - 273.15
NCEP_T2M

#now bring in pressure
c = "NCEP_DOE_2000_2016_press_monmean_remap.nc"
NCEP_press = brick(c, varname = "pres")
#convert from Pa to mb
NCEP_press = NCEP_press/100
NCEP_press

##' ---------------------------------------------------
##' converting specific humidity into relative humidity

##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio

#define function for converting specific humidity to relative humidity
qair2rh <- function(qair, temp, press = 1013.25) {
  es <- 6.112 * exp((17.67 * temp) / (temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

#Now call function and calculate RH
NCEP_RH = qair2rh(NCEP_SH, NCEP_T2M, NCEP_press)
NCEP_RH
#relative humidity here is a ratio so need to convert to %
NCEP_RH_perc = NCEP_RH * 100
NCEP_RH_perc

# Calculate saturation vapor pressure in order to calculate VPD
##' @title get es
##' @param temp temperature in degrees C 
##' @return saturation vapor pressure in mb

get.es <- function(temp) {
  return(6.11 * exp((2500000/461) * (1/273 - 1/(273 + temp))))
} 

##' Calculate vapor pressure deficit from relative humidity and temperature.
##' @title VPD
##' @param rh relative humidity, in percent 
##' @param temp temperature, degrees celsius
##' @return vpd: vapor pressure deficit, in mb

get.vpd <- function(rh, temp) {
es <- get.es(temp)
return(((100 - rh)/100) * es)
} 

#now that both the es and VPD functions are defined, invoke get.VPD to calculate VPD
NCEP_VPD = get.vpd(NCEP_RH_perc, NCEP_T2M) 

#need to divide by 10 to go from mb to kPa
NCEP_VPD = NCEP_VPD/10

NCEP_VPD
image(NCEP_VPD)

# export out as netCDF
writeRaster(NCEP_VPD, filename='NCEP_DOE_2000_2016_monthly_mean_VPD.nc', format="CDF", 
            varname = "VPD", varunit = "kPa", longname = "Vapour_Pressure_Deficit", zname = "time", overwrite=TRUE)

# ---------------------------------------------------------------------------------------------------
# ERA- Interim
# ----------------------------------------------------------------------------------------------------
# calculating relative humidity from near surface dew point temperature and air temperature
# Alduchov, O. A., and R. E. Eskridge, 1996: Improved Magnus' form approximation of saturation vapor pressure. J. Appl. Meteor., 35, 601-609.
# http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html

# T and TD inputs/outputs to the equations are in Celsius
# T is air temperature and TD is dew point temperature

#Bring in 2m air temperature (in degrees celsius)
d = "erai_2000_2016_M_t2m_celsius.nc"
ERAI_Ta = stack(d, varname = "t2m")
ERAI_Ta

#Bring in 2m dewpoint temperature (in degrees celsius)
e = "erai_2000_2016_M_2mdewpointT_celsius.nc"
ERAI_Td = stack(e, varname = "d2m")
ERAI_Td 

#Bring in surface air pressure (in mb)
f = "erai_2000_2016_M_pressure_mb.nc"
ERAI_sp = stack(f, varname = "sp")
ERAI_sp 

# calculate RH from dewpoint temperature and air temperature
ERAI_RH = 100*(exp((17.625*ERAI_Td)/(243.04+ERAI_Td))/exp((17.625*ERAI_Ta)/(243.04+ERAI_Ta)))
print(ERAI_RH)

# Calculate saturation vapor pressure in order to calculate VPD
##' @title get es
##' @param temp temperature in degrees C 
##' @return saturation vapor pressure in mb

get.es <- function(temp) {
  return(6.11 * exp((2500000/461) * (1/273 - 1/(273 + temp))))
} 

##' Calculate vapor pressure deficit from relative humidity and temperature.
##' @title VPD
##' @param rh relative humidity, in percent 
##' @param temp temperature, degrees celsius
##' @return vpd: vapor pressure deficit, in mb

get.vpd <- function(rh, temp) {
  es <- get.es(temp)
  return(((100 - rh)/100) * es)
} 

#now that both the es and VPD functions are defined, invoke get.VPD to calculate VPD
ERAI_VPD = get.vpd(ERAI_RH, ERAI_Ta) 

#need to divide by 10 to go from mb to kPa
ERAI_VPD = ERAI_VPD/10

print(ERAI_VPD)
image(ERAI_VPD)

# export out as netCDF and use CDO to calculate anomalies
writeRaster(ERAI_VPD, filename='ERAI_2000_2016_M_VPD.nc', format="CDF", overwrite=TRUE)



#compute speciic humidity
# https://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html

e = 6.112*exp((17.67*Td)/(Td + 243.5));
q = (0.622 * e)/(p - (0.378 * e));

#where:
# e = vapor pressure in mb;
# Td = dew point in deg C;
# p = surface pressure in mb;
# q = specific humidity in kg/kg. 









