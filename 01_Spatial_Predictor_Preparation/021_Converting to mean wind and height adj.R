#####################################################################
######                                                         ######
######          Converting U and V Wind Speed to Mean          ######
######                  and Adjusting for Height               ######
#####################################################################

# This code converts u and v wind speeds to average wind speed, 
# and adjusts for the height difference between NOAA values 
# (measured at 10 meters) and our wind values (measured at ~ 2 meters)

# clean workspace to improve efficiency: #
rm(list = ls() ) 
gc() #releases memory


# Load required libraries

library(terra)
library(sf)
library(lubridate)

####################################
####                            ####
####        Data Loading        ####
####                            ####
####################################

# setwd <- "00_Data"
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/Individual Wind Speed"
u_wind <- terra :: rast(paste(path, "/UWind.tif", sep = ""))
v_wind <- terra :: rast(paste(path, "/VWind.tif", sep = ""))


####################################
####                            ####
####    Convert to Average      ####
####        Wind Speed          ####
####################################

# Conversion based on pythagorean theorem

wind_speed <- sqrt(u_wind^2 + v_wind^2)


####################################
####                            ####
####    Adjustment for height   ####
####                            ####
####################################

# Our anemometers were mounted at ~ 2 meters above the ground, but the data
# here from NARR are based on a 10 meter height. 

# We can use the wind profile law cited in Megan's work to adjust for
# this height difference.
# Wind profile law: Peterson and Hennessey 1978
# cited in Fitzpatrick et al. 2019

# a = b * (x/10)^(1/7)
# where a is where a is wind speed at our anemometer’s height (value of
#    interest)
# x is the height of our anemometers (~2)
# b = wind speed at 10 meters (from raster)

wind_speed.adj <- wind_speed * ((2/10)^(1/7))


####################################
####                            ####
####    Write the raster        ####
####                            ####
####################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/"
writeRaster(wind_speed.adj, paste(path, "Wind.tif", sep = ""))
