##############################################################################################
######                                                                                  ######
######                Converting Daymet netcdfs to tifs - tmax and tmin                 ######
######                                                                                  ######
##############################################################################################

# This code converts netcdf files of minimum and maximum daily temperature to spatrasters,
# and subsets data to be for 12-01-2016 to 03-31-2017.

# Since December 31 is missing (likely due to 2016 being a leap year), this code also
# interpolates a layer for December 31, based on data from December 30 and Jan 1.


# clean workspace to improve efficiency: #
rm(list = ls() ) 
gc() #releases memory


# Load required libraries

library(terra)
library(sf)
library(lubridate)
library(gstat)

####################################
####                            ####
####        Data Loading        ####
####                            ####
####################################

setwd <- "00_Data"

# Load Great lakes states and provinces
# states <- sf :: st_read("~/share/groups/mas/04_personal/Kim_T/Data/Great_Lakes_States and Prov.shp")
states <- sf :: st_read("Great_Lakes_States and Prov.shp")


####################################
####                            ####
####          Tmin              ####
####                            ####
####################################

#############
##  2016   ##
#############

# ncpath = path where the file was downloaded
# ncpath <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/"
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Daymet/"

#ncname = name of the file
ncname <- "tmin_daily_2016_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
tmin2016 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(tmin2016)
print(full_time)

### NOTE: 365 days but it's a leap year so December 31 is missing

# Subset to only December
dec_tmin2016 <- tmin2016[[which(month(time(tmin2016)) == 12)]]

# Create new names based on dates
new_names <- format(time(dec_tmin2016), "%Y-%m-%d")

# Rename the layers
names(dec_tmin2016) <- new_names

# Verify the new names
print(names(dec_tmin2016))

### NOTE: only 30 days because Dec 31 is missing

# Project raster to same as states
dec_tmin2016 <- terra :: project(dec_tmin2016, crs(states))

# Check that it looks ok
plot(dec_tmin2016[[1]])
plot(states$geometry, add = TRUE)


#############
##  2017   ##
#############

#ncname = name of the file
ncname <- "tmin_daily_2017_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
tmin2017 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(tmin2017)
print(full_time)

# Subset to only January, Feb, and March
new_tmin2017 <- tmin2017[[which(month(time(tmin2017)) == 01 |
                                  month(time(tmin2017)) == 02 |
                                  month(time(tmin2017)) == 03)]]

# Create new names based on dates
new_names <- format(time(new_tmin2017), "%Y-%m-%d")

# Rename the layers
names(new_tmin2017) <- new_names

# Verify the new names
print(names(new_tmin2017))

# Project raster to same as states
new_tmin2017 <- terra :: project(new_tmin2017, crs(states))

# Check that it looks ok
plot(new_tmin2017[[1]])
plot(states$geometry, add = TRUE)

######################################
## Interpolating a layer for Dec 31 ##
######################################

# Extract the last layer of dec_tmin2016 and the first layer of new_tmin2017
last_2016 <- dec_tmin2016[[nlyr(dec_tmin2016)]]
first_2017 <- new_tmin2017[[1]]

# Perform linear interpolation
interpolated_layer <- (last_2016 + first_2017) / 2

# Set the correct date for the interpolated layer
names(interpolated_layer) <- "2016-12-31"

# Combine all SpatRasters
combined_raster <- c(dec_tmin2016, interpolated_layer, new_tmin2017)

# Set the time dimension for the combined raster
dates <- seq(as.Date("2016-12-01"), as.Date("2017-03-31"), by = "day")
time(combined_raster) <- dates

######################################
##      Write the raster            ##
######################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Spatial Predictors/"
# path <- getwd()

writeRaster(combined_raster, paste(path, "Tmin.tif", sep = ""))




####################################
####                            ####
####          Tmax              ####
####                            ####
####################################

# clean workspace to improve efficiency: #
rm(list = ls() ) 
gc() #releases memory

# Reload Great lakes states and provinces
# states <- sf :: st_read("~/share/groups/mas/04_personal/Kim_T/Data/Great_Lakes_States and Prov.shp")
states <- sf :: st_read("Great_Lakes_States and Prov.shp")

#############
##  2016   ##
#############

# ncpath = path where the file was downloaded
# ncpath <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/"
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Daymet/"

#ncname = name of the file
ncname <- "tmax_daily_2016_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
tmax2016 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(tmax2016)
print(full_time)

### NOTE: 365 days but it's a leap year so December 31 is missing

# Subset to only December
dec_tmax2016 <- tmax2016[[which(month(time(tmax2016)) == 12)]]

# Create new names based on dates
new_names <- format(time(dec_tmax2016), "%Y-%m-%d")

# Rename the layers
names(dec_tmax2016) <- new_names

# Verify the new names
print(names(dec_tmax2016))

### NOTE: only 30 days because Dec 31 is missing

# Project raster to same as states
dec_tmax2016 <- terra :: project(dec_tmax2016, crs(states))

# Check that it looks ok
plot(dec_tmax2016[[1]])
plot(states$geometry, add = TRUE)


#############
##  2017   ##
#############

#ncname = name of the file
ncname <- "tmax_daily_2017_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
tmax2017 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(tmax2017)
print(full_time)

# Subset to only January, Feb, and March
new_tmax2017 <- tmax2017[[which(month(time(tmax2017)) == 01 |
                                  month(time(tmax2017)) == 02 |
                                  month(time(tmax2017)) == 03)]]

# Create new names based on dates
new_names <- format(time(new_tmax2017), "%Y-%m-%d")

# Rename the layers
names(new_tmax2017) <- new_names

# Verify the new names
print(names(new_tmax2017))

# Project raster to same as states
new_tmax2017 <- terra :: project(new_tmax2017, crs(states))

# Check that it looks ok
plot(new_tmax2017[[1]])
plot(states$geometry, add = TRUE)

######################################
## Interpolating a layer for Dec 31 ##
######################################

# Extract the last layer of dec_tmax2016 and the first layer of new_tmax2017
last_2016 <- dec_tmax2016[[nlyr(dec_tmax2016)]]
first_2017 <- new_tmax2017[[1]]

# Perform linear interpolation
interpolated_layer <- (last_2016 + first_2017) / 2

# Set the correct date for the interpolated layer
names(interpolated_layer) <- "2016-12-31"

# Combine all SpatRasters
combined_raster <- c(dec_tmax2016, interpolated_layer, new_tmax2017)

# Set the time dimension for the combined raster
dates <- seq(as.Date("2016-12-01"), as.Date("2017-03-31"), by = "day")
time(combined_raster) <- dates

######################################
##      Write the raster            ##
######################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Spatial Predictors/"
# path <- getwd()

writeRaster(combined_raster, paste(path, "Tmax.tif", sep = ""))













#Code tutorial obtained from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

# Load the ncdf4 package
library(ncdf4)

### Set the values for some temporary identifier variables ###

#ncpath = path where the file was downloaded
ncpath <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/"

#ncname = name of the file
ncname <- "tmax_daily_2016_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

#Name of the variable shown in the file
dname <- "tmax"

### Open the netCDF dataset and print some basic information
ncin <- nc_open(ncfname)
print(ncin)

# Get the coordinates: the name of the coordinate (e.g. lon, longitude, or x) is found in the printed
# dimension info from the above command. Here longitude is called "x" and latitude is called "y".
lon <- ncvar_get(ncin,"x")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"y")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time - 365 days are covered although I will not need all of these days
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
# Note the structure of the time units attribute. The object tunits has two components 
# hasatt (a logical variable), and tunits$value, the actual “time since” string.
tunits


### Getting the variable information ###

# get max temperature
tmax_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmax_array)
