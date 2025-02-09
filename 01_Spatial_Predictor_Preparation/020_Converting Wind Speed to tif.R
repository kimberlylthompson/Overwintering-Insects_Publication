#####################################################################
######                                                         ######
######          Converting Wind speed netcdfs to tifs          ######
######                                                         ######
#####################################################################

# This code converts netcdf files of daily wind speed to spatrasters,
#  subsets data to Dec 1 2016 through March 31 2017, 
# creates a daily average, and crops to the Great Lakes Region.

# Data were download from https://psl.noaa.gov/data/gridded/data.narr.html
# U-wind Individual obs 10 m 8x Daily (uwnd.10m.2016.nc)
#                                     (uwnd.10m.2017.nc) 
# V-wind Individual obs 10 m 8x Daily (vwnd.10m.2016.nc)
#                                     (vwnd.10m.2017.nc) 
# Units are meters per second


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

# Load Great lakes states and provinces
states <- sf :: st_read("~/share/groups/mas/04_personal/Kim_T/Data/Great_Lakes_States and Prov.shp")
# states <- sf :: st_read("00_Data/Great_Lakes_States and Prov.shp")


####################################
####                            ####
####          U-Wind            ####
####                            ####
####################################

#############
##  2016   ##
#############

# ncpath = path where the file was downloaded
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/Wind/"

#ncname = name of the file
ncname <- "uwnd.10m.2016"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
uwind2016 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(uwind2016)
print(full_time[2850:2928])

# Subset to only December
dec_uwind2016 <- uwind2016[[which(month(time(uwind2016)) == 12)]]

# Create new names based on dates
new_names <- format(time(dec_uwind2016), "%Y-%m-%d")

# Rename the layers
names(dec_uwind2016) <- new_names

# Verify the new names
print(names(dec_uwind2016))

######################
##  Daily Average   ##
######################

# Get the unique dates
unique_dates <- unique(names(dec_uwind2016))

# Create a list to store daily averages
daily_avg_list <- list()

# Calculate the average for each day
for (date in unique_dates) {
  
  # Select the layers for the current date
  daily_layers <- dec_uwind2016[[names(dec_uwind2016) == date]]
  
  # Calculate the mean across the layers for this date
  daily_avg <- mean(daily_layers)
  
  # Store the results in the list
  daily_avg_list[[date]] <- daily_avg
  
  print(date)
}

# Create a new raster from the daily averages
dec_uwind2016_avg <- rast(daily_avg_list)


###################################
##  Crop to Great Lakes Region   ##
###################################

# Project states to same crs as raster
states2 <- sf :: st_transform(states, terra :: crs(dec_uwind2016_avg))

# Check that it looks ok
plot(dec_uwind2016[[1]])
plot(states2$geometry, add = TRUE)

dec_uwind2016_avg.crop <- terra :: crop(dec_uwind2016_avg, states2, snap = "out")

# Clean up workspace
rm(daily_avg, daily_avg_list, daily_layers,dec_uwind2016,
   dec_uwind2016_avg, uwind2016)
rm(date, full_time, ncfname, ncname, ncpath, new_names, unique_dates)


#############
##  2017   ##
#############

# ncpath = path where the file was downloaded
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/Wind/"

#ncname = name of the file
ncname <- "uwnd.10m.2017"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
uwind2017 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(uwind2017)

# Subset to only Jan, Feb, Mar
win_uwind2017 <- uwind2017[[which(month(time(uwind2017)) == 01 |
                                    month(time(uwind2017)) == 02 |
                                    month(time(uwind2017)) == 03)]]

# Create new names based on dates
new_names <- format(time(win_uwind2017), "%Y-%m-%d")

# Rename the layers
names(win_uwind2017) <- new_names

# Verify the new names
print(names(win_uwind2017))

######################
##  Daily Average   ##
######################

# Get the unique dates
unique_dates <- unique(names(win_uwind2017))

# Create a list to store daily averages
daily_avg_list <- list()

# Calculate the average for each day
for (date in unique_dates) {
  
  # Select the layers for the current date
  daily_layers <- win_uwind2017[[names(win_uwind2017) == date]]
  
  # Calculate the mean across the layers for this date
  daily_avg <- mean(daily_layers)
  
  # Store the results in the list
  daily_avg_list[[date]] <- daily_avg
  
  print(date)
}

# Create a new raster from the daily averages
win_uwind2017_avg <- rast(daily_avg_list)


###################################
##  Crop to Great Lakes Region   ##
###################################

# Check that it looks ok
plot(win_uwind2017_avg[[1]])
plot(states2$geometry, add = TRUE)

win_uwind2017_avg.crop <- terra :: crop(win_uwind2017_avg, states2, snap = "out")


#############################################################
##  Combine U-wind for Winter season: Dec, Jan, Feb, Mar   ##
#############################################################

uwind_all <- c(dec_uwind2016_avg.crop, win_uwind2017_avg.crop)


###################################
##        Write the Raster       ##
###################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/Individual Wind Speed/"
writeRaster(uwind_all, paste(path, "UWind.tif", sep = ""))



####################################
####                            ####
####          V-Wind            ####
####                            ####
####################################

#############
##  2016   ##
#############

# ncpath = path where the file was downloaded
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/Wind/"

#ncname = name of the file
ncname <- "vwnd.10m.2016"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
vwind2016 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(vwind2016)
print(full_time[2850:2928])

# Subset to only December
dec_vwind2016 <- vwind2016[[which(month(time(vwind2016)) == 12)]]

# Create new names based on dates
new_names <- format(time(dec_vwind2016), "%Y-%m-%d")

# Rename the layers
names(dec_vwind2016) <- new_names

# Verify the new names
print(names(dec_vwind2016))

######################
##  Daily Average   ##
######################

# Get the unique dates
unique_dates <- unique(names(dec_vwind2016))

# Create a list to store daily averages
daily_avg_list <- list()

# Calculate the average for each day
for (date in unique_dates) {
  
  # Select the layers for the current date
  daily_layers <- dec_vwind2016[[names(dec_vwind2016) == date]]
  
  # Calculate the mean across the layers for this date
  daily_avg <- mean(daily_layers)
  
  # Store the results in the list
  daily_avg_list[[date]] <- daily_avg
  
  print(date)
}

# Create a new raster from the daily averages
dec_vwind2016_avg <- rast(daily_avg_list)


###################################
##  Crop to Great Lakes Region   ##
###################################

# Project states to same crs as raster
states2 <- sf :: st_transform(states, terra :: crs(dec_vwind2016_avg))

# Check that it looks ok
plot(dec_vwind2016[[1]])
plot(states2$geometry, add = TRUE)

dec_vwind2016_avg.crop <- terra :: crop(dec_vwind2016_avg, states2, snap = "out")

# Clean up workspace
rm(daily_avg, daily_avg_list, daily_layers,dec_vwind2016,
   dec_vwind2016_avg, vwind2016)
rm(date, full_time, ncfname, ncname, ncpath, new_names, unique_dates)


#############
##  2017   ##
#############

# ncpath = path where the file was downloaded
ncpath <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/Wind/"

#ncname = name of the file
ncname <- "vwnd.10m.2017"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# Read the netcdf file as a spatraster
vwind2017 <- terra :: rast(ncfname)

# Examine the time component
full_time <- time(vwind2017)

# Subset to only Jan, Feb, Mar
win_vwind2017 <- vwind2017[[which(month(time(vwind2017)) == 01 |
                                    month(time(vwind2017)) == 02 |
                                    month(time(vwind2017)) == 03)]]

# Create new names based on dates
new_names <- format(time(win_vwind2017), "%Y-%m-%d")

# Rename the layers
names(win_vwind2017) <- new_names

# Verify the new names
print(names(win_vwind2017))

######################
##  Daily Average   ##
######################

# Get the unique dates
unique_dates <- unique(names(win_vwind2017))

# Create a list to store daily averages
daily_avg_list <- list()

# Calculate the average for each day
for (date in unique_dates) {
  
  # Select the layers for the current date
  daily_layers <- win_vwind2017[[names(win_vwind2017) == date]]
  
  # Calculate the mean across the layers for this date
  daily_avg <- mean(daily_layers)
  
  # Store the results in the list
  daily_avg_list[[date]] <- daily_avg
  
  print(date)
}

# Create a new raster from the daily averages
win_vwind2017_avg <- rast(daily_avg_list)


###################################
##  Crop to Great Lakes Region   ##
###################################

# Check that it looks ok
plot(win_vwind2017_avg[[1]])
plot(states2$geometry, add = TRUE)

win_vwind2017_avg.crop <- terra :: crop(win_vwind2017_avg, states2, snap = "out")


#############################################################
##  Combine V-wind for Winter season: Dec, Jan, Feb, Mar   ##
#############################################################

vwind_all <- c(dec_vwind2016_avg.crop, win_vwind2017_avg.crop)


###################################
##        Write the Raster       ##
###################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/Individual Wind Speed/"
writeRaster(vwind_all, paste(path, "VWind.tif", sep = ""))