#####################################################################
######                                                    ###########
######        CREATING A BLANK RESAMPLING RASTER          ###########
######                                                    ###########
######                                                    ###########
#####################################################################


# Author: Kimberly Thompson

# This code checks the extent of each raster and creates a blank
# resampling raster at 1 km resolution with which to harmonize
# all of the spatial predictors.

# Rasters produced:
# "Blank resampling raster_1km resolution.tif"



########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(terra)


####################################
###                              ###
###         Data Laoding         ###
###                              ###
####################################

# Load each spatial predictor
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/"

# Tmin
tmin <- terra :: rast(paste(path, "Tmin.tif", sep = ""))

# Tmax
tmax <- terra :: rast(paste(path, "Tmax.tif", sep = ""))

# Snow depth
depth <- terra :: rast(paste(path, "SnowDepth_USACan_masked.tif", sep = ""))

# Snow density
density <- terra :: rast(paste(path, "SnowDensity_USACan_masked.tif", sep = ""))

# Wind
wind <- terra :: rast(paste(path, "Wind.tif", sep = ""))

# Land cover
cover <- terra :: rast(paste(path, "USA CAN Landcover.tif", sep = ""))



####################################
###                              ###
###       Check projection       ###
###                              ###
####################################

cover # wgs1984
density # wgs 1984
depth # wgs 1984
tmax # nad83 / conus albers
tmin # nad83 / conus albers
wind # wgs84



#Check the resolution
res(cover)

# Check the extent
extent(cover)

# Wind and Snow
# Add working directory path to where downloaded data resides
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Wind and Snow")
windandsnow <- stack("X2016.12.01.tif")

# Change the names to be exactly what they are in the boosted regression tree model
names(windandsnow) <- c("Wind", "Snowmed")

#Check the projection
proj4string(windandsnow)

#Check the resolution
res(windandsnow)


#Tmin and Tmax
# Add working directory path to where downloaded data resides
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Tmax and Tmin")
airtemp <- stack("X2016.12.01.tif")

# Change the names to be exactly what they are in the boosted regression tree model
names(airtemp) <- c("Tairmax", "Tairmin")

#Check the projection
proj4string(airtemp)

#Check the resolution
res(airtemp)

# Landcover has the smallest resolution


############### Finding the right extent and resolution  #####################################

#Make a list of raster extents
rasterlist <- list(cover, windandsnow[[1]], windandsnow[[2]], airtemp[[1]], airtemp[[2]])
extent_list <- lapply(rasterlist, extent)

#Make a matrix out of it, each column represents a raster, rows the values
extent_list <- lapply(extent_list, as.matrix)
matrix_extent <- matrix(unlist(extent_list), ncol=length(extent_list))
rownames(matrix_extent) <- c("xmin", "ymin", "xmax", "ymax")

# create an extent with the extreme values of my extents
best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
                    min(matrix_extent[2,]), max(matrix_extent[4,]))

# the range of your extent in meters
ranges<-apply(as.matrix(best_extent), 1, diff)
# the resolution of my cover raster
# reso<-res(cover)

# Trying with the resolution of the temperature layers
reso<-res(airtemp)

# dividing the range by my desired resolution gives me the number of rows and columns
nrow_ncol<-ranges/reso

#Define CRS for all rasters
rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# create a new raster with the desired extent and resolution
s<-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)

# Write raster so that if I need to resample in the future, I know that this is the extent and resolution
# of the raster I need --> 1 km resolution with extent of
# -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Blank resampling rasters")
writeRaster(s, "Blank resampling raster_1km resolution.tif")

# Trying with the resolution of the land cover layer
reso<-res(cover)

# dividing the range by my desired resolution gives me the number of rows and columns
nrow_ncol<-ranges/reso

#Define CRS for all rasters
rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# create a new raster with the desired extent and resolution
s2 <-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)

# Write raster so that if I need to resample in the future, I know that this is the extent and resolution
# of the raster I need --> 30 m resolution with extent of
# -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)
writeRaster(s2, "Blank resampling raster_30m resolution.tif")

