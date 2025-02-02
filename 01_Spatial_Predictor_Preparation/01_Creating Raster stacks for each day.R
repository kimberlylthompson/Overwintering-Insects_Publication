<<<<<<< HEAD
#####################################################################
######                                                    ###########
###### CREATING RASTER STACKS WITH ALL THE PREDCITOR DATA ###########
######            to apply predict function               ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code resamples each spatial predictor raster (min and max
# temp, snow depth and density, wind speed, and land cover) to 
# have the same resolution and creates a stack of the predictors
# for each day in the winter season.

# Rasters produced:
# X2016.12.01.tif
# (and on through X2017.03.31)





########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(sp)


# ######################################################################
# ################## BELOW GETS RUN ONLY ONCE IN ORDER #################
# ################# TO EXTRACT VALUES NEEDED FOR NEW EXTENT ############
# ################# TO BE USED DURING RESAMPLING           #############
# ###############   AFTER THAT SKIP TO LINE 140          ###############
# ######################################################################
# 
# 
# 
# #Check the resolution
# res(cover)
# 
# # Check the extent
# extent(cover)
# 
# # Wind and Snow
# # Add working directory path to where downloaded data resides
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Wind and Snow")
# windandsnow <- stack("X2016.12.01.tif")
# 
# # Change the names to be exactly what they are in the boosted regression tree model
# names(windandsnow) <- c("Wind", "Snowmed")
# 
# #Check the projection
# proj4string(windandsnow)
# 
# #Check the resolution
# res(windandsnow)
# 
# 
# #Tmin and Tmax
# # Add working directory path to where downloaded data resides
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Tmax and Tmin")
# airtemp <- stack("X2016.12.01.tif")
# 
# # Change the names to be exactly what they are in the boosted regression tree model
# names(airtemp) <- c("Tairmax", "Tairmin")
# 
# #Check the projection
# proj4string(airtemp)
# 
# #Check the resolution
# res(airtemp)
# 
# # Landcover has the smallest resolution
# 
# 
# ############### Finding the right extent and resolution  #####################################
# 
# #Make a list of raster extents
# rasterlist <- list(cover, windandsnow[[1]], windandsnow[[2]], airtemp[[1]], airtemp[[2]])
# extent_list <- lapply(rasterlist, extent)
# 
# #Make a matrix out of it, each column represents a raster, rows the values
# extent_list <- lapply(extent_list, as.matrix)
# matrix_extent <- matrix(unlist(extent_list), ncol=length(extent_list))
# rownames(matrix_extent) <- c("xmin", "ymin", "xmax", "ymax")
# 
# # create an extent with the extreme values of my extents
# best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
#                     min(matrix_extent[2,]), max(matrix_extent[4,]))
# 
# # the range of your extent in meters
# ranges<-apply(as.matrix(best_extent), 1, diff)
# # the resolution of my cover raster
# # reso<-res(cover)
# 
# # Trying with the resolution of the temperature layers
# reso<-res(airtemp)
# 
# # dividing the range by my desired resolution gives me the number of rows and columns
# nrow_ncol<-ranges/reso
# 
# #Define CRS for all rasters
# rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# 
# # create a new raster with the desired extent and resolution
# s<-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)
# 
# # Write raster so that if I need to resample in the future, I know that this is the extent and resolution 
# # of the raster I need --> 1 km resolution with extent of 
# # -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)
# 
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Blank resampling rasters")
# writeRaster(s, "Blank resampling raster_1km resolution.tif")
# 
# # Trying with the resolution of the land cover layer
# reso<-res(cover)
# 
# # dividing the range by my desired resolution gives me the number of rows and columns
# nrow_ncol<-ranges/reso
# 
# #Define CRS for all rasters
# rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# 
# # create a new raster with the desired extent and resolution
# s2 <-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)
# 
# # Write raster so that if I need to resample in the future, I know that this is the extent and resolution 
# # of the raster I need --> 30 m resolution with extent of 
# # -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)
# writeRaster(s2, "Blank resampling raster_30m resolution.tif")









##### CREATING RASTER STACKS OF EACH DAY WITH 1 KM RESOLUTION AND THE SAME EXTENT ########

setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Blank resampling rasters")
s <- raster("Blank resampling raster_1km resolution.tif")


# USING THE FOLLOWING FILES IN THE STACKS
# WIND - NARR WIND
# SNOWMED - SNODAS
# TAIR MAX - DAYMET
# TAIR MIN - DAYMET
# DENSITY - SNODAS (derived from SWE and depth)


path1 <- "01_Analysis/Spatial Predictors"
# path1 <- "L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/"

# list the wind files
wind.list <- list.files(paste(path1, "Raster Stacks/Wind/", sep=""), pattern = "X")

# list the air temperature files
airtemp.list <- list.files(paste(path1, "Raster Stacks/Tmax and Tmin/", sep=""), pattern = "X")

# list the snow depth files
depth.list <- list.files(paste(path1, "SNODAS/Depth/", sep=""), pattern = "X")

# list the snow density files
density.list <- list.files(paste(path1, "SNODAS/Density/", sep=""), pattern = "X")


# Resample all rasters based on the created raster - took 15.23 hours for cover resolution, 23 minutes for temperature resolution
# According to https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
# If data is categorical - the resampling method must be nearest neighbor because this method is modal
# i.e. takes the mode of the neighboring cells for the new value
# If data is continuous then you can use either. Bilinear method takes the mean and is equivalent to the aggregate function


start_time <- Sys.time()
# Read in the rasters by day
for (i in 1:length(wind.list)){
  
  setwd(paste(path1, "Raster Stacks/Wind/", sep=""))
  wind <- raster(wind.list[[i]])
  
  setwd(paste(path1, "Raster Stacks/Tmax and Tmin/", sep=""))
  airtemp <- stack(airtemp.list[[i]]) # stack with tmax in first layer, tmin in 2nd layer
  
  setwd(paste(path1, "SNODAS/Depth/", sep=""))
  depth <- raster(depth.list[[i]])
  
  setwd(paste(path1, "SNODAS/Density/", sep=""))
  density <- raster(density.list[[i]])
  
  # Resample each raster using the 1 km resolution blank raster of the desired extent
  
  wind2 <- resample(wind, s, method="bilinear")
  airtemp2 <- resample(airtemp, s, method = "bilinear")
  depth2 <- resample(depth, s, method = "bilinear")
  density2 <- resample(density, s, method = "bilinear")
  
  # Stack each day together
  predictorstack <- stack(wind2, airtemp2[[1]], airtemp2[[2]], depth2, density2)
  names(predictorstack) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "Density")
  
  # Write the stacks to a folder
  setwd("01_Analysis/Spatial Predictors/Daily Stacks")
  # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
  filename <- airtemp.list[[i]]
  writeRaster(predictorstack, filename, overwrite=TRUE)
  
  print(i)
}
end_time <- Sys.time()
end_time - start_time

# Above works, I am just going to have to rename the layers when I read in each day for the BRT predictions
=======
#####################################################################
######                                                    ###########
###### CREATING RASTER STACKS WITH ALL THE PREDCITOR DATA ###########
######            to apply predict function               ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code resamples each spatial predictor raster (min and max
# temp, snow depth and density, wind speed, and land cover) to 
# have the same resolution and creates a stack of the predictors
# for each day in the winter season.

# Rasters produced:
# X2016.12.01.tif
# (and on through X2017.03.31)





########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(sp)


# ######################################################################
# ################## BELOW GETS RUN ONLY ONCE IN ORDER #################
# ################# TO EXTRACT VALUES NEEDED FOR NEW EXTENT ############
# ################# TO BE USED DURING RESAMPLING           #############
# ###############   AFTER THAT SKIP TO LINE 140          ###############
# ######################################################################
# 
# 
# 
# #Check the resolution
# res(cover)
# 
# # Check the extent
# extent(cover)
# 
# # Wind and Snow
# # Add working directory path to where downloaded data resides
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Wind and Snow")
# windandsnow <- stack("X2016.12.01.tif")
# 
# # Change the names to be exactly what they are in the boosted regression tree model
# names(windandsnow) <- c("Wind", "Snowmed")
# 
# #Check the projection
# proj4string(windandsnow)
# 
# #Check the resolution
# res(windandsnow)
# 
# 
# #Tmin and Tmax
# # Add working directory path to where downloaded data resides
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Tmax and Tmin")
# airtemp <- stack("X2016.12.01.tif")
# 
# # Change the names to be exactly what they are in the boosted regression tree model
# names(airtemp) <- c("Tairmax", "Tairmin")
# 
# #Check the projection
# proj4string(airtemp)
# 
# #Check the resolution
# res(airtemp)
# 
# # Landcover has the smallest resolution
# 
# 
# ############### Finding the right extent and resolution  #####################################
# 
# #Make a list of raster extents
# rasterlist <- list(cover, windandsnow[[1]], windandsnow[[2]], airtemp[[1]], airtemp[[2]])
# extent_list <- lapply(rasterlist, extent)
# 
# #Make a matrix out of it, each column represents a raster, rows the values
# extent_list <- lapply(extent_list, as.matrix)
# matrix_extent <- matrix(unlist(extent_list), ncol=length(extent_list))
# rownames(matrix_extent) <- c("xmin", "ymin", "xmax", "ymax")
# 
# # create an extent with the extreme values of my extents
# best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
#                     min(matrix_extent[2,]), max(matrix_extent[4,]))
# 
# # the range of your extent in meters
# ranges<-apply(as.matrix(best_extent), 1, diff)
# # the resolution of my cover raster
# # reso<-res(cover)
# 
# # Trying with the resolution of the temperature layers
# reso<-res(airtemp)
# 
# # dividing the range by my desired resolution gives me the number of rows and columns
# nrow_ncol<-ranges/reso
# 
# #Define CRS for all rasters
# rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# 
# # create a new raster with the desired extent and resolution
# s<-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)
# 
# # Write raster so that if I need to resample in the future, I know that this is the extent and resolution 
# # of the raster I need --> 1 km resolution with extent of 
# # -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)
# 
# # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Blank resampling rasters")
# writeRaster(s, "Blank resampling raster_1km resolution.tif")
# 
# # Trying with the resolution of the land cover layer
# reso<-res(cover)
# 
# # dividing the range by my desired resolution gives me the number of rows and columns
# nrow_ncol<-ranges/reso
# 
# #Define CRS for all rasters
# rastercrs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# 
# # create a new raster with the desired extent and resolution
# s2 <-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=rastercrs)
# 
# # Write raster so that if I need to resample in the future, I know that this is the extent and resolution 
# # of the raster I need --> 30 m resolution with extent of 
# # -93994.19, 1992406, 1565503, 2937203  (xmin, xmax, ymin, ymax)
# writeRaster(s2, "Blank resampling raster_30m resolution.tif")









##### CREATING RASTER STACKS OF EACH DAY WITH 1 KM RESOLUTION AND THE SAME EXTENT ########

setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/Raster Stacks/Blank resampling rasters")
s <- raster("Blank resampling raster_1km resolution.tif")


# USING THE FOLLOWING FILES IN THE STACKS
# WIND - NARR WIND
# SNOWMED - SNODAS
# TAIR MAX - DAYMET
# TAIR MIN - DAYMET
# DENSITY - SNODAS (derived from SWE and depth)


path1 <- "01_Analysis/Spatial Predictors"
# path1 <- "L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/"

# list the wind files
wind.list <- list.files(paste(path1, "Raster Stacks/Wind/", sep=""), pattern = "X")

# list the air temperature files
airtemp.list <- list.files(paste(path1, "Raster Stacks/Tmax and Tmin/", sep=""), pattern = "X")

# list the snow depth files
depth.list <- list.files(paste(path1, "SNODAS/Depth/", sep=""), pattern = "X")

# list the snow density files
density.list <- list.files(paste(path1, "SNODAS/Density/", sep=""), pattern = "X")


# Resample all rasters based on the created raster - took 15.23 hours for cover resolution, 23 minutes for temperature resolution
# According to https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
# If data is categorical - the resampling method must be nearest neighbor because this method is modal
# i.e. takes the mode of the neighboring cells for the new value
# If data is continuous then you can use either. Bilinear method takes the mean and is equivalent to the aggregate function


start_time <- Sys.time()
# Read in the rasters by day
for (i in 1:length(wind.list)){
  
  setwd(paste(path1, "Raster Stacks/Wind/", sep=""))
  wind <- raster(wind.list[[i]])
  
  setwd(paste(path1, "Raster Stacks/Tmax and Tmin/", sep=""))
  airtemp <- stack(airtemp.list[[i]]) # stack with tmax in first layer, tmin in 2nd layer
  
  setwd(paste(path1, "SNODAS/Depth/", sep=""))
  depth <- raster(depth.list[[i]])
  
  setwd(paste(path1, "SNODAS/Density/", sep=""))
  density <- raster(density.list[[i]])
  
  # Resample each raster using the 1 km resolution blank raster of the desired extent
  
  wind2 <- resample(wind, s, method="bilinear")
  airtemp2 <- resample(airtemp, s, method = "bilinear")
  depth2 <- resample(depth, s, method = "bilinear")
  density2 <- resample(density, s, method = "bilinear")
  
  # Stack each day together
  predictorstack <- stack(wind2, airtemp2[[1]], airtemp2[[2]], depth2, density2)
  names(predictorstack) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "Density")
  
  # Write the stacks to a folder
  setwd("01_Analysis/Spatial Predictors/Daily Stacks")
  # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
  filename <- airtemp.list[[i]]
  writeRaster(predictorstack, filename, overwrite=TRUE)
  
  print(i)
}
end_time <- Sys.time()
end_time - start_time

# Above works, I am just going to have to rename the layers when I read in each day for the BRT predictions
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
