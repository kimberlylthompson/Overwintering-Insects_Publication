<<<<<<< HEAD
#########################################################################
###########            Corrections                       ################
###########                                              ################
###########         for each model to account for        ################
###########           artefact of the greenhouse         ################
#########################################################################

# Author: Kimberly Thompson

# This code identifies an offset between the BRT predictions
# for the external conditions and for Greenhouse +0C for each day

# The offset is then applied to the other treatments (+3C and +5)
# to remove any warming effect of the greenhouse structure.

# Rasters produced:
# X12.01.2016_H3cor_sample1.tif
# X12.01.2016_H5cor_sample1.tif

# for each day from 12.01.2016 through 03.31.2017
# and for sample1 through sample50





# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(rgdal)
library(lubridate)


#########################################################################
#############            Greenhouse Correction           ################
#########################################################################

# Goal of correction is to get the house 0 prediction to be equivalent to that 
# of the external (because this takes away the effect of the greenhouse structure)

# formula: Correction = House 0 (for that day and sample) - external (for that day and sample)
# therefore: House 0 - Correction = external
# extending to other houses: House 3 (or House 5) - Correction = predictions that remove
# effect of the greenhouse

# Since applying this correction to House 0 will make it equivalent to the external, and
# I already have the rasters for each day-sample for the external, there's no need to go
# through this process for H0 --- only for House 3 and House 5.



# Make a list of the daily raster stacks
path <- "01_Analysis/Spatial Predictions/Uncorrected/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/"
external.list <- list.files(paste(path, "External", sep=""), pattern = "X")
H0.list <- list.files(paste(path, "House 0", sep=""), pattern = "X")
H3.list <- list.files(paste(path, "House 3", sep=""), pattern = "X")
H5.list <- list.files(paste(path, "House 5", sep=""), pattern = "X")

# Define CRS argument congruent with new updates to how these are defined for proj6
# http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html
# https://gis.stackexchange.com/questions/385114/how-to-update-crs-r-object-from-proj4-to-proj6
crs.new <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"

######################################################################################
####                               HOUSE 3                                        ####
######################################################################################

for (i in 1:length(external.list)) {
  
  # Read in external day's layer
  setwd(paste(path, "External", sep=""))
  external <- raster::raster(external.list[[i]])
  crs(external) <- crs.new
  
  # Read in H0 day's layer
  setwd(paste(path, "House 0", sep=""))
  H0 <- raster::raster(H0.list[[i]])
  crs(H0) <- crs.new
  
  # Read in H3 day's layer
  setwd(paste(path, "House 3", sep=""))
  H3 <- raster::raster(H3.list[[i]])
  crs(H3) <- crs.new
  
  # Find the greenhouse correction
  correction <- H0 - external
  
  # Adjust the H3 raster by the correction
  corrected.H3 <- H3 - correction
  
  # Write resulting raster predictions
  setwd("01_Analysis/Spatial Predictions/Corrected/House 3")
  
  # Write the resulting corrected raster
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 3")
  
  # To get the file name, split the string of the predictor list
  partialname <- substr(unlist(H0.list[[i]]), 1, 11)
  partialname2 <- substr(unlist(H0.list[[i]]), 16, 23)
  
  writeRaster(corrected.H3, paste(partialname, "_H3cor_", partialname2, ".tif", sep = ""), overwrite=TRUE)
  
  print(i)
  
}



######################################################################################
####                               HOUSE 5                                        ####
######################################################################################
start.time <- Sys.time()

for (i in 1:length(external.list)) {
  
  # Read in external day's layer
  setwd(paste(path, "External", sep=""))
  external <- raster::raster(external.list[[i]])
  crs(external) <- crs.new
  
  # Read in H0 day's layer
  setwd(paste(path, "House 0", sep=""))
  H0 <- raster::raster(H0.list[[i]])
  crs(H0) <- crs.new
  
  # Read in H5 day's layer
  setwd(paste(path, "House 5", sep=""))
  H5 <- raster::raster(H5.list[[i]])
  crs(H5) <- crs.new
  
  # Find the greenhouse correction
  correction <- H0 - external
  
  # Adjust the H5 raster by the correction
  corrected.H5 <- H5 - correction
  
  # Write resulting raster predictions
  setwd("01_Analysis/Spatial Predictions/Corrected/House 5")
  
  # Write the resulting corrected raster
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 5")
  
  # To get the file name, split the string of the predictor list
  partialname <- substr(unlist(H0.list[[i]]), 1, 11)
  partialname2 <- substr(unlist(H0.list[[i]]), 16, 23)
  
  writeRaster(corrected.H5, paste(partialname, "_H5cor_", partialname2, ".tif", sep = ""), overwrite=TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time
=======
#########################################################################
###########            Corrections                       ################
###########                                              ################
###########         for each model to account for        ################
###########           artefact of the greenhouse         ################
#########################################################################

# Author: Kimberly Thompson

# This code identifies an offset between the BRT predictions
# for the external conditions and for Greenhouse +0C for each day

# The offset is then applied to the other treatments (+3C and +5)
# to remove any warming effect of the greenhouse structure.

# Rasters produced:
# X12.01.2016_H3cor_sample1.tif
# X12.01.2016_H5cor_sample1.tif

# for each day from 12.01.2016 through 03.31.2017
# and for sample1 through sample50





# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(rgdal)
library(lubridate)


#########################################################################
#############            Greenhouse Correction           ################
#########################################################################

# Goal of correction is to get the house 0 prediction to be equivalent to that 
# of the external (because this takes away the effect of the greenhouse structure)

# formula: Correction = House 0 (for that day and sample) - external (for that day and sample)
# therefore: House 0 - Correction = external
# extending to other houses: House 3 (or House 5) - Correction = predictions that remove
# effect of the greenhouse

# Since applying this correction to House 0 will make it equivalent to the external, and
# I already have the rasters for each day-sample for the external, there's no need to go
# through this process for H0 --- only for House 3 and House 5.



# Make a list of the daily raster stacks
path <- "01_Analysis/Spatial Predictions/Uncorrected/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/"
external.list <- list.files(paste(path, "External", sep=""), pattern = "X")
H0.list <- list.files(paste(path, "House 0", sep=""), pattern = "X")
H3.list <- list.files(paste(path, "House 3", sep=""), pattern = "X")
H5.list <- list.files(paste(path, "House 5", sep=""), pattern = "X")

# Define CRS argument congruent with new updates to how these are defined for proj6
# http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html
# https://gis.stackexchange.com/questions/385114/how-to-update-crs-r-object-from-proj4-to-proj6
crs.new <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"

######################################################################################
####                               HOUSE 3                                        ####
######################################################################################

for (i in 1:length(external.list)) {
  
  # Read in external day's layer
  setwd(paste(path, "External", sep=""))
  external <- raster::raster(external.list[[i]])
  crs(external) <- crs.new
  
  # Read in H0 day's layer
  setwd(paste(path, "House 0", sep=""))
  H0 <- raster::raster(H0.list[[i]])
  crs(H0) <- crs.new
  
  # Read in H3 day's layer
  setwd(paste(path, "House 3", sep=""))
  H3 <- raster::raster(H3.list[[i]])
  crs(H3) <- crs.new
  
  # Find the greenhouse correction
  correction <- H0 - external
  
  # Adjust the H3 raster by the correction
  corrected.H3 <- H3 - correction
  
  # Write resulting raster predictions
  setwd("01_Analysis/Spatial Predictions/Corrected/House 3")
  
  # Write the resulting corrected raster
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 3")
  
  # To get the file name, split the string of the predictor list
  partialname <- substr(unlist(H0.list[[i]]), 1, 11)
  partialname2 <- substr(unlist(H0.list[[i]]), 16, 23)
  
  writeRaster(corrected.H3, paste(partialname, "_H3cor_", partialname2, ".tif", sep = ""), overwrite=TRUE)
  
  print(i)
  
}



######################################################################################
####                               HOUSE 5                                        ####
######################################################################################
start.time <- Sys.time()

for (i in 1:length(external.list)) {
  
  # Read in external day's layer
  setwd(paste(path, "External", sep=""))
  external <- raster::raster(external.list[[i]])
  crs(external) <- crs.new
  
  # Read in H0 day's layer
  setwd(paste(path, "House 0", sep=""))
  H0 <- raster::raster(H0.list[[i]])
  crs(H0) <- crs.new
  
  # Read in H5 day's layer
  setwd(paste(path, "House 5", sep=""))
  H5 <- raster::raster(H5.list[[i]])
  crs(H5) <- crs.new
  
  # Find the greenhouse correction
  correction <- H0 - external
  
  # Adjust the H5 raster by the correction
  corrected.H5 <- H5 - correction
  
  # Write resulting raster predictions
  setwd("01_Analysis/Spatial Predictions/Corrected/House 5")
  
  # Write the resulting corrected raster
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 5")
  
  # To get the file name, split the string of the predictor list
  partialname <- substr(unlist(H0.list[[i]]), 1, 11)
  partialname2 <- substr(unlist(H0.list[[i]]), 16, 23)
  
  writeRaster(corrected.H5, paste(partialname, "_H5cor_", partialname2, ".tif", sep = ""), overwrite=TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
