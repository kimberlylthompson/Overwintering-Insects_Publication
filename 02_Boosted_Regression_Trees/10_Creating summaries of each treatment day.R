<<<<<<< HEAD
#####################################################################
######                                                    ###########
######          Prediction Summaries: mean, med,          ###########
######       min, and max, of minimum subniv temp         ###########
######             for each day/treatment                 ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# The goal of this code is to summarize the bootstrap samples to arrive at
# a raster for each day/treatment combo.

# Mean, median, minimum, and maximum for each day/treatment combo is 
# calculated although only the mean is used in the analysis.

# Rasters produced for the analysis:
# X12.01.2016_External_Mean.tif
# X12.01.2016_H3_Mean.tif
# X12.01.2016_H5_Mean.tif

# from 12.01.2016 - 03.31.2017



########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory


library(raster)
library(sp)
library(rgdal)
library(lubridate)



# Define functions to make sure NA values are ignored
mean.na <- function (x) {
  mean(x, na.rm=TRUE)
}

# median.na <- function (x) {
#   median(x, na.rm=TRUE)
# }
# 
# minimum.na <- function (x) {
#   min(x, na.rm=TRUE)
# }
# 
# maximum.na <- function (x) {
#   max(x, na.rm=TRUE)
# }




##########################
##    External          ##
##########################

# Takes 1.35 days

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_External_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Median")
  # writeRaster(median.raster, paste("X", partialname, "_External_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_External_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_External_Maximum.tif", sep = ""), overwrite = TRUE)
 
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time

rm(path, predictions, i, boot.stack, mean.raster, median.raster, minimum.raster, maximum.raster)
rm(end.time, start.time, partialname)


##########################
##  Corrected House 3   ##
##########################

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 3")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_H3_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Median")
  # writeRaster(median.raster, paste("X", partialname, "_H3_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_H3_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_H3_Maximum.tif", sep = ""), overwrite = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time



rm(path, predictions, i, boot.stack, mean.raster, median.raster, minimum.raster, maximum.raster)
rm(end.time, start.time, partialname)

##########################
##  Corrected House 5   ##
##########################

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 5")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  # 
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_H5_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Median")
  # writeRaster(median.raster, paste("X", partialname, "_H5_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_H5_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_H5_Maximum.tif", sep = ""), overwrite = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


=======
#####################################################################
######                                                    ###########
######          Prediction Summaries: mean, med,          ###########
######       min, and max, of minimum subniv temp         ###########
######             for each day/treatment                 ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# The goal of this code is to summarize the bootstrap samples to arrive at
# a raster for each day/treatment combo.

# Mean, median, minimum, and maximum for each day/treatment combo is 
# calculated although only the mean is used in the analysis.

# Rasters produced for the analysis:
# X12.01.2016_External_Mean.tif
# X12.01.2016_H3_Mean.tif
# X12.01.2016_H5_Mean.tif

# from 12.01.2016 - 03.31.2017



########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory


library(raster)
library(sp)
library(rgdal)
library(lubridate)



# Define functions to make sure NA values are ignored
mean.na <- function (x) {
  mean(x, na.rm=TRUE)
}

# median.na <- function (x) {
#   median(x, na.rm=TRUE)
# }
# 
# minimum.na <- function (x) {
#   min(x, na.rm=TRUE)
# }
# 
# maximum.na <- function (x) {
#   max(x, na.rm=TRUE)
# }




##########################
##    External          ##
##########################

# Takes 1.35 days

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_External_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Median")
  # writeRaster(median.raster, paste("X", partialname, "_External_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_External_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_External_Maximum.tif", sep = ""), overwrite = TRUE)
 
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time

rm(path, predictions, i, boot.stack, mean.raster, median.raster, minimum.raster, maximum.raster)
rm(end.time, start.time, partialname)


##########################
##  Corrected House 3   ##
##########################

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 3")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_H3_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Median")
  # writeRaster(median.raster, paste("X", partialname, "_H3_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_H3_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_H3_Maximum.tif", sep = ""), overwrite = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time



rm(path, predictions, i, boot.stack, mean.raster, median.raster, minimum.raster, maximum.raster)
rm(end.time, start.time, partialname)

##########################
##  Corrected House 5   ##
##########################

# Set wd to where the stacks for each day are
setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 5")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "20")

start.time <- Sys.time()

for(i in 1:length(predictions)){ # Number of files (i.e. days)
  
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  
  # Read in the stack
  boot.stack <- raster :: stack(predictions[[i]])
  
  # Find the mean, median, min, and max of the stack
  mean.raster <- overlay(boot.stack, fun=mean.na)
  # median.raster <- overlay(boot.stack, fun=median.na)
  # minimum.raster <- overlay(boot.stack, fun=minimum.na)
  # maximum.raster <- overlay(boot.stack, fun=maximum.na)
  # 
  
  #### Write the rasters (each type of summary has a different folder)
  
  # Extract the date from the file name
  partialname <- substr(unlist(predictions[[i]]), 1, 10)
  
  setwd("01_Analysis/Spatial Predictions/Summaries/House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Mean")
  writeRaster(mean.raster, paste("X", partialname, "_H5_Mean.tif", sep = ""), overwrite = TRUE)
  
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Median")
  # writeRaster(median.raster, paste("X", partialname, "_H5_Median.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Minimum")
  # writeRaster(minimum.raster, paste("X", partialname, "_H5_Minimum.tif", sep = ""), overwrite = TRUE)
  # 
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/Maximum")
  # writeRaster(maximum.raster, paste("X", partialname, "_H5_Maximum.tif", sep = ""), overwrite = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
