#################################################################################################
#########            Converting SNODAS data in .dat.gz files to rasters          ################
########                  (snow depth and snow water equivalent)                 ################
#################################################################################################

# Author: Kimberly Thompson

# This code converts snow depth data and snow water equivalent data contained in .dat.gz
# files to raster files (.tif).
# One file for each day in the 
# Dec 1 2016 - March 31 2017

# Creates files:
# 

########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(terra)
library(data.table)
library(R.utils)


###########################################
####      Define file conditions       ####
###########################################

# Define directory where .dat.gz files are stored
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Datgz files")

# Define file patterns
swe_pattern <- "11034"
snow_depth_pattern <- "11036"

# List all .dat.gz files 
all_files <- list.files(pattern = "\\.dat\\.gz")

# Separate files into SWE and snow depth
swe_files <- all_files[grepl(swe_pattern, all_files)]
snow_depth_files <- all_files[grepl(snow_depth_pattern, all_files)]


###########################################
####    Converting files to rasters    ####
###########################################

# Initialize empty raster stacks
swe_stack <- rast()
snow_depth_stack <- rast()

# For testing with 1 file before running the loop
# index <- 1
# file <- swe_files[index]

# Extract data
for (file in swe_files) {
  
  gunzip(file, remove = FALSE)
  
  # Read data (assuming it's in a suitable format for terra)
  temp_raster <- rast(file)
  
  # Set CRS to Albers Equal Area as specified
  crs(temp_raster) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # Name layer according to date extracted from filename
  date_str <- sub(".*_(\\d{8}).*", "\\1", file) # Extract date from filename
  layer_name <- paste0("X", gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1.\\2.\\3", date_str), "_SWE")
  names(temp_raster) <- layer_name
  
  # Add to SWE stack
  swe_stack <- c(swe_stack, temp_raster)
  
  print(file)
}

for (file in snow_depth_files) {
  temp_raster <- rast(file)
  
  crs(temp_raster) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  date_str <- sub(".*_(\\d{8}).*", "\\1", file)
  layer_name <- paste0("X", gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1.\\2.\\3", date_str), "_SnowDepth")
  names(temp_raster) <- layer_name
  
  snow_depth_stack <- c(snow_depth_stack, temp_raster)
}
