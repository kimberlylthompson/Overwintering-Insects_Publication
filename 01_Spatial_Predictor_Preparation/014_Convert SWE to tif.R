#################################################################################################
#########            Converting SNODAS data in .dat files to rasters             ################
########                           SNOW WATER EQUIVALENT                         ################
#################################################################################################

# Author: Kimberly Thompson

# This code converts snow water equivalent data contained in .dat
# files to raster files (.tif).
# One file for each day in the 
# Dec 1 2016 - March 31 2017

# Since four files for the USA/Canada extent were corrupted, it also 
# uses data for the US for those days and the average of the adjacent 
# days for the Canadian data.

# Note: run on HPC cluster due to memory requirements


# Creates files:
# 

########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(terra)
library(raster)
library(rgdal)
library(data.table)
library(R.utils)
library(stringr)


###########################################
####      Define file conditions       ####
###########################################

# Define directory where .dat files are stored
# (not stored in Github 00_Data folder)
# setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Datgz files")
# setwd("~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/SNODAS/Datgz files")
setwd("/gpfs1/data/iupdate/Modeling_Inputs2/Overwintering_Insects/SNODAS")

# Define file patterns
swe_pattern <- "11034"
snow_depth_pattern <- "11036"



###########################################
####    Converting files to rasters    ####
###########################################

# List all .dat files (now the .gz will be gone)
all_files <- list.files(pattern = "\\.dat")

# Separate files into SWE and snow depth
swe_files <- all_files[grepl(swe_pattern, all_files)]
# snow_depth_files <- all_files[grepl(snow_depth_pattern, all_files)]

# Initialize empty raster stacks
# USA and USA+Can have different extents, so keep separate for now
swe_stack_usa <- raster :: stack()
swe_stack_can <- raster :: stack()
# depth_stack_usa <- raster :: stack()
# depth_stack_can <- raster :: stack()

# Extract data
for (i in 1:length(swe_files)) {
  
  # Define input .dat file and output raster file names
  input_file <- swe_files[i]
  
  # Extract the data from the file name
  date_str <- str_extract(input_file, "\\d{8}")
  
  # Convert date to how I want the layers to be named
  formatted_date <- paste0("X", substr(date_str, 1, 4), "-", 
                           substr(date_str, 5, 6), "-", 
                           substr(date_str, 7, 8))
  
  # Read the binary file but this differs between masked and unmasked versions
  if(stringr :: str_detect(input_file, pattern = "us")) {
    
    # Read the binary file
    swe_data <- readBin(input_file, "integer", n = 6935 * 3351, size = 2, signed = TRUE)
    
    # Create a raster object
    swe.tmp_raster <- raster :: raster(nrows = 3351, ncols = 6935, xmn = -124.733333333333, 
                                       xmx = -66.9416666666667, ymn = 24.95, ymx = 52.8749999999999, 
                                       crs = "+proj=longlat +datum=WGS84")
    
    # Assign values to the raster
    values(swe.tmp_raster) <- swe_data
    
    # Name the raster layer
    names(swe.tmp_raster) <- formatted_date
    
    # Add the temporary raster to the stack
    swe_stack_usa <- raster :: addLayer(swe_stack_usa, swe.tmp_raster)
    
  } else {
    
    # Read the binary file
    swe_data <- readBin(input_file, "integer", n = 8192 * 4096, size = 2, signed = TRUE)
    
    # Create a raster object
    swe.tmp_raster <- raster :: raster(nrows = 4096, ncols = 8192, xmn = -130.51250000000002, 
                                       xmx = -66.9416666666667, ymn = 24.95, ymx = 58.229166666666664, 
                                       crs = "+proj=longlat +datum=WGS84")
    
    # Assign values to the raster
    values(swe.tmp_raster) <- swe_data
    
    # Name the raster layer
    names(swe.tmp_raster) <- formatted_date
    
    # Add the temporary raster to the stack
    swe_stack_can <- addLayer(swe_stack_can, swe.tmp_raster)
    
  } # end of if/else
  print(i)
} # end of swe loop


###########################################
####                                   ####
####      Filling in missing days      ####
####                                   ####
###########################################

# Convert the Rasterstacks to spatrasters
can <- rast(swe_stack_can)
usa <- rast(swe_stack_usa)

# Resample USA data
usa_resampled <- resample(usa, can)

# Create new layers for missing dates

# Dates of missing layers
missing_dates <- c("2017-01-21", "2017-02-01", "2017-02-03", "2017-02-08")

for (date in missing_dates) {
  # Find the index of the USA layer for this date
  usa_index <- which(names(usa_resampled) == paste0("X", gsub("-", ".", date)))
  
  # Find the indices of the preceding and subsequent days in the CAN stack
  can_index <- which(as.Date(gsub("X", "", names(can), fixed=TRUE), format="%Y.%m.%d") == as.Date(date))
  prev_index <- can_index - 1
  next_index <- can_index + 1
  
  # Create a new layer
  new_layer <- can[[1]]  # Template layer with correct extent
  
  # Fill values where extents match
  common_extent <- ext(intersect(ext(can), ext(usa_resampled)))
  new_layer[common_extent] <- usa_resampled[[usa_index]][common_extent]
  
  # Fill remaining values with average of preceding and subsequent days
  mask <- is.na(new_layer)
  new_layer[mask] <- (can[[prev_index]][mask] + can[[next_index]][mask]) / 2
  
  # Add the new layer to the CAN stack
  can <- c(can[1:prev_index], new_layer, can[next_index:nlyr(can)])
  names(can)[can_index] <- paste0("X", gsub("-", ".", date))
}

# Ensure the layers are ordered correctly

# Sort layers by date
layer_dates <- as.Date(gsub("X", "", names(can), fixed=TRUE), format="%Y.%m.%d")
can <- can[[order(layer_dates)]]

# Write the raster stack
path <- "/gpfs1/data/iupdate/Analysis_Output2/Insects/"
writeRaster(can, paste(path, "SWE Canada USA.tif", sep = ""))


