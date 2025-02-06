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
library(raster)
library(rgdal)
library(data.table)
library(R.utils)
library(stringr)


###########################################
####      Define file conditions       ####
###########################################

# Define directory where .dat.gz files are stored
# (not stored in Github 00_Data folder)
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Datgz files")

# Define file patterns
swe_pattern <- "11034"
snow_depth_pattern <- "11036"

# List all .dat.gz files 
all_files <- list.files(pattern = "\\.dat\\.gz")


###########################################
####    Uncompressing .gz files        ####
###########################################

# Extract data from compressed .gz files
for (i in 1:length(all_files)) {
  
  # Extract the contents of the .gz file
  gunzip(all_files[i], remove = TRUE)
  
  print(i)

# Separate files into SWE and snow depth
swe_files <- all_files[grepl(swe_pattern, all_files)]
snow_depth_files <- all_files[grepl(snow_depth_pattern, all_files)]


###########################################
####    Converting files to rasters    ####
###########################################

# Initialize empty raster stacks
swe_stack <- rast()
snow_depth_stack <- rast()

# Extract data from compressed .gz files
for (file in all_files) {
  
  # Extract the contents of the .gz file
  gunzip(file, remove = TRUE)

  
}



###########################################
####    Converting files to rasters    ####
###########################################

# List all .dat files (now the .gz will be gone)
all_files <- list.files(pattern = "\\.dat")

# Separate files into SWE and snow depth
swe_files <- all_files[grepl(swe_pattern, all_files)]
snow_depth_files <- all_files[grepl(snow_depth_pattern, all_files)]

# Initialize empty raster stacks
# USA and USA+Can have different extents, so keep separate for now
swe_stack_usa <- raster :: stack()
swe_stack_can <- raster :: stack()
depth_stack_usa <- raster :: stack()
depth_stack_can <- raster :: stack()

# Extract data
# SWE
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







# 53: swe_data large integer 17789648 elements
# 52: swe_data large integer 33554432 elements
    swe_stack <- c(swe_stack, swe.tmp_raster)
    
  } else {
    
    if(stringr :: str_detect(input_file, pattern = "zz")) {
      
      # Read the binary file
      swe_data <- readBin(input_file, "integer", n = 8192 * 4096, size = 2, signed = TRUE)
      
      # Create a raster object
      swe.tmp_raster <- terra :: rast(nrows = 4096, ncols = 8192, xmn = -130.51250000000002, 
                                      xmx = -66.9416666666667, ymn = 24.95, ymx = 58.229166666666664, 
                                      crs = "+proj=longlat +datum=WGS84")
      
      # Assign values to the raster
      values(swe.tmp_raster) <- swe_data
      
      # Name the raster layer
      names(swe.tmp_raster) <- formatted_date
      
      # Add the temporary raster to the stack
      swe_stack <- c(swe_stack, swe.tmp_raster)
      
    } # end of else
  } # end of if
  print(i)
} # end of swe loop
  







# Extract data
for (file in swe_files) {
  
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
