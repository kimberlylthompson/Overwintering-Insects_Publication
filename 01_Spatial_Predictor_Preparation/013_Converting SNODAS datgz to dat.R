#################################################################################################
#########            Converting SNODAS data in .dat.gz files to .dat files       ################
########                  (snow depth and snow water equivalent)                 ################
#################################################################################################

# Author: Kimberly Thompson

# This code converts snow depth data and snow water equivalent data contained in .dat.gz
# files to .dat .


# (step along the way to getting data into rasters)

########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory


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
  
}

