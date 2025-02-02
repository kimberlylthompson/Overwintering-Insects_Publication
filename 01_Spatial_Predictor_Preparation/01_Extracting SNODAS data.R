<<<<<<< HEAD
#################################################################################################
#########         Code for extracting snow depths and snow water                 ################
########           equivalent from downloaded SNODAS .tar files                  ################
#################################################################################################

# Author: Kimberly Thompson

# This code extracts snow depth data and snow water equivalent data from the National Snow and
# Ice Data Center for the period of 
# Dec 1 2016 - March 31 2017, which was downloaded in compressed .tar files.

# Note the Canada + US files were corrupted for Jan 21, Feb 1, Feb 3, and Feb 8;
# therefore the data for these days is for the US only.

# Creates files:
# 

########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(data.table)
library(stringr)

###########################################
####         Set Conditions            ####
###########################################

# Set directory where the downloaded files are stored
tar_dir <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded"

# Define your target extraction directory
target_dir <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Datgz files"

# List all tar files in that directory with full paths
tar_files <- list.files(tar_dir, pattern = "*.tar", full.names = TRUE)

# # List all tar files in that directory with full paths
# tar_files <- list.files(tar_dir, pattern = "_masked", full.names = TRUE)

# index <- 1
# tar_file <- tar_files[index]


# Loop through each tar file
for (tar_file in tar_files) {
  
  # Print the current tar file being processed (for debugging)
  print(paste("Processing:", tar_file))
  
  # List all files in the tar archive without extracting them
  files_in_tar <- system(paste("tar -tzf", shQuote(tar_file)), intern = TRUE)
  
  # Check if any files were listed
  if (length(files_in_tar) == 0) {
    warning(paste("No files found in", tar_file))
    next
  }
  
  # Filter for the desired patterns using regular expressions
  desired_files <- files_in_tar[str_detect(files_in_tar, "ssmv11034.*\\.dat\\.gz|ssmv11036.*\\.dat\\.gz")]
  
  # Extract only the specified .dat.gz files if they exist
  if (length(desired_files) > 0) {
    for (file in desired_files) {
      # Construct the command to extract specific files to the target directory
      cmd <- paste("tar -xzf", shQuote(tar_file), "-C", shQuote(target_dir), shQuote(file))
      
      # Print command for debugging purposes
      print(cmd)
      
      # Execute the extraction command
      system(cmd)
    }
  } else {
    warning(paste("No matching .dat.gz files found in", tar_file))
  }
  print(tar_file)
}


#######################################
# After doing procedure for masked and then 4 days of unmasked:

# Within the target directory there will now be 245 files

# We would expect to have 242 (121 days x SWE, 121 x depth).

# BUT, SWE values existed for the unmasked version (USA + Canada) for 
# days 1/21/17, 2/1/17, and 2/8/17, which is why there are an extra
# 3 files.

# Note that extra SWE files were deleted manually (uSA only)


# VAriables from website
# LIQUID PRECIPITATION
# SNOW DEPTH                                       ssmv11036
# SNOW MELT RUNOFF AT THE BASE OF THE SNOW PACK    ssmv11044
# SNOW PACK AVERAGE TEMPERATURE                    ssmv11038
# SNOW WATER EQUIVALENT                            ssmv11034
# SOLID PRECIPITATION
# SUBLIMATION FROM THE SNOW PACK                   ssmv11050
# SUBLIMATION OF BLOWING SNOW                      ssmv11039


# From .txt.gz files
# non-snow accumulation, 24 hour total                 ssmv01025SlL00
# snow accumulation, 24 hour total                     ssmv01025SlL01
# snow layer thickness, total of snow layers           ssmv11036
=======
#################################################################################################
#########         Code for extracting snow depths and snow water                 ################
########           equivalent from downloaded SNODAS .tar files                  ################
#################################################################################################

# Author: Kimberly Thompson

# This code extracts snow depth data and snow water equivalent data from the National Snow and
# Ice Data Center for the period of 
# Dec 1 2016 - March 31 2017, which was downloaded in compressed .tar files.

# Note the Canada + US files were corrupted for Jan 21, Feb 1, Feb 3, and Feb 8;
# therefore the data for these days is for the US only.

# Creates files:
# 

########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(data.table)
library(stringr)

###########################################
####         Set Conditions            ####
###########################################

# Set directory where the downloaded files are stored
tar_dir <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded"

# Define your target extraction directory
target_dir <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Datgz files"

# List all tar files in that directory with full paths
tar_files <- list.files(tar_dir, pattern = "*.tar", full.names = TRUE)

# # List all tar files in that directory with full paths
# tar_files <- list.files(tar_dir, pattern = "_masked", full.names = TRUE)

# index <- 1
# tar_file <- tar_files[index]


# Loop through each tar file
for (tar_file in tar_files) {
  
  # Print the current tar file being processed (for debugging)
  print(paste("Processing:", tar_file))
  
  # List all files in the tar archive without extracting them
  files_in_tar <- system(paste("tar -tzf", shQuote(tar_file)), intern = TRUE)
  
  # Check if any files were listed
  if (length(files_in_tar) == 0) {
    warning(paste("No files found in", tar_file))
    next
  }
  
  # Filter for the desired patterns using regular expressions
  desired_files <- files_in_tar[str_detect(files_in_tar, "ssmv11034.*\\.dat\\.gz|ssmv11036.*\\.dat\\.gz")]
  
  # Extract only the specified .dat.gz files if they exist
  if (length(desired_files) > 0) {
    for (file in desired_files) {
      # Construct the command to extract specific files to the target directory
      cmd <- paste("tar -xzf", shQuote(tar_file), "-C", shQuote(target_dir), shQuote(file))
      
      # Print command for debugging purposes
      print(cmd)
      
      # Execute the extraction command
      system(cmd)
    }
  } else {
    warning(paste("No matching .dat.gz files found in", tar_file))
  }
  print(tar_file)
}


#######################################
# After doing procedure for masked and then 4 days of unmasked:

# Within the target directory there will now be 245 files

# We would expect to have 242 (121 days x SWE, 121 x depth).

# BUT, SWE values existed for the unmasked version (USA + Canada) for 
# days 1/21/17, 2/1/17, and 2/8/17, which is why there are an extra
# 3 files.

# Note that extra SWE files were deleted manually (uSA only)


# VAriables from website
# LIQUID PRECIPITATION
# SNOW DEPTH                                       ssmv11036
# SNOW MELT RUNOFF AT THE BASE OF THE SNOW PACK    ssmv11044
# SNOW PACK AVERAGE TEMPERATURE                    ssmv11038
# SNOW WATER EQUIVALENT                            ssmv11034
# SOLID PRECIPITATION
# SUBLIMATION FROM THE SNOW PACK                   ssmv11050
# SUBLIMATION OF BLOWING SNOW                      ssmv11039


# From .txt.gz files
# non-snow accumulation, 24 hour total                 ssmv01025SlL00
# snow accumulation, 24 hour total                     ssmv01025SlL01
# snow layer thickness, total of snow layers           ssmv11036
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
