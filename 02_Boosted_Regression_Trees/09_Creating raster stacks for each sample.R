<<<<<<< HEAD
#####################################################################
######                                                    ###########
######          Creating Raster Stacks of Each            ###########
######                    CORRECTED                       ###########
######             Day/Treatment Prediction               ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code creates a raster stack of the 50 BRT spatial predictions
# for each day/treatment so that the predictions can be summarized
# by their mean.

# Rasters produced: 
# X12.01.2016_Stack of 50 samples_external.tif
# X12.01.2016_Stack of 50 samples_House 3.tif
# X12.01.2016_Stack of 50 samples_House 5.tif

# for everyday from 12.01.2016-03.31.2017




# 121 days - Dec 1 2016 - March 31 - 2017
# Corrected for the effect of the greenhouse structure --> 3 treatments:
#     current conditions (external), corrected H3, corrected H5
# 50 bootstrapped samples for each day/treatment

# I want to create a stacks by day/sample/treatment, so that I can find the 
# mean, median, minimum, and maximum for each day/treatment
# i.e. for Dec 1 external stack all 50 samples together.



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

#####################
### Data Loading ####
#####################

# Use this file to get a vector of dates
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")

subnivium<-as.data.frame(read.csv(file="Subniv Temps and Predictors_complete.csv"),
                         header = TRUE)
#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")

# Make a vector of unique dates but not with POSIXct type
date.vector <- as.character(unique(subnivium$Date))

# Change dashes to periods since that's how raster files are named
date.vector <- gsub("-", ".", date.vector)

rm(subnivium)

# Define the new CRS according to new rules 
crs.new <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"


# I don't want different samples together - I want different dates together

################# External
setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/External")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  external <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      external <- addLayer(external, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")
  writeRaster(external, paste(date.vector[i], "_Stack of 50 samples_external.tif", sep = ""),
              overwrite=TRUE)
  
  rm(external, nextlayer)
  
  print(i)
  
} # close i loop
  
end.time <- Sys.time()
end.time - start.time

rm(i, j, prediction.file, predictions, path)


################# Corrected House 3
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 3")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  corr.H3 <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      corr.H3 <- addLayer(corr.H3, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  writeRaster(corr.H3, paste(date.vector[i], "_Stack of 50 samples_House 3.tif", sep = ""),
              overwrite=TRUE)
  
  rm(corr.H3, nextlayer)
  
  print(i)
  
} # close i loop

end.time <- Sys.time()
end.time - start.time

rm(i, j, prediction.file, predictions, path)

################# Corrected House 5
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 5")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  corr.H5 <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      corr.H5 <- addLayer(corr.H5, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  writeRaster(corr.H5, paste(date.vector[i], "_Stack of 50 samples_House 5.tif", sep = ""),
              overwrite=TRUE)
  
  rm(corr.H5, nextlayer)
  
  print(i)
  
} # close i loop

end.time <- Sys.time()
end.time - start.time


=======
#####################################################################
######                                                    ###########
######          Creating Raster Stacks of Each            ###########
######                    CORRECTED                       ###########
######             Day/Treatment Prediction               ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code creates a raster stack of the 50 BRT spatial predictions
# for each day/treatment so that the predictions can be summarized
# by their mean.

# Rasters produced: 
# X12.01.2016_Stack of 50 samples_external.tif
# X12.01.2016_Stack of 50 samples_House 3.tif
# X12.01.2016_Stack of 50 samples_House 5.tif

# for everyday from 12.01.2016-03.31.2017




# 121 days - Dec 1 2016 - March 31 - 2017
# Corrected for the effect of the greenhouse structure --> 3 treatments:
#     current conditions (external), corrected H3, corrected H5
# 50 bootstrapped samples for each day/treatment

# I want to create a stacks by day/sample/treatment, so that I can find the 
# mean, median, minimum, and maximum for each day/treatment
# i.e. for Dec 1 external stack all 50 samples together.



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

#####################
### Data Loading ####
#####################

# Use this file to get a vector of dates
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")

subnivium<-as.data.frame(read.csv(file="Subniv Temps and Predictors_complete.csv"),
                         header = TRUE)
#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")

# Make a vector of unique dates but not with POSIXct type
date.vector <- as.character(unique(subnivium$Date))

# Change dashes to periods since that's how raster files are named
date.vector <- gsub("-", ".", date.vector)

rm(subnivium)

# Define the new CRS according to new rules 
crs.new <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"


# I don't want different samples together - I want different dates together

################# External
setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/External")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  external <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      external <- addLayer(external, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/External")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/External")
  writeRaster(external, paste(date.vector[i], "_Stack of 50 samples_external.tif", sep = ""),
              overwrite=TRUE)
  
  rm(external, nextlayer)
  
  print(i)
  
} # close i loop
  
end.time <- Sys.time()
end.time - start.time

rm(i, j, prediction.file, predictions, path)


################# Corrected House 3
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 3")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  corr.H3 <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      corr.H3 <- addLayer(corr.H3, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 3")
  writeRaster(corr.H3, paste(date.vector[i], "_Stack of 50 samples_House 3.tif", sep = ""),
              overwrite=TRUE)
  
  rm(corr.H3, nextlayer)
  
  print(i)
  
} # close i loop

end.time <- Sys.time()
end.time - start.time

rm(i, j, prediction.file, predictions, path)

################# Corrected House 5
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected/Corrected House 5")

# List the external  files
path <- paste(getwd(), "/", sep="")
predictions <- list.files(path, pattern = "X")

start.time <- Sys.time()

for(i in 1:length(date.vector)){ # Number of days
  
  # Set up blank stack
  corr.H5 <- stack()
  
  setwd(path)
  
  for (j in 1:length(predictions)) { # rasters for all samples and days
    
    # Extract the day identifier from the prediction file name
    prediction.file <- substr(unlist(predictions[[j]]), 2, 11)
    
    #If extracted prediction file day j matches date i, then add it to the stack
    if(date.vector[i] == prediction.file) {
      
      # Read in prediction raster  
      nextlayer <- raster :: raster(predictions[[j]])
      
      # Define the CRS since reading in the raster produces a warning
      crs(nextlayer) <- crs.new
      
      # Add the prediction layer to the raster stack
      corr.H5 <- addLayer(corr.H5, nextlayer)
    }
    
  } # close j loop (so it's checked all the prediction files)
  
  # Write the raster stack
  setwd("01_Analysis/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Stacks by day/Corrected House 5")
  writeRaster(corr.H5, paste(date.vector[i], "_Stack of 50 samples_House 5.tif", sep = ""),
              overwrite=TRUE)
  
  rm(corr.H5, nextlayer)
  
  print(i)
  
} # close i loop

end.time <- Sys.time()
end.time - start.time


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
