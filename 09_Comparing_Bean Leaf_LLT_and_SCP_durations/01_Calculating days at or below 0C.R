<<<<<<< HEAD
#########################################################################################################
###########      Lower lethal Temp until 50% mortality - Reclassifying and calculating   ################
###########               the longest consecutive stretch of temps below a threshold     ################
###########                 based on literature experimental results for bumblebees      ################
#########################################################################################################

# Author: Kimberly Thompson

# This script calculates the longest consecutive number of days with ground temperatures
# at or below 0C for each scenario: current conditions, +3C, +5C.

# This is done for mean, median, min, and max estimates of ground temperature however
# only the mean is used in the manuscript.

# Rasters produced:
# LLT50_0Cdays_Mean_External.tif
# LLT50_0Cdays_Mean_3C Warmer.tif
# LLT50_0Cdays_Mean_5C Warmer.tif


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)


#########################################################################
#############      Calculating Consecutive Occurrences   ################
#########################################################################


# From the corrected dailies predictions of minimum temperature, read in each file, make a stack, then apply a formula to get 
# a raster showing the longest consecutive stretch of temps below the threshold of 0C for each raster cell.

# Create a function that would not take into account NAs when counting up the consecutive occurrences
# Based on example found at https://stackoverflow.com/questions/56255507/calculate-maximum-length-of-consecutive-days-above-a-certain-threshold-in-a-rast

cd <- function(x,t){
  
  y <- rle((x <= t)*1)
  
  y$values[is.na(y$values) == TRUE] <- 0
  
  z <- y$lengths[y$values==1]
  
  return(max(z,0))
  
}


############################################################
####### Interior Lakes Dataframe ##########################
############################################################

# To overwrite interior lakes with NAs after the analysis is complete

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)



#############################
#######  External  ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/External"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }

  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA

  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_External.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time
  

# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)


#############################
#######  3C Warmer ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/House 3"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }
  
  # I have this in here to be thorough but I don't think I need to have the whole raster stack written
  # It takes a huge amount of processing time to write such large stacks
  # Write the resulting raster stack
  # setwd(paste(write.path, "Reclassified Stacks", sep=""))
  # writeRaster(newstack, paste("LLT_Bombus_", base.list[[j]], "_External Daily Stack.tif", sep = ""), overwrite = TRUE)
  
  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA
  
  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_3C Warmer.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time


# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)



#############################
#######  5C Warmer ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/House 5"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }
  
  # I have this in here to be thorough but I don't think I need to have the whole raster stack written
  # It takes a huge amount of processing time to write such large stacks
  # Write the resulting raster stack
  # setwd(paste(write.path, "Reclassified Stacks", sep=""))
  # writeRaster(newstack, paste("LLT_Bombus_", base.list[[j]], "_External Daily Stack.tif", sep = ""), overwrite = TRUE)
  
  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA
  
  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_5C Warmer.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time


# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)

=======
#########################################################################################################
###########      Lower lethal Temp until 50% mortality - Reclassifying and calculating   ################
###########               the longest consecutive stretch of temps below a threshold     ################
###########                 based on literature experimental results for bumblebees      ################
#########################################################################################################

# Author: Kimberly Thompson

# This script calculates the longest consecutive number of days with ground temperatures
# at or below 0C for each scenario: current conditions, +3C, +5C.

# This is done for mean, median, min, and max estimates of ground temperature however
# only the mean is used in the manuscript.

# Rasters produced:
# LLT50_0Cdays_Mean_External.tif
# LLT50_0Cdays_Mean_3C Warmer.tif
# LLT50_0Cdays_Mean_5C Warmer.tif


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)


#########################################################################
#############      Calculating Consecutive Occurrences   ################
#########################################################################


# From the corrected dailies predictions of minimum temperature, read in each file, make a stack, then apply a formula to get 
# a raster showing the longest consecutive stretch of temps below the threshold of 0C for each raster cell.

# Create a function that would not take into account NAs when counting up the consecutive occurrences
# Based on example found at https://stackoverflow.com/questions/56255507/calculate-maximum-length-of-consecutive-days-above-a-certain-threshold-in-a-rast

cd <- function(x,t){
  
  y <- rle((x <= t)*1)
  
  y$values[is.na(y$values) == TRUE] <- 0
  
  z <- y$lengths[y$values==1]
  
  return(max(z,0))
  
}


############################################################
####### Interior Lakes Dataframe ##########################
############################################################

# To overwrite interior lakes with NAs after the analysis is complete

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)



#############################
#######  External  ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/External"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }

  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA

  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_External.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time
  

# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)


#############################
#######  3C Warmer ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/House 3"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 3/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }
  
  # I have this in here to be thorough but I don't think I need to have the whole raster stack written
  # It takes a huge amount of processing time to write such large stacks
  # Write the resulting raster stack
  # setwd(paste(write.path, "Reclassified Stacks", sep=""))
  # writeRaster(newstack, paste("LLT_Bombus_", base.list[[j]], "_External Daily Stack.tif", sep = ""), overwrite = TRUE)
  
  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA
  
  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_3C Warmer.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time


# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)



#############################
#######  5C Warmer ##########
#############################

# Since I have 4 different measures to do this for (mean, median, min, and max [of the 50 bootstrapped samples for daily
# minimum temperature]), make a loop

# Path of the summary folders
base.path <- "01_Analysis/Spatial Predictions/Summaries/House 5"
# base.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/Corrected House 5/"
base.list <- list.files(base.path, pattern = "M")

# Path to write resulting raster stacks and final raster
write.path <- "01_Analysis/Spatial Predictions/LLT50"
# write.path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees/"


# Determining sensitivity based on LLT50
start.time <- Sys.time()

for (j in 1:4) { # Number of summary measures to calculate for
  
  path <- paste(base.path, base.list[[j]], sep = "")
  Treatment.list <- list.files(path, pattern = "X")
  
  # Make a blank raster to store the daily layers in
  newstack <- stack()
  
  for (i in 1:length(Treatment.list)) {
    
    # Read in the daily raster
    setwd(path)
    subniv <- raster :: raster(Treatment.list[[i]])
    
    # Add the layer to the blank raster stack for this treatment
    newstack <- addLayer(newstack, subniv)
    
    print(i)
    
  }
  
  # I have this in here to be thorough but I don't think I need to have the whole raster stack written
  # It takes a huge amount of processing time to write such large stacks
  # Write the resulting raster stack
  # setwd(paste(write.path, "Reclassified Stacks", sep=""))
  # writeRaster(newstack, paste("LLT_Bombus_", base.list[[j]], "_External Daily Stack.tif", sep = ""), overwrite = TRUE)
  
  
  # Apply the function to find the longest stretch of consecutive days for which the temp was equal to
  # or below the threshold
  # does not include correction for true lakes yet
  duration <- calc(newstack, function(x) cd(x,0)) # temp values less than or equal to 0C
  
  # Convert duration to a dataframe
  duration.dataframe <- as.data.frame(duration, xy=TRUE)
  
  # Merge the two dataframes together
  df <- merge(duration.dataframe, separating.lakes, by.x = c("x", "y"), by.y = c("x", "y"), all=TRUE)
  
  # Change layer.x value to NA where layer.y value is 605
  df$layer.x[df$layer.y == 6050] <- NA
  
  # Remove layer.y column
  df$layer.y <- NULL
  
  # Convert this dataframe to a raster
  # create spatial points data frame
  spg <- df
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster(spg)
  
  # Define the coordinate system
  crs(rasterDF) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  
  # Write the resulting raster layer
  setwd(write.path)
  writeRaster(rasterDF, paste("LLT50_0Cdays_", base.list[[j]], "_5C Warmer.tif", sep = ""), overwrite = TRUE)
  
  print(j)
  
} # Close j loop

end.time <- Sys.time()
end.time - start.time


# Clean up workspace before beginning on next treatment
rm(base.list, base.path, duration, newstack, subniv, Treatment.list, path, rasterDF, duration.dataframe, df, spg, i)
rm(j, write.path, start.time, end.time)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
