<<<<<<< HEAD
######################################################################################
###########      Comparing the vulnerability shown by SCP results     ################
###########         with the vulnerability shown by LLT results       ################
###########                   for bean leaf beetles                  ################
######################################################################################

# Author: Kimberly Thompson

# This script takes the rasters that show the longest consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Comparison between LLT and SCP based on best-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.


# Datasets produced:
# 'Comparing LLTime with days under SCP_external - best case.csv'
# 'Comparing LLTime with days under SCP_3Cwarmer - best case.csv'
# 'Comparing LLTime with days under SCP_5Cwarmer - best case.csv'



# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)


# Workflow - for each scenario
# 1. Reclassify LLT raster to three values (based on green yellow and red in map, and the LLT values extracted from
#      Lam and Pedigo (2000).

# 2. Then use the SCP raster to extract the number of SCP days - essentially create a dataframe for each of the 
#      scenarios.



#########################################################################
#############              Changing Cell values          ################
#########################################################################

# Beanleaf beetle - From Lam and Pedigo 2000, lower confidence limit = 28.87259, upper confidence limit = 41.32808

# From the maps of most consecutive days of below 0C temperatures, resample so that:
#     1. < 28.87259 the cell gets a 0
#     2. >= 28.87259 and <= 41.32808 the cell gets a 1
#     3. > 41.32808 the cell gets a 2



########################################
####    Interior Lakes Data          ###
########################################

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)


#############################
###  Raster Loading    ######
#############################

#### LLT50 AT 0C RASTERS ######

# I have four summary measures # of consecutive stretches of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees")

dur0_mean <- raster :: raster("LLT50_0Cdays_Mean_External.tif")
dur3_mean <- raster :: raster("LLT50_0Cdays_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("LLT50_0Cdays_Mean_5C warmer.tif")

dur0_median <- raster :: raster("LLT50_0Cdays_Median_External.tif")
dur3_median <- raster :: raster("LLT50_0Cdays_Median_3C warmer.tif")
dur5_median <- raster :: raster("LLT50_0Cdays_Median_5C warmer.tif")

dur0_min <- raster :: raster("LLT50_0Cdays_Minimum_External.tif")
dur3_min <- raster :: raster("LLT50_0Cdays_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("LLT50_0Cdays_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("LLT50_0Cdays_Maximum_External.tif")
dur3_max <- raster :: raster("LLT50_0Cdays_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("LLT50_0Cdays_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_llt <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# Change the name to Bean leaf so it saves correctly
# names(rasterlist_llt) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist_llt))



#############################
###  SCP Raster Loading   ###
#############################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Bean Leaf")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Green Leaf Beetle")

dur0_mean <- raster :: raster("SCP_Bean Leaf_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Bean Leaf_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Bean Leaf_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Bean Leaf_Median_External.tif")
dur3_median <- raster :: raster("SCP_Bean Leaf_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Bean Leaf_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Bean Leaf_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Bean Leaf_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Bean Leaf_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Bean Leaf_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Bean Leaf_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Bean Leaf_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_scp <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                        dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)



### Getting read for loop #########

wgs84 = sp:: CRS("+init=epsg:4326")

# To help with the raster processing
# Load in the states shapefile
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#### Make adjustments to both raster stacks so I calculate the summary stats for only the extent I'll be presenting in the figures

# transform each raster into degrees so that the axes will look
# better and be clearer to understand

rasterlist_llt <- projectRaster(from=rasterlist_llt, crs =  wgs84)
rasterlist_scp <- projectRaster(from=rasterlist_scp, crs =  wgs84)

# Convert projection of states so that it matches rasters:
states <- spTransform( states, proj4string( rasterlist_llt ) )

# Define the extent for the states
newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the rasters to match
rasterlist_llt <- crop(rasterlist_llt, extent(newstates))
rasterlist_llt <- raster :: mask(rasterlist_llt, newstates)

rasterlist_scp <- crop(rasterlist_scp, extent(newstates))
rasterlist_scp <- raster :: mask(rasterlist_scp, newstates)

  
## Reclassifying the rastet 
for (i in 1:nlayers(rasterlist_llt)) { 
  
  # Reclassify the raster
  rasterlist_llt[[i]][rasterlist_llt[[i]] < 28.87259] <- 0
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] >= 28.87259 & rasterlist_llt[[i]] <= 41.32808] <- 1
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] > 41.32808] <- 2
  
  print(i)
  
}



#### Loop to create a dataframe of days under SCP that also shows whether the cells falls into below, between, or above the
#### days until LLT50.

# This loop is not actually finished! I needed to accelerate the process so only did if for the mean and then manually wrote
# the csv files

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")

for (i in 1:length(rasterlist_llt)) { # to do just the mean maps which is what I'm presenting just say 1:3 rather than
  # length(rasterlist_llt)
  
  # Convert the LLT raster to a dataframe
  lltdata <- as.data.frame(rasterlist_llt[[i]], xy=TRUE)
  
  # Convert the SCP raster to a dataframe
  scpdata <- as.data.frame(rasterlist_scp[[i]], xy=TRUE)
  
  
  # Add the LLT info to the SCP dataframe
  scpdata <- cbind(scpdata, lltdata[, 3])
  
  # Rename the columns
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_External", "LLT_Bean.Leaf_Mean_External")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_3Cwarmer", "LLT_Bean.Leaf_Mean_3Cwarmer")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_5Cwarmer", "LLT_Bean.Leaf_Mean_5Cwarmer")
  
  # write.csv("")
  
}

#External
write.csv(scpdata, "Comparing LLTime with days under SCP_external - best case.csv", row.names = FALSE)

external <- scpdata

# 3C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_3Cwarmer - best case.csv", row.names = FALSE)

H3 <- scpdata

# 5C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_5Cwarmer - best case.csv", row.names = FALSE)

H5 <- scpdata


#### Long form data used in summarizing
external.summary <- Summarize(SCP_Bean.Leaf_Mean_External ~ LLT_Bean.Leaf_Mean_External, data = external)
external.summary

H3.summary <- Summarize(SCP_Bean.Leaf_Mean_3Cwarmer ~ LLT_Bean.Leaf_Mean_3Cwarmer, data = H3)
H3.summary

H5.summary <- Summarize(SCP_Bean.Leaf_Mean_5Cwarmer ~ LLT_Bean.Leaf_Mean_5Cwarmer, data = H5)
H5.summary


=======
######################################################################################
###########      Comparing the vulnerability shown by SCP results     ################
###########         with the vulnerability shown by LLT results       ################
###########                   for bean leaf beetles                  ################
######################################################################################

# Author: Kimberly Thompson

# This script takes the rasters that show the longest consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Comparison between LLT and SCP based on best-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.


# Datasets produced:
# 'Comparing LLTime with days under SCP_external - best case.csv'
# 'Comparing LLTime with days under SCP_3Cwarmer - best case.csv'
# 'Comparing LLTime with days under SCP_5Cwarmer - best case.csv'



# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)


# Workflow - for each scenario
# 1. Reclassify LLT raster to three values (based on green yellow and red in map, and the LLT values extracted from
#      Lam and Pedigo (2000).

# 2. Then use the SCP raster to extract the number of SCP days - essentially create a dataframe for each of the 
#      scenarios.



#########################################################################
#############              Changing Cell values          ################
#########################################################################

# Beanleaf beetle - From Lam and Pedigo 2000, lower confidence limit = 28.87259, upper confidence limit = 41.32808

# From the maps of most consecutive days of below 0C temperatures, resample so that:
#     1. < 28.87259 the cell gets a 0
#     2. >= 28.87259 and <= 41.32808 the cell gets a 1
#     3. > 41.32808 the cell gets a 2



########################################
####    Interior Lakes Data          ###
########################################

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)


#############################
###  Raster Loading    ######
#############################

#### LLT50 AT 0C RASTERS ######

# I have four summary measures # of consecutive stretches of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees")

dur0_mean <- raster :: raster("LLT50_0Cdays_Mean_External.tif")
dur3_mean <- raster :: raster("LLT50_0Cdays_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("LLT50_0Cdays_Mean_5C warmer.tif")

dur0_median <- raster :: raster("LLT50_0Cdays_Median_External.tif")
dur3_median <- raster :: raster("LLT50_0Cdays_Median_3C warmer.tif")
dur5_median <- raster :: raster("LLT50_0Cdays_Median_5C warmer.tif")

dur0_min <- raster :: raster("LLT50_0Cdays_Minimum_External.tif")
dur3_min <- raster :: raster("LLT50_0Cdays_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("LLT50_0Cdays_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("LLT50_0Cdays_Maximum_External.tif")
dur3_max <- raster :: raster("LLT50_0Cdays_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("LLT50_0Cdays_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_llt <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# Change the name to Bean leaf so it saves correctly
# names(rasterlist_llt) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist_llt))



#############################
###  SCP Raster Loading   ###
#############################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Bean Leaf")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Green Leaf Beetle")

dur0_mean <- raster :: raster("SCP_Bean Leaf_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Bean Leaf_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Bean Leaf_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Bean Leaf_Median_External.tif")
dur3_median <- raster :: raster("SCP_Bean Leaf_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Bean Leaf_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Bean Leaf_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Bean Leaf_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Bean Leaf_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Bean Leaf_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Bean Leaf_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Bean Leaf_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_scp <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                        dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)



### Getting read for loop #########

wgs84 = sp:: CRS("+init=epsg:4326")

# To help with the raster processing
# Load in the states shapefile
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#### Make adjustments to both raster stacks so I calculate the summary stats for only the extent I'll be presenting in the figures

# transform each raster into degrees so that the axes will look
# better and be clearer to understand

rasterlist_llt <- projectRaster(from=rasterlist_llt, crs =  wgs84)
rasterlist_scp <- projectRaster(from=rasterlist_scp, crs =  wgs84)

# Convert projection of states so that it matches rasters:
states <- spTransform( states, proj4string( rasterlist_llt ) )

# Define the extent for the states
newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the rasters to match
rasterlist_llt <- crop(rasterlist_llt, extent(newstates))
rasterlist_llt <- raster :: mask(rasterlist_llt, newstates)

rasterlist_scp <- crop(rasterlist_scp, extent(newstates))
rasterlist_scp <- raster :: mask(rasterlist_scp, newstates)

  
## Reclassifying the rastet 
for (i in 1:nlayers(rasterlist_llt)) { 
  
  # Reclassify the raster
  rasterlist_llt[[i]][rasterlist_llt[[i]] < 28.87259] <- 0
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] >= 28.87259 & rasterlist_llt[[i]] <= 41.32808] <- 1
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] > 41.32808] <- 2
  
  print(i)
  
}



#### Loop to create a dataframe of days under SCP that also shows whether the cells falls into below, between, or above the
#### days until LLT50.

# This loop is not actually finished! I needed to accelerate the process so only did if for the mean and then manually wrote
# the csv files

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")

for (i in 1:length(rasterlist_llt)) { # to do just the mean maps which is what I'm presenting just say 1:3 rather than
  # length(rasterlist_llt)
  
  # Convert the LLT raster to a dataframe
  lltdata <- as.data.frame(rasterlist_llt[[i]], xy=TRUE)
  
  # Convert the SCP raster to a dataframe
  scpdata <- as.data.frame(rasterlist_scp[[i]], xy=TRUE)
  
  
  # Add the LLT info to the SCP dataframe
  scpdata <- cbind(scpdata, lltdata[, 3])
  
  # Rename the columns
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_External", "LLT_Bean.Leaf_Mean_External")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_3Cwarmer", "LLT_Bean.Leaf_Mean_3Cwarmer")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_5Cwarmer", "LLT_Bean.Leaf_Mean_5Cwarmer")
  
  # write.csv("")
  
}

#External
write.csv(scpdata, "Comparing LLTime with days under SCP_external - best case.csv", row.names = FALSE)

external <- scpdata

# 3C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_3Cwarmer - best case.csv", row.names = FALSE)

H3 <- scpdata

# 5C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_5Cwarmer - best case.csv", row.names = FALSE)

H5 <- scpdata


#### Long form data used in summarizing
external.summary <- Summarize(SCP_Bean.Leaf_Mean_External ~ LLT_Bean.Leaf_Mean_External, data = external)
external.summary

H3.summary <- Summarize(SCP_Bean.Leaf_Mean_3Cwarmer ~ LLT_Bean.Leaf_Mean_3Cwarmer, data = H3)
H3.summary

H5.summary <- Summarize(SCP_Bean.Leaf_Mean_5Cwarmer ~ LLT_Bean.Leaf_Mean_5Cwarmer, data = H5)
H5.summary


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
