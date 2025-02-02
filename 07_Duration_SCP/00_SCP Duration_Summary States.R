<<<<<<< HEAD
#########################################################################
###########         Comparing the distributions of       ################
###########              number of days below            ################
###########       supercooling point for each species    ################
###########           in each warming scenario           ################
#########################################################################

# Author: Kimberly Thompson

# This code summarizes the number of days in the winter season under the
# supercooling point for each insect by the mean, median, min, and max.

# Datasets produced:
# 'Best Case SCP Summary.csv"
# "Worst Case SCP Summary.csv"


# Tables produced from the datasets:
# Table S6
# Table S7


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(tidyr)
library(car)
library(ggplot2)
library(FSA)
library(rcompanion)
library(PMCMRplus)


# Loop to find the mean, median and standard deviation, and create a distribution plot for the best and worst case scenarios
# for each of the 7 bugs.

# Even though I have results for the mean, median, min, and max of the 50 bootstraps with the best and worst case scenario SCP for 
# each bug applied to that, I am making this dataframe and these plots for the mean only.




# Make a list of the bugs according to how they're named in the folders
# bug.list <- c("Bumblebees", "Diamondback Moth", "Green Leaf Beetle", "Hoverfly", "Papilio canadensis", "Papilio glaucus", "Wooly Bear")
bug.list <- c("Bumblebees", "Diamondback Moth", "Papilio canadensis", "Papilio glaucus", "Woolly Bear",
              "Bean Leaf", "Hoverfly")

# Make a list of the bugs according to how they're named in the files
# species.list <- c("Bombus", "Diamondback Moth", "Green Leaf Beetle", "Hoverfly", "P.canadensis", "P.glaucus", "Wooly Bear")
species.list <- c("Bombus", "Diamondback", "P canadensis", "P glaucus", "WoollyBear",
              "Bean Leaf", "Hoverfly")


# To help with the raster processing
# Load in the states shapefile
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
setwd("00_Data")
# setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#################################################################
###########   Best Case Scenario    #############################
#################################################################


# Make a dataframe to store the summary statistics for the SCP distributions
best.case.summary <- data.frame(Treatment=character(), n=integer(), mean=numeric(), sd=numeric(), min=integer(),
                                Q1=numeric(), median=numeric(), Q3=numeric(), max=integer(), percZero=numeric())


# Make a path to serve as the basis for setting the working directories to read in each raster set
path <- "01_Analysis/Spatial Predictions/Species Vulnerability/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/"

for (i in 1:length(bug.list)) {
  
  # set the working directory
  setwd(paste(path, bug.list[i], sep = ""))
  
  dur0 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_External.tif", sep = ""))
  dur3 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_3C warmer.tif", sep = ""))
  dur5 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_5C warmer.tif", sep = ""))
  
  #### Make adjustments to calculate the summary stats for only the extent I'll be presenting in the figures
  
  # Stack the rasters
  rasterlist <- raster :: stack(dur0, dur3, dur5)
  
  # transform each raster into degrees so that the axes will look
  # better and be clearer to understand
  wgs84 = sp:: CRS("+init=epsg:4326")
  rasterlist <- projectRaster(from=rasterlist, crs =  wgs84)
  
  # Convert projection of states so that it matches rasters:
  states <- spTransform( states, proj4string( rasterlist ) )
  
  # Define the extent for the states
  newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))
  
  # Crop the rasters to match
  rasterlist <- crop(rasterlist, extent(newstates))
  rasterlist <- raster :: mask(rasterlist, newstates)
  
  ##### Create wide and long forms of the data from each raster - wide form used in plotting, long form used in summarizing
  
  # Convert each mean SCP raster to a dataframe
  H0 <- as.data.frame(rasterlist[[1]], xy=TRUE)
  H3 <- as.data.frame(rasterlist[[2]], xy=TRUE)
  H5 <- as.data.frame(rasterlist[[3]], xy=TRUE)
  
  # Add an ID column for each which will represent the unique lat long combo
  H0$ID <- seq(from = 1, to=length(H0$x), by=1)
  H3$ID <- seq(from = 1, to=length(H3$x), by=1)
  H5$ID <- seq(from = 1, to=length(H5$x), by=1)
  
  # Merge dataframes
  combined <- merge(H0, H3, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  combined <- merge(combined, H5, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  
  # Make sure that ID column is a factor
  combined$ID <- factor(combined$ID)
  
  # Reshape data into long form 
  # Info on gather function found at http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  combined2 <- gather(combined, Treatment, Duration, colnames(combined)[4]:colnames(combined)[6], factor_key=TRUE)
  
  # Make sure that ID column is a factor
  combined2$ID <- factor(combined2$ID)
  
  # Removed the rows in long form data frame where there are NA values
  combined2 <- combined2[complete.cases(combined2),]
  
  # Remove the rows in wide form data frame where there are NA values
  combined <- combined[complete.cases(combined),]
  
  # Save the long form data for plotting density plots
  # setwd("01_Analysis/Summary_Results")
  # # setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  # write.csv(combined2, paste("SubSCP Days_", bug.list[i], ".csv", sep = ""), row.names = FALSE)
  
  print(i)

}


# Write the best case summary csv
setwd("01_Analysis/Summary_Results")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
write.csv(best.case.summary, "Best Case SCP Summary.csv", row.names = FALSE)

#clean up workspace
rm(bug.summary, combined, combined2, dur0, dur3, dur5, duration.0, duration.3, duration.5, H0, H3, H5, rasterlist)


#################################################################
###########   Worst Case Scenario    #############################
#################################################################

# Make a dataframe to store the summary statistics for the SCP distributions
worst.case.summary <- data.frame(Treatment=character(), n=integer(), mean=numeric(), sd=numeric(), min=integer(),
                                Q1=numeric(), median=numeric(), Q3=numeric(), max=integer(), percZero=numeric())


# Make a path to serve as the basis for setting the working directories to read in each raster set
path <- "01_Analysis/Spatial Predictions/Species Vulnerability/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/"

for (i in 1:length(bug.list)) {
  
  # set the working directory
  setwd(paste(path, bug.list[i], "/Worst Case/", sep = ""))
  
  dur0 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_External.tif", sep = ""))
  dur3 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_3C warmer.tif", sep = ""))
  dur5 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_5C warmer.tif", sep = ""))
  
  #### Make adjustments to calculate the summary stats for only the extent I'll be presenting in the figures
  
  # Stack the rasters
  rasterlist <- raster :: stack(dur0, dur3, dur5)
  
  # transform each raster into degrees so that the axes will look
  # better and be clearer to understand
  wgs84 = sp:: CRS("+init=epsg:4326")
  rasterlist <- projectRaster(from=rasterlist, crs =  wgs84)
  
  # Convert projection of states so that it matches rasters:
  states <- spTransform( states, proj4string( rasterlist ) )
  
  # Define the extent for the states
  newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))
  
  # Crop the rasters to match
  rasterlist <- crop(rasterlist, extent(newstates))
  rasterlist <- raster :: mask(rasterlist, newstates)
  
  ##### Create wide and long forms of the data from each raster - wide form used in plotting, long form used in summarizing
  
  # Convert each mean SCP raster to a dataframe
  H0 <- as.data.frame(rasterlist[[1]], xy=TRUE)
  H3 <- as.data.frame(rasterlist[[2]], xy=TRUE)
  H5 <- as.data.frame(rasterlist[[3]], xy=TRUE)
  
  # Add an ID column for each which will represent the unique lat long combo
  H0$ID <- seq(from = 1, to=length(H0$x), by=1)
  H3$ID <- seq(from = 1, to=length(H3$x), by=1)
  H5$ID <- seq(from = 1, to=length(H5$x), by=1)
  
  # Merge dataframes
  combined <- merge(H0, H3, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  combined <- merge(combined, H5, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  
  # Make sure that ID column is a factor
  combined$ID <- factor(combined$ID)
  
  # Reshape data into long form 
  # Info on gather function found at http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  combined2 <- gather(combined, Treatment, Duration, colnames(combined)[4]:colnames(combined)[6], factor_key=TRUE)
  
  # Make sure that ID column is a factor
  combined2$ID <- factor(combined2$ID)
  
  # Removed the rows in long form data frame where there are NA values
  combined2 <- combined2[complete.cases(combined2),]
  
  # Remove the rows in wide form data frame where there are NA values
  combined <- combined[complete.cases(combined),]
  
  # # Save the long form data for plotting density plots
  # setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  # write.csv(combined2, paste("Worstcase_SubSCP Days_", bug.list[i], ".csv", sep = ""), row.names = FALSE)
  
  print(i)
  
}


# Write the worst case summary csv
setwd("01_Analysis/Summary_Results")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
write.csv(worst.case.summary, "Worst Case SCP Summary.csv", row.names = FALSE)



=======
#########################################################################
###########         Comparing the distributions of       ################
###########              number of days below            ################
###########       supercooling point for each species    ################
###########           in each warming scenario           ################
#########################################################################

# Author: Kimberly Thompson

# This code summarizes the number of days in the winter season under the
# supercooling point for each insect by the mean, median, min, and max.

# Datasets produced:
# 'Best Case SCP Summary.csv"
# "Worst Case SCP Summary.csv"


# Tables produced from the datasets:
# Table S6
# Table S7


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(tidyr)
library(car)
library(ggplot2)
library(FSA)
library(rcompanion)
library(PMCMRplus)


# Loop to find the mean, median and standard deviation, and create a distribution plot for the best and worst case scenarios
# for each of the 7 bugs.

# Even though I have results for the mean, median, min, and max of the 50 bootstraps with the best and worst case scenario SCP for 
# each bug applied to that, I am making this dataframe and these plots for the mean only.




# Make a list of the bugs according to how they're named in the folders
# bug.list <- c("Bumblebees", "Diamondback Moth", "Green Leaf Beetle", "Hoverfly", "Papilio canadensis", "Papilio glaucus", "Wooly Bear")
bug.list <- c("Bumblebees", "Diamondback Moth", "Papilio canadensis", "Papilio glaucus", "Woolly Bear",
              "Bean Leaf", "Hoverfly")

# Make a list of the bugs according to how they're named in the files
# species.list <- c("Bombus", "Diamondback Moth", "Green Leaf Beetle", "Hoverfly", "P.canadensis", "P.glaucus", "Wooly Bear")
species.list <- c("Bombus", "Diamondback", "P canadensis", "P glaucus", "WoollyBear",
              "Bean Leaf", "Hoverfly")


# To help with the raster processing
# Load in the states shapefile
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
setwd("00_Data")
# setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#################################################################
###########   Best Case Scenario    #############################
#################################################################


# Make a dataframe to store the summary statistics for the SCP distributions
best.case.summary <- data.frame(Treatment=character(), n=integer(), mean=numeric(), sd=numeric(), min=integer(),
                                Q1=numeric(), median=numeric(), Q3=numeric(), max=integer(), percZero=numeric())


# Make a path to serve as the basis for setting the working directories to read in each raster set
path <- "01_Analysis/Spatial Predictions/Species Vulnerability/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/"

for (i in 1:length(bug.list)) {
  
  # set the working directory
  setwd(paste(path, bug.list[i], sep = ""))
  
  dur0 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_External.tif", sep = ""))
  dur3 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_3C warmer.tif", sep = ""))
  dur5 <- raster :: raster(paste("SCP_", species.list[i], "_Mean_5C warmer.tif", sep = ""))
  
  #### Make adjustments to calculate the summary stats for only the extent I'll be presenting in the figures
  
  # Stack the rasters
  rasterlist <- raster :: stack(dur0, dur3, dur5)
  
  # transform each raster into degrees so that the axes will look
  # better and be clearer to understand
  wgs84 = sp:: CRS("+init=epsg:4326")
  rasterlist <- projectRaster(from=rasterlist, crs =  wgs84)
  
  # Convert projection of states so that it matches rasters:
  states <- spTransform( states, proj4string( rasterlist ) )
  
  # Define the extent for the states
  newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))
  
  # Crop the rasters to match
  rasterlist <- crop(rasterlist, extent(newstates))
  rasterlist <- raster :: mask(rasterlist, newstates)
  
  ##### Create wide and long forms of the data from each raster - wide form used in plotting, long form used in summarizing
  
  # Convert each mean SCP raster to a dataframe
  H0 <- as.data.frame(rasterlist[[1]], xy=TRUE)
  H3 <- as.data.frame(rasterlist[[2]], xy=TRUE)
  H5 <- as.data.frame(rasterlist[[3]], xy=TRUE)
  
  # Add an ID column for each which will represent the unique lat long combo
  H0$ID <- seq(from = 1, to=length(H0$x), by=1)
  H3$ID <- seq(from = 1, to=length(H3$x), by=1)
  H5$ID <- seq(from = 1, to=length(H5$x), by=1)
  
  # Merge dataframes
  combined <- merge(H0, H3, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  combined <- merge(combined, H5, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  
  # Make sure that ID column is a factor
  combined$ID <- factor(combined$ID)
  
  # Reshape data into long form 
  # Info on gather function found at http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  combined2 <- gather(combined, Treatment, Duration, colnames(combined)[4]:colnames(combined)[6], factor_key=TRUE)
  
  # Make sure that ID column is a factor
  combined2$ID <- factor(combined2$ID)
  
  # Removed the rows in long form data frame where there are NA values
  combined2 <- combined2[complete.cases(combined2),]
  
  # Remove the rows in wide form data frame where there are NA values
  combined <- combined[complete.cases(combined),]
  
  # Save the long form data for plotting density plots
  # setwd("01_Analysis/Summary_Results")
  # # setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  # write.csv(combined2, paste("SubSCP Days_", bug.list[i], ".csv", sep = ""), row.names = FALSE)
  
  print(i)

}


# Write the best case summary csv
setwd("01_Analysis/Summary_Results")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
write.csv(best.case.summary, "Best Case SCP Summary.csv", row.names = FALSE)

#clean up workspace
rm(bug.summary, combined, combined2, dur0, dur3, dur5, duration.0, duration.3, duration.5, H0, H3, H5, rasterlist)


#################################################################
###########   Worst Case Scenario    #############################
#################################################################

# Make a dataframe to store the summary statistics for the SCP distributions
worst.case.summary <- data.frame(Treatment=character(), n=integer(), mean=numeric(), sd=numeric(), min=integer(),
                                Q1=numeric(), median=numeric(), Q3=numeric(), max=integer(), percZero=numeric())


# Make a path to serve as the basis for setting the working directories to read in each raster set
path <- "01_Analysis/Spatial Predictions/Species Vulnerability/"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/"

for (i in 1:length(bug.list)) {
  
  # set the working directory
  setwd(paste(path, bug.list[i], "/Worst Case/", sep = ""))
  
  dur0 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_External.tif", sep = ""))
  dur3 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_3C warmer.tif", sep = ""))
  dur5 <- raster :: raster(paste("SCP_", species.list[i], "_Worst Case_Mean_5C warmer.tif", sep = ""))
  
  #### Make adjustments to calculate the summary stats for only the extent I'll be presenting in the figures
  
  # Stack the rasters
  rasterlist <- raster :: stack(dur0, dur3, dur5)
  
  # transform each raster into degrees so that the axes will look
  # better and be clearer to understand
  wgs84 = sp:: CRS("+init=epsg:4326")
  rasterlist <- projectRaster(from=rasterlist, crs =  wgs84)
  
  # Convert projection of states so that it matches rasters:
  states <- spTransform( states, proj4string( rasterlist ) )
  
  # Define the extent for the states
  newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))
  
  # Crop the rasters to match
  rasterlist <- crop(rasterlist, extent(newstates))
  rasterlist <- raster :: mask(rasterlist, newstates)
  
  ##### Create wide and long forms of the data from each raster - wide form used in plotting, long form used in summarizing
  
  # Convert each mean SCP raster to a dataframe
  H0 <- as.data.frame(rasterlist[[1]], xy=TRUE)
  H3 <- as.data.frame(rasterlist[[2]], xy=TRUE)
  H5 <- as.data.frame(rasterlist[[3]], xy=TRUE)
  
  # Add an ID column for each which will represent the unique lat long combo
  H0$ID <- seq(from = 1, to=length(H0$x), by=1)
  H3$ID <- seq(from = 1, to=length(H3$x), by=1)
  H5$ID <- seq(from = 1, to=length(H5$x), by=1)
  
  # Merge dataframes
  combined <- merge(H0, H3, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  combined <- merge(combined, H5, by.x = c("x", "y", "ID"), by.y = c("x", "y", "ID"), all=TRUE )
  
  # Make sure that ID column is a factor
  combined$ID <- factor(combined$ID)
  
  # Reshape data into long form 
  # Info on gather function found at http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  combined2 <- gather(combined, Treatment, Duration, colnames(combined)[4]:colnames(combined)[6], factor_key=TRUE)
  
  # Make sure that ID column is a factor
  combined2$ID <- factor(combined2$ID)
  
  # Removed the rows in long form data frame where there are NA values
  combined2 <- combined2[complete.cases(combined2),]
  
  # Remove the rows in wide form data frame where there are NA values
  combined <- combined[complete.cases(combined),]
  
  # # Save the long form data for plotting density plots
  # setwd("L:/fwe/labs/Zuckerberg_Lab/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  # write.csv(combined2, paste("Worstcase_SubSCP Days_", bug.list[i], ".csv", sep = ""), row.names = FALSE)
  
  print(i)
  
}


# Write the worst case summary csv
setwd("01_Analysis/Summary_Results")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
write.csv(worst.case.summary, "Worst Case SCP Summary.csv", row.names = FALSE)



>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
