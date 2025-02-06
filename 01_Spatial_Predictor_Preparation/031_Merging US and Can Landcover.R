#################################################################################
######         Combining USA and Canada land Cover in the             ###########
######                     Great Lakes study area                     ###########
#################################################################################

# Author: Kimberly Thompson

# This code combines US and Canda land cover maps, which were both reclassified
# to have the same cover classes.

# clean workspace and load required packages #
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(terra)
library(sf)
library(tidyverse)

###################################
###                             ###
###       Data Loading          ###
###                             ###
###################################

# Load the states and provinces file
setwd("00_Data")
# region <- sf :: st_read("~/share/groups/mas/04_personal/Kim_T/Data/Great_Lakes_States and Prov.shp")
region <- sf :: st_read("Great_Lakes_States and Prov.shp")

# Load the Canadian data
# (Not stored in 00_Data of Github repo)
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Spatial Predictors/Individual Land Cover/"
can <- terra :: rast(paste(path, "Canada Great Lakes 2016 Landcover.tif", sep = ""))

# Load the USA data
usa <- terra :: rast(paste(path, "US Great Lakes 2016 Landcover.tif", sep = ""))



###################################
###                             ###
###       Data Merging          ###
###                             ###
###################################

# Project Canadian raster to be same projection as USA
can <- terra :: project(can, usa)

# Merge the rasters
merged <- terra :: merge(usa, can, first = TRUE, na.rm = TRUE)

merged

# Create a new factor level for the combined raster
new_levels <- c("Deciduous", "Conifer", "Open", "Urban", "Water", "Snow Ice")
merged <- categories(merged, value=data.frame(ID=1:6, landcover=new_levels))


# Check that everything looks ok
region2 <- sf :: st_transform(region, crs(merged))

plot(merged)
plot(region2$geometry, add = TRUE)

# Define a custom color palette
custom_colors <- c("Deciduous" = "#90EE90", # Light green
                   "Conifer" = "#006400",   # Dark green
                   "Open" = "#FFFF00",      # Yellow
                   "Urban" = "#8B4513",     # Brown
                   "Water" = "#0000FF",     # Blue
                   "Snow Ice" = "#FFFFFF")  # White

# Plot the raster with custom colors
plot(merged, col = custom_colors)
plot(region2$geometry, add = TRUE)


# Write the land cover layer
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/"
# path <- getwd()
writeRaster(merged, paste(path, "USA CAN Landcover.tif", sep = ""))
