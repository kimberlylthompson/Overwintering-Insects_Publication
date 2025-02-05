#################################################################################
######         Aggregating landcover classes and cropping to          ###########
######                     Great Lakes study area                     ###########
#################################################################################

# Author: Kimberly Thompson

# This code reclassifies land cover into 6
# classes and crops to the Great Lakes Region

# Done for both USA and Canada - but this is for Canada

# USA: data from USGS (Multi-resolution Land characteristics Consortium)
# downloaded for the year 2016
# from https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover

# Canada: data from Canadian Governemnt - 2015 land cover
# https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6/resource/54200576-6203-4c1f-b646-924a1df3e08b

# Produces dataset:
#'US Great Lakes 2016 Landcover.tif'
#'Canada Great Lakes 2015 Landcover.tif'


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

# Load the Canada 2015 land cover file
# (Not stored in 00_Data of Github repo)
nlcd <- terra :: rast("H:/My Drive/Ch 4 Bumblebees/00_Data/Canadian_Data")
# nlcd <- terra :: rast("~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/Canadian land cover 2015/landcover-2015-classification.tif")


###################################
###                             ###
###     Data Manipulation       ###
###                             ###
###################################

###############
##  Canada   ##
###############

# Convert region so that it match nlcd:
region2 <- sf :: st_transform( region, crs(nlcd) )

# Crop the nation-wide raster to the states extent
nlcd_crop <- terra :: crop(nlcd, region2)

plot(region2$geometry)
plot(nlcd_crop, add=TRUE)

# Collapse some of the landcover classes 
# Generate landcover number to name conversions:
num.codes2 <- unique( unlist( nlcd_crop ) ) # Run this too just to see which classes are present in the cropped

# Add names for each category based on metadata
num.codes2$cover.names <- c("Temperate or sub-polar needleleaf forest",
                            "Sub-polar taiga needleleaf forest",
                            "Temperate or sub-polar broadleaf deciduous forest",
                            "Mixed forest", "Temperate or sub-polar shrubland",
                            "Temperate or sub-polar grassland",
                            "Sub-polar or polar shrubland-lichen-moss",
                            "Sub-polar or polar grassland-lichen-moss",
                            "Sub-polar or polar barren-lichen-moss",
                            "Wetland", "Cropland", "Barren lands",
                            "Urban", "Water", "Snow and Ice")


# Reclassify nlcd_crop into 6 classes:
# "1", Deciduous: Temperate or subpolar deciduous (3), Mixed forest (5)
# "2", Conifer: Temperate or subpolar needleleaf (1), sub-polar or taiga needleleaf (2),
#               Temperate or subpolar shrubland (6)
# "3", Open: Temperate or subpolar grassland(8), subpolar or polar shrubland lichen moss (10),
#           subpolar or polar grassland lichen moss(11), subpolar or polar barren lichen moss (12),
#            Wetland (13), cropland (14), Barren lands (15)
# "4", Urban: Urban (16)
# "5", Water: Water (17)
# "6", Snow and Ice (18)


# Make a reclassification matrix
# from - each existing class, to - what it should turn into
rc <- as.matrix(data.frame(from=c(3,5,1,2,6,8,10,11,12,13,14,15,16,17,18),
                           to=c(1,1,2,2,2,3,3,3,3,3,3,3,4,5,6)))

# Reclassify
nlcd_crop <- terra :: classify(nlcd_crop, rc, include.lowest = TRUE)


# Specify colors so that plot looks alright
breakpoints <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
breakpoints <- c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
colors <- c("darkgoldenrod3", "darkgreen", "khaki2", "deeppink", "blue", "black")

# Plot reclassfied raster
plot(nlcd_crop, breaks=breakpoints, col=colors)
plot(region2$geometry, add=TRUE)


# Make sure the raster is treated as categorical
nlcd_crop <- as.factor(nlcd_crop)

# Set the levels with category names
levels(nlcd_crop) <- data.frame(ID = 1:6, category = c("Deciduous", "Conifer", "Open", "Urban", "Open Water",
                                                       "Snow Ice"))

# Save the reclassified and cropped NLCD raster
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Spatial Predictors/Individual Land cover/"
# path <- getwd()
writeRaster(nlcd_crop, paste(path, "Canada Great Lakes 2016 Landcover.tif", sep = ""))