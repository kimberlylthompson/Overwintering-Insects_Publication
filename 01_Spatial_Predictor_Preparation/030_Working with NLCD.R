#################################################################################
######         Aggregating landcover classes and cropping to          ###########
######                     Great Lakes study area                     ###########
#################################################################################

# Author: Kimberly Thompson

# This code reclassifies land cover into 6
# classes and crops to the Great Lakes Region

# Done for both USA and Canada but this is for USA

# USA: data from USGS (Multi-resolution Land characteristics Consortium)
# downloaded for the year 2016
# from https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover

# Canada: 

# Produces dataset:
#'US Great Lakes 2016 Landcover.tif'


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

# Load the USA 2016 land cover file
# (Not stored in 00_Data of Github repo)
nlcd <- terra :: rast("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/NLCD 2016/Annual_NLCD_LndCov_2016_CU_C1V0.tif")
# nlcd <- terra :: rast("~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Raw/NLCD 2016/Annual_NLCD_LndCov_2016_CU_C1V0.tif")


###################################
###                             ###
###     Data Manipulation       ###
###                             ###
###################################

############
##  USA   ##
############

# Convert region so that it match nlcd:
region2 <- sf :: st_transform( region, crs(nlcd) )

# Crop the nation-wide raster to the states extent
nlcd_crop <- terra :: crop(nlcd, region2)

plot(region2$geometry)
plot(nlcd_crop, add=TRUE)

# Collapse some of the landcover classes (using the big NLCD raster)
# Generate landcover number to name conversions:
num.codes <- unique( unlist( nlcd ) ) # This step takes a while
num.codes2 <- unique( unlist( nlcd_crop ) ) # Run this too just to see which classes are present in the cropped

# Add names for each category based on metadata
num.codes2$cover.names <- c("Open Water", "Developed Open Space",
                            "Developed Low Intensity", "Developed Medium Intensity",
                            "Developed High Intensity", "Barren Land",
                            "Deciduous Forest", "Evergreen Forest",
                            "Mixed Forest", "Shrub Scrub", "Grassland Herbaceous",
                            "Pasture Hay", "Cultivated Crops", "Woody Wetlands",
                            "Emergent Herbaceous Wetlands")

# Add row for no data
num.codes2[16, ] <- c("250", "No Data")


# Reclassify nlcd_crop into 6 classes:
# "1", Deciduous: Deciduous forest (41), Mixed forest (43), Woody wetlands (90)
# "2", Conifer: Evergreen forest (42), Scrub/shrub (52)
# "3", Open: Developed, Open Space (21), Barren Land (31), Grassland/Herbaceous (71), Pasture/Hay (81),
#             Cultivated Crops (82), Emergent Herbaceous Wetlands (95)
# "4", Urban: Developed, Low Intensity (22), Developed, Medium Intensity (23), Developed High Intensity (24)
# "5", Water: Open Water (11)
# "6", Unclassified: Unclassified (250)


# Make a reclassification matrix
# from - each existing class, to - what it should turn into
rc <- as.matrix(data.frame(from=c(41,43,90,42,52,21,31,71,81,82,95,22,23,24,11,250),
                           to=c(1,1,1,2,2,3,3,3,3,3,3,4,4,4,5,6)))

# Reclassify
nlcd_crop <- terra :: classify(nlcd_crop, rc, include.lowest = TRUE)


# Specify colors so that plot looks alright
breakpoints <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
colors <- c("darkgoldenrod3", "darkgreen", "khaki2", "deeppink", "blue", "black")

# Plot reclassfied raster
plot(nlcd_crop, breaks=breakpoints, col=colors)
plot(region2$geometry, add=TRUE)


# Make the raster categorical
cat_df <- data.frame(
  id = 1:6,
  category = c("Deciduous", "Conifer", "Open", "Urban", "Open Water", "Unclassified")
)


# Set the levels of the raster
levels(nlcd_crop) <- cat_df

# Make sure the raster is treated as categorical
nlcd_crop <- as.factor(nlcd_crop)

# Save the reclassified and cropped NLCD raster
path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Spatial Predictors/Individual Land cover/"
path <- getwd()
writeRaster(nlcd_crop, paste(path, "US Great Lakes 2016 Landcover.tif", sep == ""))





