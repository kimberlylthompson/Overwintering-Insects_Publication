#################################################################################
######         Aggregating landcover classes and cropping to          ###########
######                     Great Lakes study area                     ###########
#################################################################################

# Author: Kimberly Thompson

# This code reclassifies land cover into 6
# classes and crops to the Great Lakes Region

# Done for both USA and Canada

# USA: data from USGS (Multi-resolution Land characteristics Consortium)
# downloaded for the year 2016
# from https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover

# Canada: 

# Produces dataset:
#'NLCD_Great Lakes Area_reclassified.tif'


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
region <- sf :: st_read("Great_Lakes_States and Prov.shp")

# Load the USA 2016 land cover file
# (Not stored in 00_Data of Github repo)
nlcd <- terra :: rast("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/NLCD 2016/Annual_NLCD_LndCov_2016_CU_C1V0.tif")



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
nlcd_crop <- terra :: crop(nlcd, extent(region))

plot(region)
plot(nlcd_crop, add=TRUE)

# Collapse some of the landcover classes (using the big NLCD raster)
# Generate landcover number to name conversions:
num.codes <- unique( unlist( NLCD ) ) # This step takes a while
num.codes2 <- unique( unlist( NLCD_GL ) ) # Run this too just to see which classes are present in the cropped
cover.names <- NLCD_GL@data@attributes[[1]]$Land.Cover.Class[ num.codes + 1 ]
levels( cover.names )[1] <- NA # first level is ""
conversions <- data.frame( num.codes, cover.names )
conversions <- na.omit( conversions )
conversions <- conversions[ order( conversions$num.codes ), ]

# Reclassify NLCD_GL into 6 classes:
# "1", Deciduous: Deciduous forest (41), Mixed forest (43), Woody wetlands (90)
# "2", Conifer: Evergreen forest (42), Scrub/shrub (52)
# "3", Open: Developed, Open Space (21), Barren Land (31), Grassland/Herbaceous (71), Pasture/Hay (81),
#             Cultivated Crops (82), Emergent Herbaceous Wetlands (95)
# "4", Urban: Developed, Low Intensity (22), Developed, Medium Intensity (23), Developed High Intensity (24)
# "5", Water: Open Water (11)
# "6", Unclassified: Unclassified (0)







################################################################
###### Aggregating landcover classes and cropping to ###########
######      Great Lakes States study area            ###########
###############################################################

# Author: Kimberly Thompson

# This code reclassifies the 2014 National Landcover Database into 6
# classes and crops to the Great Lakes Region

# Prior to executing this code, download the National Landcover 
# Database from https://www.usgs.gov/centers/eros/science/national-land-cover-database

# Produces dataset:
#'NLCD_Great Lakes Area_reclassified.tif'


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory


####### load relevant packages ###
library( dplyr ) #manipulates dataframes
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( tidyr ) #spread and other dataframe functions
library( ggplot2 ) #fancy plots
library( lubridate ) #easy date adjustments and calculations.
library( rgdal ) #imports projects transforms spatial data. Requires sp.
library( rgeos ) #spatial manipulations e.g. buffer, interpolate.
library( maptools ) #reads and manipulates ESRI shapefiles.
library( raster ) #manipulates rasters
library( rasterVis ) #visualises rasters

# Set working directory to folder where NLCD was downloaded to
# setwd("L:/LabMemberFolders/KimberlyThompson/Species Distribution Modeling/SDM class 2018 subnivium/data/nlcd_2001_landcover_2011ed/nlcd_2001_landcover_2011_edition_2014_10_10")
workdir <- getwd() 

######### Manipulating landcover data ##############


# I downloaded data directly from the website and extracted them into
# L:\LabMemberFolders\KimberlyThompson\Species Distribution Modeling\SDM class 2018 subnivium\data\nlcd_2001_landcover_2011ed\nlcd_2001_landcover_2011_edition_2014_10_10.
# We are interested in landcover covering the Upper Midwest/Great Lakes States
# (MN, IA, WI, IL, IN, MI, OH, PA, and NY). So we import the complete file #
# first and then extract landcover values for the states. We do this by #
# adapting code from http://mbjoseph.github.io/2014/11/08/nlcd.html #

# Define file location and name:
filename <- paste( workdir, #this is our working directory
                   "/",
                   "nlcd_2001_landcover_2011_edition_2014_10_10.img", #this is the actual file name
                   sep="" ) #this pastes those 3 components together with no spaces

# Import NLCD (landcover) data file using the raster package:
NLCD <- raster::raster( filename )
# Plot:
plot( NLCD )

# Check the projection:
proj4string( NLCD )


# Load in the states shapefile to crop landcover to that extent
#Import shapefile of these states

setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )

# View file details:
summary( states )
# Quick plot:
plot( states )
# Check if projection matches site location data:
proj4string( states )
# Convert projection so that it does match:
states <- spTransform( states, proj4string( NLCD ) )

# Crop the nation-wide raster to the states extent
# This step will take a while and generate a warning message about an uneven number of columns and 
# rows in the matrix, but everything is fine.
NLCD_GL <- raster::crop(NLCD, extent(states))

plot(NLCD_GL)
plot(states, add=TRUE)


# Collapse some of the landcover classes (using the big NLCD raster)
# Generate landcover number to name conversions:
num.codes <- unique( unlist( NLCD ) ) # This step takes a while
num.codes2 <- unique( unlist( NLCD_GL ) ) # Run this too just to see which classes are present in the cropped
cover.names <- NLCD_GL@data@attributes[[1]]$Land.Cover.Class[ num.codes + 1 ]
levels( cover.names )[1] <- NA # first level is ""
conversions <- data.frame( num.codes, cover.names )
conversions <- na.omit( conversions )
conversions <- conversions[ order( conversions$num.codes ), ]

# Reclassify NLCD_GL into 6 classes:
# "1", Deciduous: Deciduous forest (41), Mixed forest (43), Woody wetlands (90)
# "2", Conifer: Evergreen forest (42), Scrub/shrub (52)
# "3", Open: Developed, Open Space (21), Barren Land (31), Grassland/Herbaceous (71), Pasture/Hay (81),
#             Cultivated Crops (82), Emergent Herbaceous Wetlands (95)
# "4", Urban: Developed, Low Intensity (22), Developed, Medium Intensity (23), Developed High Intensity (24)
# "5", Water: Open Water (11)
# "6", Unclassified: Unclassified (0)

# Per Megan: I just treated mixed forest as deciduous. I was originally intending to re-run the simulation 
# with it as coniferous and show the difference. I decided not to bother because we didn't end up addressing 
# land cover differences as a main point of the paper, the paper was already too long with all the other 
# sensitivity analyses, and mixed forest was less than 10% of my locations.

# Make a reclassification matrix
# from - each existing class, to - what it should turn into
rc <- as.matrix(data.frame(from=c(41,43,90,42,52,21,31,71,81,82,95,22,23,24,11,0),
                               to=c(1,1,1,2,2,3,3,3,3,3,3,4,4,4,5,6)))

# Reclassify (takes about 5 minutes)
NLCD_GL.rc <- raster :: reclassify(NLCD_GL, rc)
# NLCD_GL_proj.rc <- raster :: reclassify(NLCD_GL_proj, rc)

# Specify colors so that plot looks alright
breakpoints <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
colors <- c("darkgoldenrod3", "darkgreen", "khaki2", "deeppink", "blue", "black")

# Plot reclassfied raster
plot(NLCD_GL.rc, breaks=breakpoints, col=colors)
# plot(NLCD_GL_proj.rc, breaks=breakpoints, col=colors)
plot(states, add=TRUE)

# This below block did not work - so I saved it in its original CRS and will adjust later
# # Convert both NLCD_GL and states to be WGS 84
# # Project the raster 
# NLCD_GL_proj.rc <- projectRaster(from=NLCD_GL.rc, crs =  "+init=epsg:4326")
# # Convert the states projection so that it matches new raster projection:
# states <- spTransform( states, proj4string( NLCD_GL_proj ) )
# 
# plot(NLCD_GL_proj)
# plot(states, add=TRUE)

#Write both the NLCD_GL.rc and the NLCD_GL_proj.rc rasters
setwd("01_Analysis/Spatial Predictors")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Land Cover")

writeRaster(NLCD_GL.rc, "NLCD_Great Lakes Area_reclassified.tif")
# writeRaster(NLCD_GL_proj.rc, "NLCD_Great Lakes Area_reclassified_WGS84.tif")





