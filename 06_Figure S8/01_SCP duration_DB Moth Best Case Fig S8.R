<<<<<<< HEAD
###########################################################################
###########    Figures for SCP Sensitivity in Diamondback Moth     ########
###########   Number of days of below SCP temps for each scenario  ########
###########          and the change between scenarios              ########
###########################################################################

# Author: Kimberly Thompson

# This script creates the map panels used in Figure S8 depicting the 
# best-case scenario for days under the SCP of diamondback moths.

# This done for the SCP days based on the mean, median, min and max 
# predicted values of ground temperatures
# however only the mean is used in the analysis and in Figure S8.

# Jpgs produced:
# SCP_Diamondback_Mean_External_Best.jpg
# SCP_Diamondback_Mean_3C_warmer_Best.jpg
# SCP_Diamondback_Mean_5C_warmer_Best.jpg

# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(RColorBrewer)
library(maptools)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(viridis)


#############################
###  Raster Loading    ######
#############################

# I have four summary measures # of occurrences of below SCP temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Diamondback Moth")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Diamondback Moth")

dur0_mean <- raster :: raster("SCP_Diamondback Moth_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Diamondback Moth_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Diamondback Moth_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Diamondback Moth_Median_External.tif")
dur3_median <- raster :: raster("SCP_Diamondback Moth_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Diamondback Moth_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Diamondback Moth_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Diamondback Moth_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Diamondback Moth_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Diamondback Moth_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Diamondback Moth_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Diamondback Moth_Maximum_5C warmer.tif")


#########################################################################
#      make adjustments to each raster to get them ready for plotting   #
#########################################################################

# Make a stack of the rasters
rasterlist <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# For plotting transform each raster into degrees so that the axes will look
# better and be clearer to understand
wgs84 = sp:: CRS("+init=epsg:4326")
# To keep the same values of the cells, the method of interpolation has to be nearest neighbor
rasterlist <- raster :: projectRaster(from=rasterlist, crs =  wgs84)



################################
###  Shapefile Loading    ######
################################

# Load in the states shapefile and make sure everything matches up
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )

# Convert projection so that it does match:
states <- spTransform( states, proj4string( rasterlist ) )


# Read in the latitude raster so that I can use it to crop the predictive map
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Latitude")
latitude.deg <- raster :: raster("Latitude_degrees.tif") 

# Make sure it matches the spatial projection of the degrees predictive rasters
latitude.deg <- projectRaster(from=latitude.deg, crs =  wgs84)

# Load in great lakes to add to plot
# Load in the states shapefile and make sure everything matches up
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
lakes<-rgdal::readOGR("Great_Lakes.shp" )

# Convert projection so that it does match:
lakes <- spTransform( lakes, proj4string( rasterlist ) )


################################
###   Cropping and Masking  ####
################################

newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the predictive raster to the states extent and then mask it so only states show
rasterlist <- crop(rasterlist, extent(newstates))
rasterlist <- raster :: mask(rasterlist, newstates)


##############################################################################################
#######                          Making SCP occurrence graphs                      ###########
##############################################################################################

# Define the color ramp
colr2 <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
# colr <- colorRampPalette(brewer.pal(9, 'YlGnBu'))

# Define where to print the labels on the color ramp legend
# my.at <- seq(0, 121, by=20)
my.at <- seq(0, 90, by=10)

### How to figure out the len value for the argument below in the levelplot code: "at=seq(0, 121, len=121)"
### Write out the breaks where the labels will be printed on the legend
# For example, 0 to 121 by 10 = 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120
# Count how many numbers are written (13), subtract 1 (12), multiply by 10 (120), add 1 (121) 
# Keeping it by 10 will allow a better color interpolation, above line of code though that says by 20 will keep
# a clearer legend interval


# Loop to make figures of the mean, median, min, and max of each scenario (4x3 = 12 total figures)
###### I can not get the loop to work!!! I had to go i by i to create the graphs - no googling helped.
path <- "01_Analysis/Figure S8/"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Diamondback Moths"
setwd(path)

for (i in nlayers(rasterlist)) {
  
  file.name <- paste(names(rasterlist)[[i]], "_Best.jpg", sep="")
  
  jpeg(file.name, width=7, height=5, units= 'in', res=600)
  
  print(levelplot(rasterlist[[i]],
                      layout=c(1,1),
                      margin=FALSE,                                       # suppress marginal graphics
                      xlab = list(label="Longitude", cex=1.5),
                      ylab = list(label="Latitude", cex=1.5),
                      colorkey=list(
                        space='right',                                    # plot legend at bottom
                        labels=list(at=my.at, font=4),                    # legend ticks and labels
                        axis.line=list(col='black')
                      ),
                      par.settings=list(
                        axis.line=list(col='transparent'),                # suppress axes and legend outline
                        strip.background = list(col='transparent'),         # get rid of color in the strips
                        strip.border = list(col='transparent')),            # get rid of border around the strips
                      names.attr = c(rep("", 1)),                         # get rid of strip text (if I wanted to keep it I would just change the labels in this line)
                      scales=list(draw=TRUE, cex=1.25, col="black"),      # include x and y axis labels, col argument specifies to draw the tick marks
                      col.regions=colr2,                                   # colour ramp
                      at=seq(0, 90, len=91)) +                            # colour ramp breaks
    latticeExtra::layer(sp.polygons(states, lwd=2)) +           # add states and lakes with latticeExtra::layer
    latticeExtra::layer(sp.polygons(lakes, fill='cadetblue')))
  
  dev.off()
  
  print(i)
  
}


=======
###########################################################################
###########    Figures for SCP Sensitivity in Diamondback Moth     ########
###########   Number of days of below SCP temps for each scenario  ########
###########          and the change between scenarios              ########
###########################################################################

# Author: Kimberly Thompson

# This script creates the map panels used in Figure S8 depicting the 
# best-case scenario for days under the SCP of diamondback moths.

# This done for the SCP days based on the mean, median, min and max 
# predicted values of ground temperatures
# however only the mean is used in the analysis and in Figure S8.

# Jpgs produced:
# SCP_Diamondback_Mean_External_Best.jpg
# SCP_Diamondback_Mean_3C_warmer_Best.jpg
# SCP_Diamondback_Mean_5C_warmer_Best.jpg

# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(RColorBrewer)
library(maptools)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(viridis)


#############################
###  Raster Loading    ######
#############################

# I have four summary measures # of occurrences of below SCP temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Diamondback Moth")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Diamondback Moth")

dur0_mean <- raster :: raster("SCP_Diamondback Moth_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Diamondback Moth_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Diamondback Moth_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Diamondback Moth_Median_External.tif")
dur3_median <- raster :: raster("SCP_Diamondback Moth_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Diamondback Moth_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Diamondback Moth_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Diamondback Moth_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Diamondback Moth_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Diamondback Moth_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Diamondback Moth_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Diamondback Moth_Maximum_5C warmer.tif")


#########################################################################
#      make adjustments to each raster to get them ready for plotting   #
#########################################################################

# Make a stack of the rasters
rasterlist <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# For plotting transform each raster into degrees so that the axes will look
# better and be clearer to understand
wgs84 = sp:: CRS("+init=epsg:4326")
# To keep the same values of the cells, the method of interpolation has to be nearest neighbor
rasterlist <- raster :: projectRaster(from=rasterlist, crs =  wgs84)



################################
###  Shapefile Loading    ######
################################

# Load in the states shapefile and make sure everything matches up
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )

# Convert projection so that it does match:
states <- spTransform( states, proj4string( rasterlist ) )


# Read in the latitude raster so that I can use it to crop the predictive map
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Latitude")
latitude.deg <- raster :: raster("Latitude_degrees.tif") 

# Make sure it matches the spatial projection of the degrees predictive rasters
latitude.deg <- projectRaster(from=latitude.deg, crs =  wgs84)

# Load in great lakes to add to plot
# Load in the states shapefile and make sure everything matches up
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
lakes<-rgdal::readOGR("Great_Lakes.shp" )

# Convert projection so that it does match:
lakes <- spTransform( lakes, proj4string( rasterlist ) )


################################
###   Cropping and Masking  ####
################################

newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the predictive raster to the states extent and then mask it so only states show
rasterlist <- crop(rasterlist, extent(newstates))
rasterlist <- raster :: mask(rasterlist, newstates)


##############################################################################################
#######                          Making SCP occurrence graphs                      ###########
##############################################################################################

# Define the color ramp
colr2 <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
# colr <- colorRampPalette(brewer.pal(9, 'YlGnBu'))

# Define where to print the labels on the color ramp legend
# my.at <- seq(0, 121, by=20)
my.at <- seq(0, 90, by=10)

### How to figure out the len value for the argument below in the levelplot code: "at=seq(0, 121, len=121)"
### Write out the breaks where the labels will be printed on the legend
# For example, 0 to 121 by 10 = 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120
# Count how many numbers are written (13), subtract 1 (12), multiply by 10 (120), add 1 (121) 
# Keeping it by 10 will allow a better color interpolation, above line of code though that says by 20 will keep
# a clearer legend interval


# Loop to make figures of the mean, median, min, and max of each scenario (4x3 = 12 total figures)
###### I can not get the loop to work!!! I had to go i by i to create the graphs - no googling helped.
path <- "01_Analysis/Figure S8/"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Diamondback Moths"
setwd(path)

for (i in nlayers(rasterlist)) {
  
  file.name <- paste(names(rasterlist)[[i]], "_Best.jpg", sep="")
  
  jpeg(file.name, width=7, height=5, units= 'in', res=600)
  
  print(levelplot(rasterlist[[i]],
                      layout=c(1,1),
                      margin=FALSE,                                       # suppress marginal graphics
                      xlab = list(label="Longitude", cex=1.5),
                      ylab = list(label="Latitude", cex=1.5),
                      colorkey=list(
                        space='right',                                    # plot legend at bottom
                        labels=list(at=my.at, font=4),                    # legend ticks and labels
                        axis.line=list(col='black')
                      ),
                      par.settings=list(
                        axis.line=list(col='transparent'),                # suppress axes and legend outline
                        strip.background = list(col='transparent'),         # get rid of color in the strips
                        strip.border = list(col='transparent')),            # get rid of border around the strips
                      names.attr = c(rep("", 1)),                         # get rid of strip text (if I wanted to keep it I would just change the labels in this line)
                      scales=list(draw=TRUE, cex=1.25, col="black"),      # include x and y axis labels, col argument specifies to draw the tick marks
                      col.regions=colr2,                                   # colour ramp
                      at=seq(0, 90, len=91)) +                            # colour ramp breaks
    latticeExtra::layer(sp.polygons(states, lwd=2)) +           # add states and lakes with latticeExtra::layer
    latticeExtra::layer(sp.polygons(lakes, fill='cadetblue')))
  
  dev.off()
  
  print(i)
  
}


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
