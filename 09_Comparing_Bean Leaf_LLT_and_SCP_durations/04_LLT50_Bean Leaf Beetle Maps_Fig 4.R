<<<<<<< HEAD
###########################################################################
###########   Figures for LLT50 Sensitivity in Bean Leaf Beetle    ########
###########   Longest consecutive stretch of days at or below 0C   ########
###########          and the change between scenarios              ########
###########            based on the LLT50 of Bean Leaf Beetle      ########
###########################################################################

# Author: Kimberly Thompson

# This script creates figure panels from the rasters that show the longest
# consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Based on worst-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.

# Figures produced:
# Figure 4



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

# I have four summary measures # of occurrences of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

#### I initially made these maps to apply to Bombus, but they are not specific to Bombus yet since all they show are the longest
#### consecutive days during the winter season that the temperature was at or below 0C. So I can use these same ones for Bean Leaf and
#### just change the day threshold further down in the code.

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
#######                          Making LLT50  graphs                              ###########
##############################################################################################

# I extracted this data from Lam and Pedigo 2000, using xyscan. I then created quadratic models for both the data points
# and the reported standard errors from the figure in the paper, resulting in a value, as well as an upper and lower 
# confidence interval for the number of days it takes to reach 50% mortality.

# These values are: [1] 28.87259 34.61265 41.32808

# Below the LLT50 is good, within the less good, above the threshold is bad
# Use the brewer pal to extract the hex codes to correspond with these above catgories

brewer.pal(9, 'Greens')
brewer.pal(9, 'YlOrRd')

breakpoints <- c(0, 28.87259, 41.32808, 121)

colr2 <- c("#74C476", "#FED976", "#800026")
# colr2 <- c("#74C476", "#FED976", "#FEB24C", "#FD8D3C", "#800026")

my.at <- c(0, 28.9, 34.6, 41.3, 121)

# Change the name to Bean leaf so it saves correctly
# names(rasterlist) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist))

# Loop to make figures of the mean, median, min, and max of each scenario (4x3 = 12 total figures)
###### I can not get the loop to work!!! I had to go i by i to create the graphs - no googling helped.
path <- "01_Analysis/Figure 4"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Bean Leaf Beetle"
setwd(path)

for (i in nlayers(rasterlist)) {
  
  file.name <- paste(names(rasterlist)[[i]], ".jpg", sep="")
  
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
                  at=c(0, 28.87259, 41.32808, 121)) +                            # colour ramp breaks
          latticeExtra::layer(sp.polygons(states, lwd=2)) +           # add states and lakes with latticeExtra::layer
          latticeExtra::layer(sp.polygons(lakes, fill='cadetblue')))
  
  dev.off()
  
  print(i)
  
}



=======
###########################################################################
###########   Figures for LLT50 Sensitivity in Bean Leaf Beetle    ########
###########   Longest consecutive stretch of days at or below 0C   ########
###########          and the change between scenarios              ########
###########            based on the LLT50 of Bean Leaf Beetle      ########
###########################################################################

# Author: Kimberly Thompson

# This script creates figure panels from the rasters that show the longest
# consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Based on worst-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.

# Figures produced:
# Figure 4



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

# I have four summary measures # of occurrences of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

#### I initially made these maps to apply to Bombus, but they are not specific to Bombus yet since all they show are the longest
#### consecutive days during the winter season that the temperature was at or below 0C. So I can use these same ones for Bean Leaf and
#### just change the day threshold further down in the code.

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
#######                          Making LLT50  graphs                              ###########
##############################################################################################

# I extracted this data from Lam and Pedigo 2000, using xyscan. I then created quadratic models for both the data points
# and the reported standard errors from the figure in the paper, resulting in a value, as well as an upper and lower 
# confidence interval for the number of days it takes to reach 50% mortality.

# These values are: [1] 28.87259 34.61265 41.32808

# Below the LLT50 is good, within the less good, above the threshold is bad
# Use the brewer pal to extract the hex codes to correspond with these above catgories

brewer.pal(9, 'Greens')
brewer.pal(9, 'YlOrRd')

breakpoints <- c(0, 28.87259, 41.32808, 121)

colr2 <- c("#74C476", "#FED976", "#800026")
# colr2 <- c("#74C476", "#FED976", "#FEB24C", "#FD8D3C", "#800026")

my.at <- c(0, 28.9, 34.6, 41.3, 121)

# Change the name to Bean leaf so it saves correctly
# names(rasterlist) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist))

# Loop to make figures of the mean, median, min, and max of each scenario (4x3 = 12 total figures)
###### I can not get the loop to work!!! I had to go i by i to create the graphs - no googling helped.
path <- "01_Analysis/Figure 4"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Bean Leaf Beetle"
setwd(path)

for (i in nlayers(rasterlist)) {
  
  file.name <- paste(names(rasterlist)[[i]], ".jpg", sep="")
  
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
                  at=c(0, 28.87259, 41.32808, 121)) +                            # colour ramp breaks
          latticeExtra::layer(sp.polygons(states, lwd=2)) +           # add states and lakes with latticeExtra::layer
          latticeExtra::layer(sp.polygons(lakes, fill='cadetblue')))
  
  dev.off()
  
  print(i)
  
}



>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
