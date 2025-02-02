<<<<<<< HEAD
#################################################################################################
#########         Code for Extracting snow depths from SNODAS files              ################
########            Gotten from Megan with some small adjustments                ################
#################################################################################################

# Author: Kimberly Thompson

# This code uses snow depth data and snow water equivalent data to calculate snow density, 
# and creates raster stacks of snow depth and snow density with a layer for each day from
# Dec 1 2016 - March 31 2017.

# Prior to executing code, snow depth and snow water equivalent files (.bil) were downloaded from 
# https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools
# These data are freely available

# Creates raster (bricks):
# ' .snowdep.tif'
# ' .swe.tif'
# ' .snowdensity.tif'




########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(rgdal)
library(foreign)
library(stringr)
library(tidyr)


# change path to where downloaded files reside
# setwd("D:/Workspace_Megan/SNODAS/gzdatfiles")
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded")
allfiles<-list.files(pattern="*.bil")
allfiles<-as.data.frame(allfiles)
colnames(allfiles)<-"from"
allfiles$temp<-allfiles$from
allfiles<-separate(allfiles,temp,into=c("base","ending"),sep=".bil")
allfiles$ending<-NULL
allfiles$to<-paste0(allfiles$base,".bil")


#creating a column with types for later. 
allfiles$type<-NA
for(i in 1:length(allfiles$from)){
  test<-strsplit(allfiles$to[i],"tS__")
  if(test[[1]][1]=="us_ssmv11034"){allfiles$type[i]="swe"}else{allfiles$type[i]="depth"}
  
}

#Now going to do a loop to read the rasters into a stack

#initializing a couple of things
blueprint<-raster(nrows=3351,ncols=6935,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                  xmn=-124.733333333329,xmx=-66.9416666666642,ymn=24.9499999999990,ymx=52.8749999999978)
depth<-stack(blueprint)
swe<-stack(blueprint)
j<-1
k<-1

for(i in 1:length(allfiles$to)){
  #read in the raster
  temprast<-raster(allfiles$to[i]) 
  #create a blueprint with desired coordinate system and extent
  blueprint<-raster(nrows=3351,ncols=6935,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                    xmn=-124.733333333329,xmx=-66.9416666666642,ymn=24.9499999999990,ymx=52.8749999999978)
  #fill in the values of the blueprint raster with the values read in
  values(blueprint)<-values(temprast) #fill in the values
  blueprint[blueprint==-9999]<-NA
  #assign to the correct raster stack depending on whether this is depth or swe data
  if(allfiles$type[i]=="depth"){
    depth[[j]]<-blueprint
    j<-j+1
  }else{
    swe[[k]]<-blueprint
    k<-k+1
  }
  print(i)
}

#Now have snow depths in Units = meters*1000. Snapsot at 06:00 UTC. One layer per day. First layer is 11/1/16, last layer is 4/30/17. 

# Remove dataframes that aren't needed
rm(allfiles)
rm(blueprint)
rm(temprast)
rm(test)

# Change the names in the new depth rasterstack
date1<-as.Date("2016-11-01")
date2<-as.Date("2017-04-30")
dates<-seq.Date(from=date1,to=date2,by=1)
names(depth) <- dates
names(swe) <- dates

# I only need December 1 2016 - March 31 2017 so trim down the rasterstack
names(depth)
# 31 = Dec 1st, 151 = March 31st
depth <- subset(depth, 31:151)
swe <- subset(swe, 31:151)

# Change the projection
#Use the land cover projection and project each brick to the landcover projection
crslandcover <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# crslandcover <- crs(landcover)

# Project the snowdepth stack - This takes a while. ~10.13 hours
start_time <- Sys.time()
depth2 <- projectRaster(from=depth, crs =  crslandcover)
swe2 <- projectRaster(from=swe, crs =  crslandcover)
end_time <- Sys.time()

############################################################
###### Crop the raster stack to the states extent ##########
###########################################################

#Import shapefile of these states
setwd("00_Data")
# setwd("D:/Workspace_Kim/Chapter 2")
states<-rgdal::readOGR("Great_Lakes_States.shp" )

# View file details:
summary( states )
# Quick plot:
plot( states )
# Check if projection matches site location data:
proj4string( states )
# Convert projection so that it does match:
states <- spTransform( states, proj4string( swe2 ) )

# Crop - this will still give a rectangular extent beyond the state boundaries but this is preferable
# because otherwise some of the raster cells around the perimeter of the states are missing
depth2_winter <- crop(depth2, extent(states))
swe2_winter <- crop(swe2, extent(states))

# Check CRS, extent, and resolution of the bricks and put info in the master metadata file
# Found in L:\LabMemberFolders\KimberlyThompson\Ch 2 Climate\Analysis\Interpolated Predictor Data
proj4string(depth2_winter)
extent(depth2_winter)
res(depth2_winter)

plot(depth2_winter[[1]])
plot(states, add=TRUE)

proj4string(swe2_winter)
extent(swe2_winter)
res(swe2_winter)

plot(swe2_winter[[1]])
plot(states, add=TRUE)



############################################################
######   Raster math to get density brick        ##########
###########################################################

density<-swe2_winter/depth2_winter 



############################################################
###### Raster math to get units into centimeters ##########
###########################################################

# Centimeters = meters * 100
# If what I have right now is meters * 1000, then to get Meters* 100 (centimeters)
# I need to divide each cell by 10.
depth2_winter_cm <- depth2_winter/10

plot(depth2_winter_cm[[1]])
plot(states, add=TRUE)


density_cm <- density/10

swe2_winter_cm <- swe2_winter/10



#############################################################################
### Extract each layer from the raster bricks into their own raster stack ###
### Each stack will represent one day and eventually will contain all     ###
### variables in the Boosted Regression Tree Models (daily wind speed, mean ###
### snow depth, daily tmax, daily tmin, land cover)                        ###

#### Depth

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("D:/Workspace_Kim/Chapter 2/Final SNODAS files")

# For the depth brick
start_time <- Sys.time()
for (i in 1:length(names(depth2_winter_cm))) {
  stacks <- stack(depth2_winter_cm[[i]])
  writeRaster(stacks, paste(names(stacks), ".snowdep.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time

#### SWE

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/SNODAS/Snow Water Equivalent")

# For the SWE brick
start_time <- Sys.time()
for (i in 1:length(names(swe2_winter_cm))) {
  stacks <- stack(swe2_winter_cm[[i]])
  writeRaster(stacks, paste(names(stacks), ".swe.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time

#### Density

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("D:/Workspace_Kim/Chapter 2/Final SNODAS files/Density")

# For the density brick
start_time <- Sys.time()
for (i in 1:length(names(density))) {
  stacks <- stack(density[[i]])
  writeRaster(stacks, paste(names(stacks), ".snowdensity.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time









=======
#################################################################################################
#########         Code for Extracting snow depths from SNODAS files              ################
########            Gotten from Megan with some small adjustments                ################
#################################################################################################

# Author: Kimberly Thompson

# This code uses snow depth data and snow water equivalent data to calculate snow density, 
# and creates raster stacks of snow depth and snow density with a layer for each day from
# Dec 1 2016 - March 31 2017.

# Prior to executing code, snow depth and snow water equivalent files (.bil) were downloaded from 
# https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools
# These data are freely available

# Creates raster (bricks):
# ' .snowdep.tif'
# ' .swe.tif'
# ' .snowdensity.tif'




########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(rgdal)
library(foreign)
library(stringr)
library(tidyr)


# change path to where downloaded files reside
# setwd("D:/Workspace_Megan/SNODAS/gzdatfiles")
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded")
allfiles<-list.files(pattern="*.bil")
allfiles<-as.data.frame(allfiles)
colnames(allfiles)<-"from"
allfiles$temp<-allfiles$from
allfiles<-separate(allfiles,temp,into=c("base","ending"),sep=".bil")
allfiles$ending<-NULL
allfiles$to<-paste0(allfiles$base,".bil")


#creating a column with types for later. 
allfiles$type<-NA
for(i in 1:length(allfiles$from)){
  test<-strsplit(allfiles$to[i],"tS__")
  if(test[[1]][1]=="us_ssmv11034"){allfiles$type[i]="swe"}else{allfiles$type[i]="depth"}
  
}

#Now going to do a loop to read the rasters into a stack

#initializing a couple of things
blueprint<-raster(nrows=3351,ncols=6935,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                  xmn=-124.733333333329,xmx=-66.9416666666642,ymn=24.9499999999990,ymx=52.8749999999978)
depth<-stack(blueprint)
swe<-stack(blueprint)
j<-1
k<-1

for(i in 1:length(allfiles$to)){
  #read in the raster
  temprast<-raster(allfiles$to[i]) 
  #create a blueprint with desired coordinate system and extent
  blueprint<-raster(nrows=3351,ncols=6935,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                    xmn=-124.733333333329,xmx=-66.9416666666642,ymn=24.9499999999990,ymx=52.8749999999978)
  #fill in the values of the blueprint raster with the values read in
  values(blueprint)<-values(temprast) #fill in the values
  blueprint[blueprint==-9999]<-NA
  #assign to the correct raster stack depending on whether this is depth or swe data
  if(allfiles$type[i]=="depth"){
    depth[[j]]<-blueprint
    j<-j+1
  }else{
    swe[[k]]<-blueprint
    k<-k+1
  }
  print(i)
}

#Now have snow depths in Units = meters*1000. Snapsot at 06:00 UTC. One layer per day. First layer is 11/1/16, last layer is 4/30/17. 

# Remove dataframes that aren't needed
rm(allfiles)
rm(blueprint)
rm(temprast)
rm(test)

# Change the names in the new depth rasterstack
date1<-as.Date("2016-11-01")
date2<-as.Date("2017-04-30")
dates<-seq.Date(from=date1,to=date2,by=1)
names(depth) <- dates
names(swe) <- dates

# I only need December 1 2016 - March 31 2017 so trim down the rasterstack
names(depth)
# 31 = Dec 1st, 151 = March 31st
depth <- subset(depth, 31:151)
swe <- subset(swe, 31:151)

# Change the projection
#Use the land cover projection and project each brick to the landcover projection
crslandcover <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# crslandcover <- crs(landcover)

# Project the snowdepth stack - This takes a while. ~10.13 hours
start_time <- Sys.time()
depth2 <- projectRaster(from=depth, crs =  crslandcover)
swe2 <- projectRaster(from=swe, crs =  crslandcover)
end_time <- Sys.time()

############################################################
###### Crop the raster stack to the states extent ##########
###########################################################

#Import shapefile of these states
setwd("00_Data")
# setwd("D:/Workspace_Kim/Chapter 2")
states<-rgdal::readOGR("Great_Lakes_States.shp" )

# View file details:
summary( states )
# Quick plot:
plot( states )
# Check if projection matches site location data:
proj4string( states )
# Convert projection so that it does match:
states <- spTransform( states, proj4string( swe2 ) )

# Crop - this will still give a rectangular extent beyond the state boundaries but this is preferable
# because otherwise some of the raster cells around the perimeter of the states are missing
depth2_winter <- crop(depth2, extent(states))
swe2_winter <- crop(swe2, extent(states))

# Check CRS, extent, and resolution of the bricks and put info in the master metadata file
# Found in L:\LabMemberFolders\KimberlyThompson\Ch 2 Climate\Analysis\Interpolated Predictor Data
proj4string(depth2_winter)
extent(depth2_winter)
res(depth2_winter)

plot(depth2_winter[[1]])
plot(states, add=TRUE)

proj4string(swe2_winter)
extent(swe2_winter)
res(swe2_winter)

plot(swe2_winter[[1]])
plot(states, add=TRUE)



############################################################
######   Raster math to get density brick        ##########
###########################################################

density<-swe2_winter/depth2_winter 



############################################################
###### Raster math to get units into centimeters ##########
###########################################################

# Centimeters = meters * 100
# If what I have right now is meters * 1000, then to get Meters* 100 (centimeters)
# I need to divide each cell by 10.
depth2_winter_cm <- depth2_winter/10

plot(depth2_winter_cm[[1]])
plot(states, add=TRUE)


density_cm <- density/10

swe2_winter_cm <- swe2_winter/10



#############################################################################
### Extract each layer from the raster bricks into their own raster stack ###
### Each stack will represent one day and eventually will contain all     ###
### variables in the Boosted Regression Tree Models (daily wind speed, mean ###
### snow depth, daily tmax, daily tmin, land cover)                        ###

#### Depth

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("D:/Workspace_Kim/Chapter 2/Final SNODAS files")

# For the depth brick
start_time <- Sys.time()
for (i in 1:length(names(depth2_winter_cm))) {
  stacks <- stack(depth2_winter_cm[[i]])
  writeRaster(stacks, paste(names(stacks), ".snowdep.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time

#### SWE

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Interpolated Predictor Data/SNODAS/Snow Water Equivalent")

# For the SWE brick
start_time <- Sys.time()
for (i in 1:length(names(swe2_winter_cm))) {
  stacks <- stack(swe2_winter_cm[[i]])
  writeRaster(stacks, paste(names(stacks), ".swe.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time

#### Density

# Set the directory to store the files in
setwd("01_Analysis/Spatial Predictors")
# setwd("D:/Workspace_Kim/Chapter 2/Final SNODAS files/Density")

# For the density brick
start_time <- Sys.time()
for (i in 1:length(names(density))) {
  stacks <- stack(density[[i]])
  writeRaster(stacks, paste(names(stacks), ".snowdensity.tif", sep=""))
  print(i)
}
end_time <- Sys.time()
end_time-start_time









>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
