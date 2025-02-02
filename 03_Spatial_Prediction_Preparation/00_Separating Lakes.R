<<<<<<< HEAD
######################################################################################
###########           Ensuring that Great Lakes remain as NA          ################
###########         and do not show durations for SCP analysis        ################
###########                                                           ################
######################################################################################


# Author: Kimberly Thompson

# This code creates a csv of coordinates for the Great Lakes so that they can be 
# designated as NA for any question regarding how insect vulnerability has changed.

# Dataset produced:
# 'Separating Lakes.csv'




# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)


# We found that in creaing maps of duration, because the sum function necessarily needs to ignore NAs since some areas
# will be NA when they have no snow, but then will have snow on a later day, the resulting maps were showing the interior lakes 
# (in Minnesotat, Wisconsin, etc.) as having duration of 0 (and therefore were dark red), when really they should be 
# showing as NA (and be in white). Below is a workaround to remedy this problem involving the following steps.

# 1. Reclassify daily rasters >=0.5 to 1, <0.5 to 0, NA to 50 (50 is an arbitrary random value)
# 2. Sum these daily rasters together (into season-long dummy raster)
# 3. Convert dummy raster to dataframe.
# 4. Pare down dataframe to include only values of 6050 (= 50, the arbitrary value * 121 days = 6050, 121 days because lakes
#     should always be NA).
# 5. Clear everything except for the pared dataframe.
# 6. Rerun daily reclassification and sum function but this time without changing NA values.
# 7. Convert duration raster to a dataframe.
# 8. Merge dataframes together and overwrite cells that are lakes (values of 605 in dummy dataframe) to NA
# 9. Convert newly revised (and correct) dataframe back to a raster and save it.
# 10. The pared dataframe (called separating lakes) can be used for the other two treatments as is, so for scenario +3 (house 3)
#      and scenario +5 (house 5) this process can be repeated starting from step 6.


#########################################################################################################
##### Making a dataframe that shows the interior lakes (so they can continue to be NA)           ########
#########################################################################################################

# This can be done using any of the summary measures (mean, median, min, or max) and any treatment as a model. The same dataframe 
# of interior lakes will subsequently be applied to all rasters.

path <- "01_Analysis/Spatial Predictions/Corrected Stacks by day/External"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Mean"
Treatment.list <- list.files(path, pattern = "X")

# Make a blank raster to store the altered layers in
newstack <- stack()

for (i in 1:length(Treatment.list)) {
  
  # Read in the daily raster
  setwd(path)
  subniv <- raster :: raster(Treatment.list[[i]])
  
  # Reclassify the raster 
  # The -7.3 value is for bumblebees but does not get used for the lakes so 
  # is irrelevant
  subniv[subniv >= -7.3] <- 0
  subniv[subniv < -7.3] <- 1
  subniv[is.na(subniv)] <- 50 # Change the NA values 
  
  # Add the reclassified layer to the blank raster stack for this treatment
  newstack <- addLayer(newstack, subniv)
  
  print(i)
  
}

# Sum daily rasters together - true lakes which would have NA values for all days will have value of 6050
duration <- overlay(newstack, fun=sum.na)

# Make a dataframe of the duration raster which exists now only as a dummy raster to be able to extract the lakes
separating.lakes <- as.data.frame(duration, xy=TRUE)

# Pare down to be season-long NA values (those cells where the value = 6050)
separating.lakes <- separating.lakes[separating.lakes$layer == 6050,]

setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
write.csv(separating.lakes, "Separating Lakes.csv", row.names = FALSE)

# Now clear everything except for separating lakes
rm(duration, newstack, subniv, i, path, Treatment.list)

=======
######################################################################################
###########           Ensuring that Great Lakes remain as NA          ################
###########         and do not show durations for SCP analysis        ################
###########                                                           ################
######################################################################################


# Author: Kimberly Thompson

# This code creates a csv of coordinates for the Great Lakes so that they can be 
# designated as NA for any question regarding how insect vulnerability has changed.

# Dataset produced:
# 'Separating Lakes.csv'




# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)


# We found that in creaing maps of duration, because the sum function necessarily needs to ignore NAs since some areas
# will be NA when they have no snow, but then will have snow on a later day, the resulting maps were showing the interior lakes 
# (in Minnesotat, Wisconsin, etc.) as having duration of 0 (and therefore were dark red), when really they should be 
# showing as NA (and be in white). Below is a workaround to remedy this problem involving the following steps.

# 1. Reclassify daily rasters >=0.5 to 1, <0.5 to 0, NA to 50 (50 is an arbitrary random value)
# 2. Sum these daily rasters together (into season-long dummy raster)
# 3. Convert dummy raster to dataframe.
# 4. Pare down dataframe to include only values of 6050 (= 50, the arbitrary value * 121 days = 6050, 121 days because lakes
#     should always be NA).
# 5. Clear everything except for the pared dataframe.
# 6. Rerun daily reclassification and sum function but this time without changing NA values.
# 7. Convert duration raster to a dataframe.
# 8. Merge dataframes together and overwrite cells that are lakes (values of 605 in dummy dataframe) to NA
# 9. Convert newly revised (and correct) dataframe back to a raster and save it.
# 10. The pared dataframe (called separating lakes) can be used for the other two treatments as is, so for scenario +3 (house 3)
#      and scenario +5 (house 5) this process can be repeated starting from step 6.


#########################################################################################################
##### Making a dataframe that shows the interior lakes (so they can continue to be NA)           ########
#########################################################################################################

# This can be done using any of the summary measures (mean, median, min, or max) and any treatment as a model. The same dataframe 
# of interior lakes will subsequently be applied to all rasters.

path <- "01_Analysis/Spatial Predictions/Corrected Stacks by day/External"
# path <- "Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Corrected Daily Summaries/External/Mean"
Treatment.list <- list.files(path, pattern = "X")

# Make a blank raster to store the altered layers in
newstack <- stack()

for (i in 1:length(Treatment.list)) {
  
  # Read in the daily raster
  setwd(path)
  subniv <- raster :: raster(Treatment.list[[i]])
  
  # Reclassify the raster 
  # The -7.3 value is for bumblebees but does not get used for the lakes so 
  # is irrelevant
  subniv[subniv >= -7.3] <- 0
  subniv[subniv < -7.3] <- 1
  subniv[is.na(subniv)] <- 50 # Change the NA values 
  
  # Add the reclassified layer to the blank raster stack for this treatment
  newstack <- addLayer(newstack, subniv)
  
  print(i)
  
}

# Sum daily rasters together - true lakes which would have NA values for all days will have value of 6050
duration <- overlay(newstack, fun=sum.na)

# Make a dataframe of the duration raster which exists now only as a dummy raster to be able to extract the lakes
separating.lakes <- as.data.frame(duration, xy=TRUE)

# Pare down to be season-long NA values (those cells where the value = 6050)
separating.lakes <- separating.lakes[separating.lakes$layer == 6050,]

setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
write.csv(separating.lakes, "Separating Lakes.csv", row.names = FALSE)

# Now clear everything except for separating lakes
rm(duration, newstack, subniv, i, path, Treatment.list)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
