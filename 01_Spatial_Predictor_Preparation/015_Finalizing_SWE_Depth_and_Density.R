################################################################################
#########                                                              #########
#########              Finalizing SWE, Snow Depth, and                 #########
#########                    Snow Density rasters                      #########
################################################################################

# This code combines monthly SWE and Snowdepth rasters into a complete
# season-long raster (for each). 

# Additionally, using the metadata provided by NOHRSC which details the scaling
# factor and units of the two variables, it calculates the actual values
# of SWE and Depth in centimeters.

# Also, because of an issue with SWE measurements described in the metadata
# it uses the repair mask provided by NOHRSC to identify potentially erroneous
# 0 values in the SWE and snow depth data.

# Finally, it uses the corrected swe and depth rasters to create a raster of
# daily snow density.

# Metadata found at: https://nsidc.org/data/g02158/versions/1


### clean workspace and load required packages ###

rm(list = ls() ) 
gc() #releases memory

library(terra)

###############################################
###                                         ###
###              Data Loading               ###
###                                         ###
###############################################

# Not saved in the Github data folder
setwd("~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/SNODAS")

depth.files <- list.files(getwd(), pattern = "Depth")
swe.files <- list.files(getwd(), pattern = "SWE")

repair.mask <- terra :: rast("SNODAS_Zero_Repair_Mask.tif")


###############################################
###                                         ###
###           Combining rasters             ###
###                                         ###
###############################################

# Create blank raster
snow.depth <- terra :: rast()

# Combine
for (i in 1:length(depth.files)) {
  
  tmp.raster <- terra :: rast(depth.files[i])
  
  snow.depth <- c(snow.depth, tmp.raster)
  
  print(i)
  
}


# Create blank raster
swe <- terra :: rast()

# Combine
for (i in 1:length(swe.files)) {
  
  tmp.raster <- terra :: rast(swe.files[i])
  
  swe <- c(swe, tmp.raster)
  
  print(i)
  
}


###############################################
###                                         ###
###       Raster Math to Get Values         ###
###            in centimeters               ###
###############################################

# According to the SNODAS metadata both SWE and Depth are in meters but have
# a scale factor of 1000. "To convert integers in files to model output
# values, divide integers by scale factor"

# Convert to meters
snow.depth_meters <- snow.depth / 1000 
swe.meters <- swe / 1000

# Convert to centimeters (cm = meters*100)

snow.depth_cm <- snow.depth_meters * 100
swe_cm <- swe.meters * 100

rm(snow.depth_meters, swe.meters)
gc()

###############################################
###                                         ###
###       Fixing erroneous zero value       ###
###       in the swe and depth rasters      ###
###############################################

# A value of 0 in the repair mask indicates that the value in the swe and depth
# rasters can be used as is. A value of 1 indicates that the value in swe and 
# depth is erroneous and should be set to NA.

# Crop the repair mask to the extent of swe
repair_cropped <- terra :: crop(repair.mask, snow.depth_cm)

# Resample the cropped repair mask to match the resolution and alignment of
# the snow raster
repair_aligned <- resample(repair_cropped, snow.depth_cm[[1]], method="near")

# Round the values to ensure they remain 0 and 1
repair_aligned <- round(repair_aligned)

# Apply the mask to all layers of snow.depth_cm
snow_depth_masked <- mask(snow.depth_cm, repair_aligned,
                          maskvalues = 1, updatevalue = NA)

rm(snow.depth_cm, repair_cropped)
gc()

# Apply the mask to all layers of swe_cm
swe_masked <- mask(swe_cm, repair_aligned,
                   maskvalues = 1, updatevalue = NA)

rm(swe_cm, swe, snow.depth, repair_aligned, repair.mask)
gc()


###############################################
###                                         ###
###          Creating a stack for           ###
###              snow density               ###
###############################################

# The mathematical relationship between SWE, depth, and density is:
#        SWE = snow depth x snow density
#   or   snow density = SWE / snow depth

# Since both SWE and depth are in cm, the units of density will be 
# grams / cm3

density_masked <- swe_masked / snow_depth_masked


###############################################
###                                         ###
###            Write the rasters            ###
###                                         ###
###############################################

path <- "~/share/groups/mas/04_personal/Kim_T/Insect Cold Tolerance/00_Data/Individual Spatial Predictors/"

writeRaster(swe_masked, paste(path, "SWE_USACan_masked.tif", sep = ""))
writeRaster(snow_depth_masked, paste(path, "SnowDepth_USACan_masked.tif", sep = ""))
writeRaster(density_masked, paste(path, "SnowDensity_USACan_masked.tif", sep = ""))