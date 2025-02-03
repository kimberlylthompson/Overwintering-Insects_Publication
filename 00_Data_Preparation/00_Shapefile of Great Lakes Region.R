#################################################################################################
#########         Code for creating a shapefile of the state and                 ################
########                provinces around the Great Lakes                         ################
#################################################################################################

# Author: Kimberly Thompson

# This code creates a shapefile of the relevant states and provinces surrounding the Great
# Lakes.

# The bounding box of these states and provinces will be used to specify the 
# download extent of some of the spatial predictors like temperture from Daymet.

library(sf)
library(sp)
library(ggplot2)


setwd("00_Data")

# Read in shapefile of all states and provinces
all <- sf :: st_read("USACANAB.shp")

# Define coordinate system
albers <- sf :: st_crs(5070)

# Define coordintate system as albers
all <- sf :: st_set_crs( all, albers )

# Plot
ggplot() + geom_sf(data = all)


# the states which I want data for:  MN, IA, WI, IL, MI, IN, OH, PA, & NY
# the provinces which I want data for: Manitoba, Ontario, Quebec

# Crop to great lakes states and provinces
greatlakesregion <- all[all$STATE == "MIN" | all$STATE == "IA" |
                          all$STATE == "WIS" | all$STATE == "ILL" |
                          all$STATE == "MIC" | all$STATE == "IND" |
                          all$STATE == "OHI" | all$STATE == "PA" |
                          all$STATE == "NY" | all$STATE == "MAN" |
                          all$STATE == "ONT" | all$STATE == "QUE", ]

ggplot() + geom_sf(data = greatlakesregion)

# A large part of the Canadian provinces will end up getting cropped because
# SNODAS for snow depth and density does not go all the way north through the provinces.

# To avoid downloading unnecessary data (for temperature for example), crop
# greatlakesregion to 55 degrees north as the upper limit
# and the western limit at 98 degrees W
# and the eastern limit 72 W

# Create a bounding box with desired coordinates in WGS84
new_bbox <- st_bbox(c(xmin = -98, xmax = -72, 
                      ymin = 32, ymax = 55), crs = 4326)

# Convert to sf object
new_bbox_sf <- st_as_sfc(new_bbox)

# Transform to your Albers projection (assuming EPSG:5070)
new_bbox_albers <- st_transform(new_bbox_sf, crs = 5070)

# Extract the new bounding box in Albers coordinates
final_bbox <- st_bbox(new_bbox_albers)

# Crop the sf object
cropped_greatlakesregion <- st_crop(greatlakesregion, final_bbox)

ggplot() + geom_sf(data = cropped_greatlakesregion)


# Save the shapefile
st_write(cropped_greatlakesregion, "Great_Lakes_States and Prov.shp")

# Make a dataframe with the degree and meter coordinates

# Create dataframe from final_bbox
df1 <- data.frame(coordinate = names(final_bbox), albers = as.numeric(final_bbox))

# Create dataframe from new_bbox
df2 <- data.frame(coordinate = names(new_bbox), wgs84 = as.numeric(new_bbox))

# Merge the two dataframes
result_df <- merge(df1, df2, by = "coordinate")

# Write the dataframe
write.csv(result_df, "Bounding Box Coordinates.csv", row.names = FALSE)



