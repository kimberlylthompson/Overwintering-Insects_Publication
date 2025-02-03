

# Author: Kimberly Thompson

# This code downloads minimum daily temperature and maximum daily temperature
# for Dec 1 2016 - March 31, 2017 from Daymet
# https://daymet.ornl.gov/

# This code shows how to download, but the data is freely available



library(daymetr)

setwd("00_Data")
# setwd("Z:/Daymet_V3_Daily/data/Thompson/data/")
# setwd("D:/Daymet_Kim/")

# Download code retrieved from https://khufkens.github.io/daymetr/
# https://github.com/bluegreen-labs/daymetr

# netCDF subset (ncss) data
# download_daymet_ncss(location = c(36.61,-85.37,33.57,-81.29),
#                      start = 1980,
#                      end = 1980,
#                      param = "tmin")

# Parameter	Description
# location: bounding box extent defined as top left / bottom right pair c(lat,lon,lat,lon) --> ymax, xmin then ymin, xmax
# start: start year of the time series (data start in 1980)
# end: end year of the time series (current year - 2 years, use force = TRUE to override)
# param: climate variable you want to download vapour pressure (vp),
#           minimum and maximum temperature (tmin,tmax), snow water equivalent (swe), solar radiation (srad),
#           precipitation (prcp) , day length (dayl). The default setting is ALL,
#           this will download all the previously mentioned climate variables.
# path: path where to store the data, defaults to tempdir()
# silent:	suppress the verbose output

# Keep in mind that the bounding box is defined by the minimum (square) bounding box in a 
# Lambert Conformal Conic (LCC) projection as defined by the provided geographic coordinates. 
# In general the query area will be larger than the requested location. 
# For more information I refer to Daymet documentation on the web service.


# For the states which I want data for:  MN, IA, WI, IL, MI, IN, OH, PA, & NY
# and the provinces: Manitoba, Ontario, and Quebec

# Read in CSV of bounding box
bbox <- read.csv("Bounding Box Coordinates.csv", header = TRUE)


# The extent of the shapefile of states in WGS84 is the following:
#Coordinates:
# min       max
# x -98     -72
# y  37      55


# Download tmax files
download_daymet_ncss(location = c(55, -98, 37, -72),
                     start = 2016,
                     end = 2017,
                     param = "tmax",
                     path = "H:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/")
                     # Change path for particular user
                     # path = "Z:/Daymet_V3_Daily/data/Thompson/data/")



# Download tmin files
download_daymet_ncss(location = c(55, -98, 32, -72),
                     start = 2016,
                     end = 2017,
                     param = "tmin",
                     path = "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/")
                     # Change path for particular user
                     # path = "D:/Daymet_Kim/")







