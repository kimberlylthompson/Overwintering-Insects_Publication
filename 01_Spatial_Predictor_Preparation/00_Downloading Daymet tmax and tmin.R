<<<<<<< HEAD

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
# The extent of the shapefile of states in WGS84 is the following:
#Coordinates:
# min       max
# x -97.22910 -71.87048
# y  36.98677  49.37173


# Download tmax files
download_daymet_ncss(location = c(49.37173, -97.22910, 36.98677, -71.87048),
                     start = 2015,
                     end = 2015,
                     param = "tmax",
                     path = getwd())
                     # Change path for particular user
                     # path = "Z:/Daymet_V3_Daily/data/Thompson/data/")



# Download tmin files
download_daymet_ncss(location = c(49.37173, -97.22910, 36.98677, -71.87048),
                     start = 2016,
                     end = 2017,
                     param = "tmin",
                     path = getwd())
                     # Change path for particular user
                     # path = "D:/Daymet_Kim/")


=======

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
# The extent of the shapefile of states in WGS84 is the following:
#Coordinates:
# min       max
# x -97.22910 -71.87048
# y  36.98677  49.37173


# Download tmax files
download_daymet_ncss(location = c(49.37173, -97.22910, 36.98677, -71.87048),
                     start = 2015,
                     end = 2015,
                     param = "tmax",
                     path = getwd())
                     # Change path for particular user
                     # path = "Z:/Daymet_V3_Daily/data/Thompson/data/")



# Download tmin files
download_daymet_ncss(location = c(49.37173, -97.22910, 36.98677, -71.87048),
                     start = 2016,
                     end = 2017,
                     param = "tmin",
                     path = getwd())
                     # Change path for particular user
                     # path = "D:/Daymet_Kim/")


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
