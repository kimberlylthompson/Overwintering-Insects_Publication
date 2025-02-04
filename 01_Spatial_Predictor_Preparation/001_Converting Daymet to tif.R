##############################################################################################
######         Working with netCDF data from DAYMET for daily tmax and tmin             ######
##############################################################################################

library(terra)

#ncpath = path where the file was downloaded
ncpath <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/"

#ncname = name of the file
ncname <- "tmax_daily_2016_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

#Name of the variable shown in the file
dname <- "tmax"

# Read the netcdf file
tmax2016 <- terra :: rast(ncfname, var = dname)















#Code tutorial obtained from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

# Load the ncdf4 package
library(ncdf4)

### Set the values for some temporary identifier variables ###

#ncpath = path where the file was downloaded
ncpath <- "D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/Daymet/"

#ncname = name of the file
ncname <- "tmax_daily_2016_ncss"

#ncfname = pasted path and name
ncfname <- paste(ncpath, ncname, ".nc", sep="")

#Name of the variable shown in the file
dname <- "tmax"

### Open the netCDF dataset and print some basic information
ncin <- nc_open(ncfname)
print(ncin)

# Get the coordinates: the name of the coordinate (e.g. lon, longitude, or x) is found in the printed
# dimension info from the above command. Here longitude is called "x" and latitude is called "y".
lon <- ncvar_get(ncin,"x")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"y")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time - 365 days are covered although I will not need all of these days
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
# Note the structure of the time units attribute. The object tunits has two components 
# hasatt (a logical variable), and tunits$value, the actual “time since” string.
tunits


### Getting the variable information ###

# get max temperature
tmax_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmax_array)
