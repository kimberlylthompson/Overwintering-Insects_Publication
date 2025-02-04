<<<<<<< HEAD
#################################################################################################
#########         Code for downloading snow depths and snow water                ################
########                       equivalent from SNODAS                            ################
#################################################################################################

# Author: Kimberly Thompson

# This code downloads snow depth data and snow water equivalent data from the National Snow and
# Ice Data Center for the period of 
# Dec 1 2016 - March 31 2017.

# https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools
# These data are freely available

# Code taken from 
# https://nsidc.org/data/user-resources/help-center/how-access-and-download-noaansidc-data#anchor-how-to-access-data-using-a-python-or-r-script

# Metadata found in
# https://nsidc.org/sites/default/files/g02158-v001-userguide_2_1.pdf

# Creates .tar files:
# 1 for each month/day combination




########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(rvest)
library(tidyverse)


###########################################
####    Set Conditions and Downloads   ####
###########################################

# Data are found: https://noaadata.apps.nsidc.org/NOAA/G02158/
# Note that the unmasked folder contains data that extends into Canada so this
# is what we want

# Directories are organized by year and month (within that is a .tar file
# for each day)

# Set the working directory for where data should be downloaded to
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded")


############################
###        December      ###
############################

# Define the URL
dec.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2016/12_Dec/"

dec.page <- rvest :: read_html(dec.url)

dec.files <- dec.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
dec.files <- dec.files[2:length(dec.files)]

# Download the December 2016 file
for(i in 1:length(dec.files)) {
  u <- paste(dec.url, dec.files[i], sep = "/")
  download.file(u, dec.files[i], mode = "wb")
  
  print(i)
}



############################
###        January       ###
############################

# Define the URL
jan.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/01_Jan/"

jan.page <- rvest :: read_html(jan.url)

jan.files <- jan.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
jan.files <- jan.files[2:length(jan.files)]

# Download the janember 2016 file
for(i in 1:length(jan.files)) {
  u <- paste(jan.url, jan.files[i], sep = "/")
  download.file(u, jan.files[i], mode = "wb")
  
  print(i)
}



############################
###        February      ###
############################

# Define the URL
feb.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/02_Feb/"

feb.page <- rvest :: read_html(feb.url)

feb.files <- feb.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
feb.files <- feb.files[2:length(feb.files)]

# Download the febember 2016 file
for(i in 1:length(feb.files)) {
  u <- paste(feb.url, feb.files[i], sep = "/")
  download.file(u, feb.files[i], mode = "wb")
  
  print(i)
}

# Received these warnings but not sure which day they refer to:
# Warning messages:
#   1: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 3100672 != reported length 43724800
# 2: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 1101824 != reported length 42096640
# 3: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 15093760 != reported length 45885440


############################
###        March         ###
############################

# Define the URL
mar.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/03_Mar/"

mar.page <- rvest :: read_html(mar.url)

mar.files <- mar.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
mar.files <- mar.files[2:length(mar.files)]

# Download the marember 2016 file
for(i in 1:length(mar.files)) {
  u <- paste(mar.url, mar.files[i], sep = "/")
  download.file(u, mar.files[i], mode = "wb")
  
  print(i)
}


###########################
###  Supplemental Data  ###
###########################

# There were several .tar files that were corrupted or incomplete and needed
# to be downloaded manually:

# These include
# SWE: February 3, 2017
# Depth: January 21, 2017
#        Feburary 1, 2017
#        February 3, 2017
#        February 8, 2017

# After trying to download the unmasked version, the problem remains, but the
# masked version (including USA only should be ok so will use this for these
# days instead)

# Jan 21
# Define the URL
day1.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/01_Jan/SNODAS_20170121.tar"

# Destination name
destination1 <- "SNODAS_masked_20170121.tar"

download.file(day1.url, destination1, mode = "wb")

# Feb 1
# Define the URL
day2.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170201.tar"

# Destination name
destination2 <- "SNODAS_masked_20170201.tar"

download.file(day2.url, destination2, mode = "wb")

# Feb 3
# Define the URL
day3.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170203.tar"

# Destination name
destination3 <- "SNODAS_masked_20170203.tar"

download.file(day3.url, destination3, mode = "wb")


# Feb 8
# Define the URL
day4.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170208.tar"

# Destination name
destination4 <- "SNODAS_masked_20170208.tar"

download.file(day4.url, destination4, mode = "wb")




###########################
### Additional Data     ###
###########################

# There are issues with SWE measurements from the period of 10/09/14 to 10/10/19

# In December 2018, NSIDC became aware that there were persistent, erroneous zeroes in SWE 
# data primarily found around the perimeters of water bodies and along coastlines.

# This could definitely impact the Great Lakes.

# The issue arose when NOHRSC adjusted their land/water mask in October 2014. Because of an 
# error made at the time in the modification of the model state variables, the new mask was never 
# properly initialized for many locations on the SNODAS grid. Exactly 126,950 cells on the grid, 
# primarily found around the perimeters of water bodies and along coastlines, were affected. In the 
# SNODAS archives at NSIDC, these cells will erroneously have zero values for snow water 
# equivalent and snow depth for the period from 09 October 2014 through 10 October 2019.

# NOHRSC provided a GeoTIFF repair mask that can be used to identify the affected grid cells in the 
# unmasked version of the SNODAS data files. A value of zero in this mask file indicates a cell where 
# NSIDC archives should be used as is. A value of one in this mask file indicates a cell where 
# gridded SNODAS archives of snow depth and snow water equivalent from 2014-10-09 through 
# 2019-10-10 should be set to a no-data value.


# Define the URL
url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/ancillary/SNODAS_Zero_Repair_Mask.tif"

# Destination name
destination <- "SNODAS_Zero_Repair_Mask.tif"

download.file(url, destination, mode = "wb")
=======
#################################################################################################
#########         Code for downloading snow depths and snow water                ################
########                       equivalent from SNODAS                            ################
#################################################################################################

# Author: Kimberly Thompson

# This code downloads snow depth data and snow water equivalent data from the National Snow and
# Ice Data Center for the period of 
# Dec 1 2016 - March 31 2017.

# https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools
# These data are freely available

# Code taken from 
# https://nsidc.org/data/user-resources/help-center/how-access-and-download-noaansidc-data#anchor-how-to-access-data-using-a-python-or-r-script

# Metadata found in
# https://nsidc.org/sites/default/files/g02158-v001-userguide_2_1.pdf

# Creates .tar files:
# 1 for each month/day combination




########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(rvest)
library(tidyverse)


###########################################
####    Set Conditions and Downloads   ####
###########################################

# Data are found: https://noaadata.apps.nsidc.org/NOAA/G02158/
# Note that the unmasked folder contains data that extends into Canada so this
# is what we want

# Directories are organized by year and month (within that is a .tar file
# for each day)

# Set the working directory for where data should be downloaded to
setwd("D:/My Drive/Ch 4 Bumblebees/00_Data/Raw/SNODAS/Downloaded")


############################
###        December      ###
############################

# Define the URL
dec.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2016/12_Dec/"

dec.page <- rvest :: read_html(dec.url)

dec.files <- dec.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
dec.files <- dec.files[2:length(dec.files)]

# Download the December 2016 file
for(i in 1:length(dec.files)) {
  u <- paste(dec.url, dec.files[i], sep = "/")
  download.file(u, dec.files[i], mode = "wb")
  
  print(i)
}



############################
###        January       ###
############################

# Define the URL
jan.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/01_Jan/"

jan.page <- rvest :: read_html(jan.url)

jan.files <- jan.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
jan.files <- jan.files[2:length(jan.files)]

# Download the janember 2016 file
for(i in 1:length(jan.files)) {
  u <- paste(jan.url, jan.files[i], sep = "/")
  download.file(u, jan.files[i], mode = "wb")
  
  print(i)
}



############################
###        February      ###
############################

# Define the URL
feb.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/02_Feb/"

feb.page <- rvest :: read_html(feb.url)

feb.files <- feb.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
feb.files <- feb.files[2:length(feb.files)]

# Download the febember 2016 file
for(i in 1:length(feb.files)) {
  u <- paste(feb.url, feb.files[i], sep = "/")
  download.file(u, feb.files[i], mode = "wb")
  
  print(i)
}

# Received these warnings but not sure which day they refer to:
# Warning messages:
#   1: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 3100672 != reported length 43724800
# 2: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 1101824 != reported length 42096640
# 3: In download.file(u, feb.files[i], mode = "wb") :
#   downloaded length 15093760 != reported length 45885440


############################
###        March         ###
############################

# Define the URL
mar.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/2017/03_Mar/"

mar.page <- rvest :: read_html(mar.url)

mar.files <- mar.page %>% rvest :: html_nodes("a") %>% rvest :: html_attr("href")

# files will include an upper level "go back" option: remove to only have days
mar.files <- mar.files[2:length(mar.files)]

# Download the marember 2016 file
for(i in 1:length(mar.files)) {
  u <- paste(mar.url, mar.files[i], sep = "/")
  download.file(u, mar.files[i], mode = "wb")
  
  print(i)
}


###########################
###  Supplemental Data  ###
###########################

# There were several .tar files that were corrupted or incomplete and needed
# to be downloaded manually:

# These include
# SWE: February 3, 2017
# Depth: January 21, 2017
#        Feburary 1, 2017
#        February 3, 2017
#        February 8, 2017

# After trying to download the unmasked version, the problem remains, but the
# masked version (including USA only should be ok so will use this for these
# days instead)

# Jan 21
# Define the URL
day1.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/01_Jan/SNODAS_20170121.tar"

# Destination name
destination1 <- "SNODAS_masked_20170121.tar"

download.file(day1.url, destination1, mode = "wb")

# Feb 1
# Define the URL
day2.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170201.tar"

# Destination name
destination2 <- "SNODAS_masked_20170201.tar"

download.file(day2.url, destination2, mode = "wb")

# Feb 3
# Define the URL
day3.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170203.tar"

# Destination name
destination3 <- "SNODAS_masked_20170203.tar"

download.file(day3.url, destination3, mode = "wb")


# Feb 8
# Define the URL
day4.url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/2017/02_Feb/SNODAS_20170208.tar"

# Destination name
destination4 <- "SNODAS_masked_20170208.tar"

download.file(day4.url, destination4, mode = "wb")




###########################
### Additional Data     ###
###########################

# There are issues with SWE measurements from the period of 10/09/14 to 10/10/19

# In December 2018, NSIDC became aware that there were persistent, erroneous zeroes in SWE 
# data primarily found around the perimeters of water bodies and along coastlines.

# This could definitely impact the Great Lakes.

# The issue arose when NOHRSC adjusted their land/water mask in October 2014. Because of an 
# error made at the time in the modification of the model state variables, the new mask was never 
# properly initialized for many locations on the SNODAS grid. Exactly 126,950 cells on the grid, 
# primarily found around the perimeters of water bodies and along coastlines, were affected. In the 
# SNODAS archives at NSIDC, these cells will erroneously have zero values for snow water 
# equivalent and snow depth for the period from 09 October 2014 through 10 October 2019.

# NOHRSC provided a GeoTIFF repair mask that can be used to identify the affected grid cells in the 
# unmasked version of the SNODAS data files. A value of zero in this mask file indicates a cell where 
# NSIDC archives should be used as is. A value of one in this mask file indicates a cell where 
# gridded SNODAS archives of snow depth and snow water equivalent from 2014-10-09 through 
# 2019-10-10 should be set to a no-data value.


# Define the URL
url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/ancillary/SNODAS_Zero_Repair_Mask.tif"

# Destination name
destination <- "SNODAS_Zero_Repair_Mask.tif"

download.file(url, destination, mode = "wb")
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
