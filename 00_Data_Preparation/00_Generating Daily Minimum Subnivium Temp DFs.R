<<<<<<< HEAD
##########################################################################################
###### Generate dataframes for each site showing daily minimum subnivium            ######
######                             temperatures.                                    ######
##########################################################################################

# Author: Kimberly Thompson

# This code finds the minimum daily subnivium temperature across the winter season (Dec 1
# 2016 - March 31 2017) for each site and creates a master dataframe with these values

# Produces datasets:
# 'Master_Min_Subnivium_Temps.csv'
# Minimum Subnvium Temp csvs with 1 per site (9 sites)



########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)

# Set working directory
# setwd("L:/LabMemberFolders/KimberlyThompson/Greenhouses/Data Analysis/SEASON 2/Greenhouses R Data - S2/Wide form String Data")
path <- "00_Data/Wide Form String Data"

# List the files
# subfiles <- list.files(getwd())
subfiles <- list.files(path)

# List the GH locations
Locations <- c("A", "C", "HH", "L", "M", "MTD", "MTOP", "SW", "TH")

# Loop to Read in wide form string data for each site
# aggregate by day
# Then take the minimum temperature across probes for each day to get a house-level subnivium temperature

for (k in 1:length(subfiles)) {
  
  # Read in the file and fix the date and location columns
  # setwd("L:/LabMemberFolders/KimberlyThompson/Greenhouses/Data Analysis/SEASON 2/Greenhouses R Data - S2/Wide form String Data")
  setwd(path)
  
  subtemp <- as.data.frame(read.csv(file = subfiles[k]), colClasses=c(rep("character", 3), "integer", rep("character", 6),
                                                            rep("numeric", 22)))
  
  subtemp$Date<-gsub("/", "-", subtemp$Date)
  subtemp$Date<-parse_date_time(subtemp$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
  subtemp$Date<-as.POSIXct(subtemp$Date, tz = "US/Central", format = "%Y-%m-%d")
  
  subtemp$Loc<-as.character(subtemp$Loc)
  
  # For each treatment, aggregate by day
  daily <- aggregate(subtemp, by = list(Date = subtemp$Date, Treat = subtemp$Treat), FUN = min)
  
  # Get rid of unnecessary columns
  daily <- daily[-c(3:12)]
  
  # Add Loc column back in
  daily$Loc <- Locations[k]
  
  # Separate dataframe into internal (S1 through S16) and external (S17 through S20)
  internal <- daily[,c(1:18, 25)]
  external <- daily[,c(1:2, 19:22, 25)]
  
  ##### Internal #####
  # create new columns with the internal house minimum (S1 through S16)
  for (i in 1:length(internal$Date)) {
    
    internal$min.subniv[i] <- mean(as.numeric(internal[i, c(3:18)]), na.rm=TRUE)
    
  }
  
  # Get rid of individual probe values
  internal <- internal[,-c(3:18)]
  
  #### External #####
  #Sort by date
  external <- dplyr :: arrange(external, Date)
  
  # Rearrange the external dataframe so all temp values are in the same column
  external <- melt(data = external, id.vars = "Date", measure.vars = c("S17", "S18", "S19", "S20")) 
  
  # Group values from external by date, and then take the mean
  external.summary <- external %>%
    dplyr :: group_by(Date) %>%
    dplyr :: summarise(
      min.subniv = mean(value, na.rm=TRUE)
    )
  
  # Fill in the Treat and Loc columns
  external.summary$Treat <- "ext"
  external.summary$Loc <- Locations[k]
  
  #Bind the final dataframes together
  subniv.complete <- rbind(internal, external.summary)
  
  #Write CSV file
  # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")
  setwd("Output Data/Minimum Subnivium Temperature")
  write.csv(subniv.complete, paste(Locations[k], "_Min Subnivium Temps.csv", sep = ""), row.names = FALSE)
  
  print(k)
}
  

# Combine these location-specific dataframes into one big dataframe

#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")
setwd("O0_Data/Minimum Subnivium Temperature")

#List the files
subniv.files <- list.files(getwd())

# Create a new dataframe to add each dataframe to
subniv.master <- data.frame(Date = character(), Treat = character(), Loc = character(),
                            min.subniv = numeric())

for(i in 1:length(subniv.files)) {
  
  individ.file <- read.csv(subniv.files[i], header = TRUE)
  
  subniv.master <- rbind(subniv.master, individ.file)
  
  print(i)
}
  
# Write the subnivium master file
write.csv(subniv.master, "Master_Min Subnivium Temps.csv", row.names = FALSE)
=======
##########################################################################################
###### Generate dataframes for each site showing daily minimum subnivium            ######
######                             temperatures.                                    ######
##########################################################################################

# Author: Kimberly Thompson

# This code finds the minimum daily subnivium temperature across the winter season (Dec 1
# 2016 - March 31 2017) for each site and creates a master dataframe with these values

# Produces datasets:
# 'Master_Min_Subnivium_Temps.csv'
# Minimum Subnvium Temp csvs with 1 per site (9 sites)



########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)

# Set working directory
# setwd("L:/LabMemberFolders/KimberlyThompson/Greenhouses/Data Analysis/SEASON 2/Greenhouses R Data - S2/Wide form String Data")
path <- "00_Data/Wide Form String Data"

# List the files
# subfiles <- list.files(getwd())
subfiles <- list.files(path)

# List the GH locations
Locations <- c("A", "C", "HH", "L", "M", "MTD", "MTOP", "SW", "TH")

# Loop to Read in wide form string data for each site
# aggregate by day
# Then take the minimum temperature across probes for each day to get a house-level subnivium temperature

for (k in 1:length(subfiles)) {
  
  # Read in the file and fix the date and location columns
  # setwd("L:/LabMemberFolders/KimberlyThompson/Greenhouses/Data Analysis/SEASON 2/Greenhouses R Data - S2/Wide form String Data")
  setwd(path)
  
  subtemp <- as.data.frame(read.csv(file = subfiles[k]), colClasses=c(rep("character", 3), "integer", rep("character", 6),
                                                            rep("numeric", 22)))
  
  subtemp$Date<-gsub("/", "-", subtemp$Date)
  subtemp$Date<-parse_date_time(subtemp$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
  subtemp$Date<-as.POSIXct(subtemp$Date, tz = "US/Central", format = "%Y-%m-%d")
  
  subtemp$Loc<-as.character(subtemp$Loc)
  
  # For each treatment, aggregate by day
  daily <- aggregate(subtemp, by = list(Date = subtemp$Date, Treat = subtemp$Treat), FUN = min)
  
  # Get rid of unnecessary columns
  daily <- daily[-c(3:12)]
  
  # Add Loc column back in
  daily$Loc <- Locations[k]
  
  # Separate dataframe into internal (S1 through S16) and external (S17 through S20)
  internal <- daily[,c(1:18, 25)]
  external <- daily[,c(1:2, 19:22, 25)]
  
  ##### Internal #####
  # create new columns with the internal house minimum (S1 through S16)
  for (i in 1:length(internal$Date)) {
    
    internal$min.subniv[i] <- mean(as.numeric(internal[i, c(3:18)]), na.rm=TRUE)
    
  }
  
  # Get rid of individual probe values
  internal <- internal[,-c(3:18)]
  
  #### External #####
  #Sort by date
  external <- dplyr :: arrange(external, Date)
  
  # Rearrange the external dataframe so all temp values are in the same column
  external <- melt(data = external, id.vars = "Date", measure.vars = c("S17", "S18", "S19", "S20")) 
  
  # Group values from external by date, and then take the mean
  external.summary <- external %>%
    dplyr :: group_by(Date) %>%
    dplyr :: summarise(
      min.subniv = mean(value, na.rm=TRUE)
    )
  
  # Fill in the Treat and Loc columns
  external.summary$Treat <- "ext"
  external.summary$Loc <- Locations[k]
  
  #Bind the final dataframes together
  subniv.complete <- rbind(internal, external.summary)
  
  #Write CSV file
  # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")
  setwd("Output Data/Minimum Subnivium Temperature")
  write.csv(subniv.complete, paste(Locations[k], "_Min Subnivium Temps.csv", sep = ""), row.names = FALSE)
  
  print(k)
}
  

# Combine these location-specific dataframes into one big dataframe

#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")
setwd("O0_Data/Minimum Subnivium Temperature")

#List the files
subniv.files <- list.files(getwd())

# Create a new dataframe to add each dataframe to
subniv.master <- data.frame(Date = character(), Treat = character(), Loc = character(),
                            min.subniv = numeric())

for(i in 1:length(subniv.files)) {
  
  individ.file <- read.csv(subniv.files[i], header = TRUE)
  
  subniv.master <- rbind(subniv.master, individ.file)
  
  print(i)
}
  
# Write the subnivium master file
write.csv(subniv.master, "Master_Min Subnivium Temps.csv", row.names = FALSE)
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
  