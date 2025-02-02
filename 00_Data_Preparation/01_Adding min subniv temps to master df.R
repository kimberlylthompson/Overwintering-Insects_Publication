<<<<<<< HEAD
##########################################################################################
###### Add column of daily minimum subnivium temperatures for each site and         ######
######      treatment to the master subnivium dataframe which includes the key      ######
######      columns used in separate analysis of subnivium extent and duration, and ######
######      will be used in this project to evaluate minimum temps insects would be ######
######      exposed to                                                              ######
##########################################################################################

# Author: Kimberly Thompson

# This code creates a master dataframe that includes all necessry columns to execute 
# the analyses for the overwintering insects project:

# "Date"          
# "Treat" - Treatment (external, GH+0, GH+3, GH+5)         
# "Loc" - Site Code           
# "Julian" - Julian Date       
# "Tairmin" - minimum air temperature      
# "Tairmax" - maximum air temperature       
# "Snowmed" - Median snow depth       
# "Wind" - Average wind speed          
# "Cover" - Land cover category
# "density" - Snow density    
# "min.subniv" - Minimum subnivium temperature


# Produces datasets:
# 'Subniv Temps and Predictors_complete.csv'


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)


########################################################################################
## Read in the master dataframe used for project on Change in Subnivium Extent and Duration ##

path <- "00_Data/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 5 Subnivium Resilience/Analysis/Data")

presence<-read.csv(paste(path, "Subniv Temps and Predictors_complete.csv",
                                       sep = ""),
                         header = TRUE)
#Fix the dates
presence$Date<-gsub("/", "-", presence$Date)
presence$Date<-parse_date_time(presence$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
presence$Date<-as.POSIXct(presence$Date, tz = "US/Central", format = "%Y-%m-%d")


## Read in the master dataframe of minimum daily subnivium temps

path2 <- "Output_Data/Minimum Subnivium Temperature/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")

subnivium <- read.csv(paste(path2, "Master_Min Subnivium Temps.csv", sep = ""), header=TRUE)

#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")



###############################################
##### Merge the dataframes
#################################

total <- base :: merge(presence, subnivium, by = c("Date", "Treat", "Loc"), all.x=TRUE)

total <- dplyr :: arrange(total, Loc, Treat, Date)


# Write the complete dataframe
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")
write.csv(total, "Subniv Temps and Predictors_complete.csv", row.names = FALSE)

=======
##########################################################################################
###### Add column of daily minimum subnivium temperatures for each site and         ######
######      treatment to the master subnivium dataframe which includes the key      ######
######      columns used in separate analysis of subnivium extent and duration, and ######
######      will be used in this project to evaluate minimum temps insects would be ######
######      exposed to                                                              ######
##########################################################################################

# Author: Kimberly Thompson

# This code creates a master dataframe that includes all necessry columns to execute 
# the analyses for the overwintering insects project:

# "Date"          
# "Treat" - Treatment (external, GH+0, GH+3, GH+5)         
# "Loc" - Site Code           
# "Julian" - Julian Date       
# "Tairmin" - minimum air temperature      
# "Tairmax" - maximum air temperature       
# "Snowmed" - Median snow depth       
# "Wind" - Average wind speed          
# "Cover" - Land cover category
# "density" - Snow density    
# "min.subniv" - Minimum subnivium temperature


# Produces datasets:
# 'Subniv Temps and Predictors_complete.csv'


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)


########################################################################################
## Read in the master dataframe used for project on Change in Subnivium Extent and Duration ##

path <- "00_Data/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 5 Subnivium Resilience/Analysis/Data")

presence<-read.csv(paste(path, "Subniv Temps and Predictors_complete.csv",
                                       sep = ""),
                         header = TRUE)
#Fix the dates
presence$Date<-gsub("/", "-", presence$Date)
presence$Date<-parse_date_time(presence$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
presence$Date<-as.POSIXct(presence$Date, tz = "US/Central", format = "%Y-%m-%d")


## Read in the master dataframe of minimum daily subnivium temps

path2 <- "Output_Data/Minimum Subnivium Temperature/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Minimum Subnivium Temperature")

subnivium <- read.csv(paste(path2, "Master_Min Subnivium Temps.csv", sep = ""), header=TRUE)

#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")



###############################################
##### Merge the dataframes
#################################

total <- base :: merge(presence, subnivium, by = c("Date", "Treat", "Loc"), all.x=TRUE)

total <- dplyr :: arrange(total, Loc, Treat, Date)


# Write the complete dataframe
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")
write.csv(total, "Subniv Temps and Predictors_complete.csv", row.names = FALSE)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
