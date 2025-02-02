<<<<<<< HEAD
#########################################################################
####                                                                #####
####                  Summarizing the extent of                     #####
####                vulnerability for each species                  #####
####                  in each warming scenario                      #####
#########################################################################

# Author: Kimberly Thompson

# This code summarizes the extent of vulnerability (km2) for each insect
# for each day of the winter season (Dec 1, 2016 - March 31, 2017).

# Datasets produced:
# 'Extent of Vulnerability Summary.csv'

# Tables created from the datasets:
# Table S8
# Table S9


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(tidyverse)


################################
####                        ####
####       Data Loading     ####
################################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
extent.files <- list.files(getwd())
best <- list.files(getwd(), pattern = "Best")
worst <- list.files(getwd(), pattern = "Worst")



# Set up blank dataframe to store summary in
# type is the climate treatment (external, H3, H5)
# Scenario is best or worst case
summary <- data.frame(Species = character(), Type = character(),
                      Scenario = character(), Mean = numeric(),
                      SD = numeric(),
                      Min = numeric(), Max = numeric())


################################
####  Summary calculations  ####
####                        ####
################################

for (i in 1:length(extent.files)) {

  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  tmp.file <- read.csv(extent.files[i])
  
  # summarize the values
  tmp.summ <- tmp.file %>%
    group_by(Type) %>%
    dplyr :: summarize(Mean = mean(value),
                       SD = sd(value),
                       Min = min(value), Max = max(value))
  
  # add the species name to the dataframe
  tmp.summ$Species <- str_split(extent.files[i], "_")[[1]][1]
  
  # Add the Scenario to the dataframe
  tmp.summ$Scenario <- trimws(substr(str_split(extent.files[i], "_")[[1]][2], 1, 5))
  
  # add the data to the summary dataframe
  summary <- rbind(summary, tmp.summ)
  
  print(i)
  
}


# Sort the summary dataframe according to Scenario
summary <- arrange(summary, Scenario)

# Adjust the number of decimal places for the mean column
summary$Mean <- format(round(summary$Mean, 2), nsmall = 2)


# write the csv
setwd("01_Analysis/Summary_Results")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities")
write.csv(summary, "Extent of Vulnerability Summary.csv", row.names = FALSE)


=======
#########################################################################
####                                                                #####
####                  Summarizing the extent of                     #####
####                vulnerability for each species                  #####
####                  in each warming scenario                      #####
#########################################################################

# Author: Kimberly Thompson

# This code summarizes the extent of vulnerability (km2) for each insect
# for each day of the winter season (Dec 1, 2016 - March 31, 2017).

# Datasets produced:
# 'Extent of Vulnerability Summary.csv'

# Tables created from the datasets:
# Table S8
# Table S9


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(tidyverse)


################################
####                        ####
####       Data Loading     ####
################################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
extent.files <- list.files(getwd())
best <- list.files(getwd(), pattern = "Best")
worst <- list.files(getwd(), pattern = "Worst")



# Set up blank dataframe to store summary in
# type is the climate treatment (external, H3, H5)
# Scenario is best or worst case
summary <- data.frame(Species = character(), Type = character(),
                      Scenario = character(), Mean = numeric(),
                      SD = numeric(),
                      Min = numeric(), Max = numeric())


################################
####  Summary calculations  ####
####                        ####
################################

for (i in 1:length(extent.files)) {

  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  tmp.file <- read.csv(extent.files[i])
  
  # summarize the values
  tmp.summ <- tmp.file %>%
    group_by(Type) %>%
    dplyr :: summarize(Mean = mean(value),
                       SD = sd(value),
                       Min = min(value), Max = max(value))
  
  # add the species name to the dataframe
  tmp.summ$Species <- str_split(extent.files[i], "_")[[1]][1]
  
  # Add the Scenario to the dataframe
  tmp.summ$Scenario <- trimws(substr(str_split(extent.files[i], "_")[[1]][2], 1, 5))
  
  # add the data to the summary dataframe
  summary <- rbind(summary, tmp.summ)
  
  print(i)
  
}


# Sort the summary dataframe according to Scenario
summary <- arrange(summary, Scenario)

# Adjust the number of decimal places for the mean column
summary$Mean <- format(round(summary$Mean, 2), nsmall = 2)


# write the csv
setwd("01_Analysis/Summary_Results")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities")
write.csv(summary, "Extent of Vulnerability Summary.csv", row.names = FALSE)


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
