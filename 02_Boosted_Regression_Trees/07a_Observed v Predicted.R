###############################################################################
#####                                                                     #####
#####           Plotting Observed (minimum) subnivium temperature vs      #####
#####              predicted (minimum) subnvivium temperature             #####
#####                       (Model evaluation part 2)                     #####
###############################################################################

# This script averages predicted values (across 50-bootstrap samples) for 
# the 9 sites at which subnivium temperatures were measured from 
# December 1, 2016 through March 31, 2017.

# Note, we took a random sample of 30% of the data for each bootstrap, so 
# observations have different numbers of predictions going into the average.


# Clean workspace and load required packages
rm(list = ls() ) 
gc() #releases memory

library(ggplot2)
library(tidyverse)



#####################################
###                               ###
###         Data Loading          ###
###                               ###
#####################################

# Load the prediction dataframes for each treatment: 
# external (i.e., no greenhouse, house 0, house 3, and house 5)

setwd("H:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
house0 <- read.csv("House 0 - Prediction Dataframe_50 Samples.csv", header = TRUE)
house3 <- read.csv("House 3 - Prediction Dataframe_50 Samples.csv", header = TRUE)
house5 <- read.csv("House 5 - Prediction Dataframe_50 Samples.csv", header = TRUE)


#####################################
###                               ###
###     Summarize Predictions     ###
###                               ###
#####################################

### House 0 ###

house0$summarized.pred <- NA

# Summarize predictions
for(i in 1:length(house0$ID)) {
  house0$summarized.pred[i] <- 
    mean(house0[i, c(16:65)][is.na(house0[i, c(16:65)]) == FALSE])
  print(i)
}

## House 3 ###

house3$summarized.pred <- NA

# Summarize predictions
for(i in 1:length(house3$ID)) {
  house3$summarized.pred[i] <- 
    mean(house3[i, c(16:65)][is.na(house3[i, c(16:65)]) == FALSE])
  print(i)
}


## House 5 ###

house5$summarized.pred <- NA

# Summarize predictions
for(i in 1:length(house5$ID)) {
  house5$summarized.pred[i] <- 
    mean(house5[i, c(16:65)][is.na(house5[i, c(16:65)]) == FALSE])
  print(i)
}


#####################################
###                               ###
###           Plotting            ###
###                               ###
#####################################


h0.plot <- ggplot() +
  geom_point(data = house0, aes(x = min.subniv, y = summarized.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(lim = c(-19, 8.5), name = "Observed (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  scale_y_continuous(lim = c(-19, 8.5), name = "Predicted (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))


h3.plot <- ggplot() +
  geom_point(data = house3, aes(x = min.subniv, y = summarized.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(lim = c(-19, 8.5), name = "Observed (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  scale_y_continuous(lim = c(-19, 8.5), name = "Predicted (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) + 
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))


h5.plot <- ggplot() +
  geom_point(data = house5, aes(x = min.subniv, y = summarized.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(lim = c(-19, 8.5), name = "Observed (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  scale_y_continuous(lim = c(-19, 8.5), name = "Predicted (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))
