###############################################################################
#####                                                                     #####
#####                 Examining whether any dependence exists             #####
#####                         in BRT model residuals                      #####
#####                       (Model evaluation part 2)                     #####
###############################################################################

# This script assesses whether their is dependence in the residuals, likely
# due to either spatial or temporal autocorrelation.

# Studies have shown that randomly selecting subsets of data to use as the 
# testing and training sets can lead to overconfident errors (i.e., lower
# errors than would be expected if the underlying structure of the data - 
# spatial or temporal, was accounted for)



# Clean workspace and load required packages
rm(list = ls() ) 
gc() #releases memory

library(tidyverse)

#####################################
###                               ###
###         Data Loading          ###
###                               ###
#####################################

# Load the prediction dataframes for each treatment: 
# external (i.e., no greenhouse, house 0, house 3, and house 5)

setwd("H:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
external <- read.csv("External - Prediction Dataframe_50 Samples.csv", header = TRUE)
house0 <- read.csv("House 0 - Prediction Dataframe_50 Samples.csv", header = TRUE)
house3 <- read.csv("House 3 - Prediction Dataframe_50 Samples.csv", header = TRUE)
house5 <- read.csv("House 5 - Prediction Dataframe_50 Samples.csv", header = TRUE)

# For house 3, for some reason, there are two pred.1 columns - remove one
house3 <- house3[, c(1:15, 17:66)]


#####################################
###                               ###
###     Calculate Residuals       ###
###                               ###
#####################################

# residual = observed - predicted

### External ###

# Loop through each of the 50 prediction columns
for (i in 1:50) {
  # Dynamically construct the column names for predictions and residuals
  pred_col <- paste0("preds_", i) 
  resid_col <- paste0("resid_", i)
  
  # Calculate residuals (observed - predicted) and store them in new columns
  external[[resid_col]] <- external$min.subniv - external[[pred_col]]
  
  print(i)
}

# summarize residuals
for(i in 1:length(external$ID)) {
  external$summarized.resid[i] <- 
    mean(external[i, c(66:115)][is.na(external[i, c(66:115)]) == FALSE])
  print(i)
}


### House 0 ###

# Loop through each of the 50 prediction columns
for (i in 1:50) {
  # Dynamically construct the column names for predictions and residuals
  pred_col <- paste0("preds_", i) 
  resid_col <- paste0("resid_", i)
  
  # Calculate residuals (observed - predicted) and store them in new columns
  house0[[resid_col]] <- house0$min.subniv - house0[[pred_col]]
  
  print(i)
}

# summarize residuals
for(i in 1:length(house0$ID)) {
  house0$summarized.resid[i] <- 
    mean(house0[i, c(66:115)][is.na(house0[i, c(66:115)]) == FALSE])
  print(i)
}


### House 3 ###

names(house3)[16] <- "preds_1"

# Loop through each of the 50 prediction columns
for (i in 1:50) {
  # Dynamically construct the column names for predictions and residuals
  pred_col <- paste0("preds_", i) 
  resid_col <- paste0("resid_", i)
  
  # Calculate residuals (observed - predicted) and store them in new columns
  house3[[resid_col]] <- house3$min.subniv - house3[[pred_col]]
  
  print(i)
}

# summarize residuals
for(i in 1:length(house3$ID)) {
  house3$summarized.resid[i] <- 
    mean(house3[i, c(66:115)][is.na(house3[i, c(66:115)]) == FALSE])
  print(i)
}


### House 5 ###

# Loop through each of the 50 prediction columns
for (i in 1:50) {
  # Dynamically construct the column names for predictions and residuals
  pred_col <- paste0("preds_", i) 
  resid_col <- paste0("resid_", i)
  
  # Calculate residuals (observed - predicted) and store them in new columns
  house5[[resid_col]] <- house5$min.subniv - house5[[pred_col]]
  
  print(i)
}

# summarize residuals
for(i in 1:length(house5$ID)) {
  house5$summarized.resid[i] <- 
    mean(house5[i, c(66:115)][is.na(house5[i, c(66:115)]) == FALSE])
  print(i)
}


#####################################
###                               ###
###     Summarize Predictions     ###
###                               ###
#####################################

### External ###

external$summarized.pred <- NA

# Summarize predictions
for(i in 1:length(external$ID)) {
  external$summarized.pred[i] <- 
    mean(external[i, c(16:65)][is.na(external[i, c(16:65)]) == FALSE])
  print(i)
}



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
###      Examine Dependence       ###
###                               ###
#####################################

acf(external$summarized.resid)
acf(house0$summarized.resid)
acf(house3$summarized.resid)
acf(house5$summarized.resid)


plot(external$summarized.pred, external$summarized.resid,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


plot(house0$summarized.pred, house0$summarized.resid,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

plot(house3$summarized.pred, house3$summarized.resid,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

plot(house5$summarized.pred, house5$summarized.resid,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


plot(external$summarized.resid, type = "l",
     xlab = "Observation Order", ylab = "Residuals",
     main = "Residuals Time Sequence Plot")

Box.test(external$summarized.resid, type = "Ljung-Box")
