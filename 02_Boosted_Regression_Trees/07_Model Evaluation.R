<<<<<<< HEAD
#####################################################################################
########   Calculating the predictive deviance and RMSE for the 50 bootstraps #######
########       for each model: external, House 0, House 3, House 5            #######
#######                     to show model evaluation                          #######
#####################################################################################

# Author: Kimberly Thompson

# This scripts executes model evaluation and evaluates the BRT models by calculating
# predictive deviance and RMSE for the 50 bootstrap samples for each model (one
# model for each treatment): external conditions, House 0 (+0C), House 3 (+3C),
# and House 5 (+5C)

# Datasets produced
# 'House 0 - Deviance and RMSE values.csv'
# 'House 3 - Deviance and RMSE values.csv'
# 'House 5 - Deviance and RMSE values.csv'

# Data used to produce tables
# Table S4
# Table S5




# External already calculated from earlier code determining the number of iterations needed
# but the other treatments need to be calculated (and displayed either as a table or a figure in the 
# main manuscript)

rm(list = ls() ) 
gc() #releases memory

####### load relevant packages ###
library(lubridate)
library( ggplot2 ) #fancy plots
library( dplyr )
library( tidyr )
library( dismo )
library( gbm )


##############################################################
########    data loading            ##########################
##############################################################

# Load in the deviance and RMSE values for the external model which have already been calculated
setwd("01_Analysis/Model_Parameters")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
external <- read.csv("50 Samples Deviance and RMSE.csv", header = TRUE)

# It reads in with 1000 rows even though there are only 50 with values so remove the other rows
external <- external[1:50,]


# Read in the other dataframes that have predicted values, but no deviance and RMSE calculated yet
path <- "01_Analysis/Spatial Predictions/Uncorrected"

H0pred <- read.csv(paste(path, "House 0/House 0 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)
H3pred <- read.csv(paste(path, "House 3/House 3 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)
H5pred <- read.csv(paste(path, "House 5/House 5 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)


# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
# H0pred <- read.csv("House 0 - Prediction Dataframe_50 Samples.csv", header = TRUE)
# H3pred <- read.csv("House 3 - Prediction Dataframe_50 Samples.csv", header = TRUE)
# H5pred <- read.csv("House 5 - Prediction Dataframe_50 Samples.csv", header = TRUE)

# For house 3, for some reason, there are two pred.1 columns - remove one
H3pred <- H3pred[, c(1:15, 17:66)]


###### I could do a loop to run these all at once, but I'm keeping them individually for now for oversight.


#####################
##     House 0   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H0pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H0_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                                 RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H0_it50$Iteration <- seq(from=1, to=50, by=1)
H0_it50$Deviance <- NA
H0_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H0pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H0pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H0pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H0pred$min.subniv[i] - H0pred[i,j]
      
    } else {
      
      if (!is.na(H0pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H0_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H0_it50$Deviance[j-15] <- 
    calc.deviance(obs=H0pred$min.subniv[!is.na(H0pred[,j])],
                  pred = H0pred[,j][!is.na(H0pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H0_it50, "House 0 - Deviance and RMSE values.csv", row.names = FALSE)


#####################
##     House 3   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H3pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H3_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H3_it50$Iteration <- seq(from=1, to=50, by=1)
H3_it50$Deviance <- NA
H3_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H3pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H3pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H3pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H3pred$min.subniv[i] - H3pred[i,j]
      
    } else {
      
      if (!is.na(H3pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H3_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H3_it50$Deviance[j-15] <- 
    calc.deviance(obs=H3pred$min.subniv[!is.na(H3pred[,j])],
                  pred = H3pred[,j][!is.na(H3pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H3_it50, "House 3 - Deviance and RMSE values.csv", row.names = FALSE)


#####################
##     House 3   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H3pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H3_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H3_it50$Iteration <- seq(from=1, to=50, by=1)
H3_it50$Deviance <- NA
H3_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H3pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H3pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H3pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H3pred$min.subniv[i] - H3pred[i,j]
      
    } else {
      
      if (!is.na(H3pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H3_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H3_it50$Deviance[j-15] <- 
    calc.deviance(obs=H3pred$min.subniv[!is.na(H3pred[,j])],
                  pred = H3pred[,j][!is.na(H3pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H3_it50, "House 3 - Deviance and RMSE values.csv", row.names = FALSE)



#####################
##     House 5   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H5pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H5_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H5_it50$Iteration <- seq(from=1, to=50, by=1)
H5_it50$Deviance <- NA
H5_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H5pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H5pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H5pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H5pred$min.subniv[i] - H5pred[i,j]
      
    } else {
      
      if (!is.na(H5pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H5_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H5_it50$Deviance[j-15] <- 
    calc.deviance(obs=H5pred$min.subniv[!is.na(H5pred[,j])],
                  pred = H5pred[,j][!is.na(H5pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H5_it50, "House 5 - Deviance and RMSE values.csv", row.names = FALSE)




#######################################################################################################
# Summarize the deviance and RMSE of each treatment and make a plot
#######################################################################################################

# # Add treatments to the columns of each and then put them together
# external$Treatment <- "external"
# H0_it50$Treatment <- "House 0"
# H3_it50$Treatment <- "House 3"
# H5_it50$Treatment <- "House 5"
# 
# total <- rbind(external, H0_it50, H3_it50, H5_it50)
# 
# # Summarize Deviance
# performance.summary <- Summarize(Deviance ~ Treatment, data = total)
# 
# # Summarize RMSE
# performance.summary2 <- Summarize(RMSE ~ Treatment, data = total)
# 
# # Write both 
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
# write.csv(performance.summary, "Deviance Summary by Treatment.csv", row.names = FALSE)
# write.csv(performance.summary2, "RMSE Summary by Treatment.csv", row.names = FALSE)








=======
#####################################################################################
########   Calculating the predictive deviance and RMSE for the 50 bootstraps #######
########       for each model: external, House 0, House 3, House 5            #######
#######                     to show model evaluation                          #######
#####################################################################################

# Author: Kimberly Thompson

# This scripts executes model evaluation and evaluates the BRT models by calculating
# predictive deviance and RMSE for the 50 bootstrap samples for each model (one
# model for each treatment): external conditions, House 0 (+0C), House 3 (+3C),
# and House 5 (+5C)

# Datasets produced
# 'House 0 - Deviance and RMSE values.csv'
# 'House 3 - Deviance and RMSE values.csv'
# 'House 5 - Deviance and RMSE values.csv'

# Data used to produce tables
# Table S4
# Table S5




# External already calculated from earlier code determining the number of iterations needed
# but the other treatments need to be calculated (and displayed either as a table or a figure in the 
# main manuscript)

rm(list = ls() ) 
gc() #releases memory

####### load relevant packages ###
library(lubridate)
library( ggplot2 ) #fancy plots
library( dplyr )
library( tidyr )
library( dismo )
library( gbm )


##############################################################
########    data loading            ##########################
##############################################################

# Load in the deviance and RMSE values for the external model which have already been calculated
setwd("01_Analysis/Model_Parameters")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
external <- read.csv("50 Samples Deviance and RMSE.csv", header = TRUE)

# It reads in with 1000 rows even though there are only 50 with values so remove the other rows
external <- external[1:50,]


# Read in the other dataframes that have predicted values, but no deviance and RMSE calculated yet
path <- "01_Analysis/Spatial Predictions/Uncorrected"

H0pred <- read.csv(paste(path, "House 0/House 0 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)
H3pred <- read.csv(paste(path, "House 3/House 3 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)
H5pred <- read.csv(paste(path, "House 5/House 5 - Prediction Dataframe_50 Samples.csv",
                         sep = ""), header = TRUE)


# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
# H0pred <- read.csv("House 0 - Prediction Dataframe_50 Samples.csv", header = TRUE)
# H3pred <- read.csv("House 3 - Prediction Dataframe_50 Samples.csv", header = TRUE)
# H5pred <- read.csv("House 5 - Prediction Dataframe_50 Samples.csv", header = TRUE)

# For house 3, for some reason, there are two pred.1 columns - remove one
H3pred <- H3pred[, c(1:15, 17:66)]


###### I could do a loop to run these all at once, but I'm keeping them individually for now for oversight.


#####################
##     House 0   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H0pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H0_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                                 RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H0_it50$Iteration <- seq(from=1, to=50, by=1)
H0_it50$Deviance <- NA
H0_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H0pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H0pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H0pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H0pred$min.subniv[i] - H0pred[i,j]
      
    } else {
      
      if (!is.na(H0pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H0_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H0_it50$Deviance[j-15] <- 
    calc.deviance(obs=H0pred$min.subniv[!is.na(H0pred[,j])],
                  pred = H0pred[,j][!is.na(H0pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H0_it50, "House 0 - Deviance and RMSE values.csv", row.names = FALSE)


#####################
##     House 3   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H3pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H3_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H3_it50$Iteration <- seq(from=1, to=50, by=1)
H3_it50$Deviance <- NA
H3_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H3pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H3pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H3pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H3pred$min.subniv[i] - H3pred[i,j]
      
    } else {
      
      if (!is.na(H3pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H3_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H3_it50$Deviance[j-15] <- 
    calc.deviance(obs=H3pred$min.subniv[!is.na(H3pred[,j])],
                  pred = H3pred[,j][!is.na(H3pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H3_it50, "House 3 - Deviance and RMSE values.csv", row.names = FALSE)


#####################
##     House 3   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H3pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H3_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H3_it50$Iteration <- seq(from=1, to=50, by=1)
H3_it50$Deviance <- NA
H3_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H3pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H3pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H3pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H3pred$min.subniv[i] - H3pred[i,j]
      
    } else {
      
      if (!is.na(H3pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H3_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H3_it50$Deviance[j-15] <- 
    calc.deviance(obs=H3pred$min.subniv[!is.na(H3pred[,j])],
                  pred = H3pred[,j][!is.na(H3pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H3_it50, "House 3 - Deviance and RMSE values.csv", row.names = FALSE)



#####################
##     House 5   ####
#####################

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(H5pred$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
H5_it50 <- data.frame(Iteration = integer(50), Deviance = numeric(50),
                      RMSE = numeric(50))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
H5_it50$Iteration <- seq(from=1, to=50, by=1)
H5_it50$Deviance <- NA
H5_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(H5pred$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(H5pred[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(H5pred[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- H5pred$min.subniv[i] - H5pred[i,j]
      
    } else {
      
      if (!is.na(H5pred[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  H5_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  H5_it50$Deviance[j-15] <- 
    calc.deviance(obs=H5pred$min.subniv[!is.na(H5pred[,j])],
                  pred = H5pred[,j][!is.na(H5pred[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the Deviance and RMSE CSV so that I don't have to do it again
setwd("01_Analysis/Model Evaluation")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
write.csv(H5_it50, "House 5 - Deviance and RMSE values.csv", row.names = FALSE)




#######################################################################################################
# Summarize the deviance and RMSE of each treatment and make a plot
#######################################################################################################

# # Add treatments to the columns of each and then put them together
# external$Treatment <- "external"
# H0_it50$Treatment <- "House 0"
# H3_it50$Treatment <- "House 3"
# H5_it50$Treatment <- "House 5"
# 
# total <- rbind(external, H0_it50, H3_it50, H5_it50)
# 
# # Summarize Deviance
# performance.summary <- Summarize(Deviance ~ Treatment, data = total)
# 
# # Summarize RMSE
# performance.summary2 <- Summarize(RMSE ~ Treatment, data = total)
# 
# # Write both 
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Deviance and RMSE for each treatment")
# write.csv(performance.summary, "Deviance Summary by Treatment.csv", row.names = FALSE)
# write.csv(performance.summary2, "RMSE Summary by Treatment.csv", row.names = FALSE)








>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
