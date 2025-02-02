<<<<<<< HEAD
##################################################################################
##############    Bootstrapping Using 70/30 split with       ######################
##############  gbm.step for BRT models of minimum subniv    ######################
##############    temperature to determine the ideal # of    ######################
##############   predictions needed for the chosen settings  ######################
##############   of TC5, LR005, and BF65. Extracts RMSE &    ######################
#############   Deviance to compare the difference btw models#######################
###################################################################################

# Author: Kimberly Thompson

### This code helps determine the optimal number of predictions needed. 
### Diagnostics run from previous scripts help to determine that the best settings to move
### forward with are TC5, LR 0.005, and BF 0.65.
### The repetitive samples help to compare the different means and distributions to make an informed 
### decision of how many predictions to run to minimize computation time.

# Datasets created
# '50 Samples Deviance and RMSE.csv'
# '25 Samples Deviance and RMSE.csv'
# '75 Samples Deviance and RMSE.csv'
# '100 Samples Deviance and RMSE.csv'
# '150 Samples Deviance and RMSE.csv'
# '200 Samples Deviance and RMSE.csv'




# Divide the dataset into training and testing
# Build the BRT model


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

####### load relevant packages ###
library(lubridate)
library( ggplot2 ) #fancy plots
library( dplyr ) #data manipulation
library( dismo ) #Species distribution analysis package. Package used in Elith's 2017 tutorial
library( gbm) #Boosted regression tree package that is used in Elith's 2008 tutorial
########## end of package loading ###########

##############################################################
########    data loading            ##########################
##############################################################

path <- "00_Data/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")

subnivium<-as.data.frame(read.csv(file="Subniv Temps and Predictors_complete.csv"),
                         header = TRUE)
#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")

subnivium$Cover <- as.factor(subnivium$Cover)

############################################################
## Missing values are not allowed in the repsonse in BRTs ##
##   Therefore: pare down the master dataframe to include ##
##   only rows where there are values for subniv temp     ##
############################################################

subnivium <- subnivium[!is.na(subnivium$min.subniv),]


#### Model each treatment separately ####

# sub0 <- subnivium[subnivium$Treat == "0",]
# sub3 <- subnivium[subnivium$Treat == "3",]
# sub5 <- subnivium[subnivium$Treat == "5",]
subext <- subnivium[subnivium$Treat == "ext",]

#Add ID column to each dataframe to aid in subsetting
# sub0$ID <- seq(from=1, to=length(sub0$Date), by=1)
# sub3$ID <- seq(from=1, to=length(sub3$Date), by=1)
# sub5$ID <- seq(from=1, to=length(sub5$Date), by=1)
subext$ID <- seq(from=1, to=length(subext$Date), by=1)

rm(subnivium)

######################################################################################
####                              External Model                                 ####
######################################################################################


# 1: 50 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 20 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 20 mins

for (i in 1:50) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it50 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                            RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it50$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it50$Deviance <- NA
evaluation.df_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it50$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it50, "50 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it50)

################################################################################


# 2: 25 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 10 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 10 mins

for (i in 1:25) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it25 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it25$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it25$Deviance <- NA
evaluation.df_it25$RMSE <- NA


for (j in 16:40) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it25$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it25$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it25, "25 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it25)


################################################################################


# 3: 75 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 30 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 30 mins

for (i in 1:75) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it75 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it75$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it75$Deviance <- NA
evaluation.df_it75$RMSE <- NA


for (j in 16:90) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it75$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it75$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it75, "75 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it75)


################################################################################


# 4: 100 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 40 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 40 mins

for (i in 1:100) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it100 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it100$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it100$Deviance <- NA
evaluation.df_it100$RMSE <- NA


for (j in 16:115) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it100$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it100$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 100 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it100, "100 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it100)



################################################################################


# 5: 150 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 60 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 40 mins

for (i in 1:150) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it150 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                  RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it150$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it150$Deviance <- NA
evaluation.df_it150$RMSE <- NA


for (j in 16:165) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it150$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it150$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 150 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it150, "150 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it150)


################################################################################


# 6: 200 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 80 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 80 mins

for (i in 1:200) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it200 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                  RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it200$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it200$Deviance <- NA
evaluation.df_it200$RMSE <- NA


for (j in 16:215) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it200$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it200$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 200 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it200, "200 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it200)
=======
##################################################################################
##############    Bootstrapping Using 70/30 split with       ######################
##############  gbm.step for BRT models of minimum subniv    ######################
##############    temperature to determine the ideal # of    ######################
##############   predictions needed for the chosen settings  ######################
##############   of TC5, LR005, and BF65. Extracts RMSE &    ######################
#############   Deviance to compare the difference btw models#######################
###################################################################################

# Author: Kimberly Thompson

### This code helps determine the optimal number of predictions needed. 
### Diagnostics run from previous scripts help to determine that the best settings to move
### forward with are TC5, LR 0.005, and BF 0.65.
### The repetitive samples help to compare the different means and distributions to make an informed 
### decision of how many predictions to run to minimize computation time.

# Datasets created
# '50 Samples Deviance and RMSE.csv'
# '25 Samples Deviance and RMSE.csv'
# '75 Samples Deviance and RMSE.csv'
# '100 Samples Deviance and RMSE.csv'
# '150 Samples Deviance and RMSE.csv'
# '200 Samples Deviance and RMSE.csv'




# Divide the dataset into training and testing
# Build the BRT model


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory

####### load relevant packages ###
library(lubridate)
library( ggplot2 ) #fancy plots
library( dplyr ) #data manipulation
library( dismo ) #Species distribution analysis package. Package used in Elith's 2017 tutorial
library( gbm) #Boosted regression tree package that is used in Elith's 2008 tutorial
########## end of package loading ###########

##############################################################
########    data loading            ##########################
##############################################################

path <- "00_Data/"
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data")

subnivium<-as.data.frame(read.csv(file="Subniv Temps and Predictors_complete.csv"),
                         header = TRUE)
#Fix the dates
subnivium$Date<-gsub("/", "-", subnivium$Date)
subnivium$Date<-parse_date_time(subnivium$Date, c("%Y-%m-%d", "%m-%d-%Y"), tz = "US/Central")
subnivium$Date<-as.POSIXct(subnivium$Date, tz = "US/Central", format = "%Y-%m-%d")

subnivium$Cover <- as.factor(subnivium$Cover)

############################################################
## Missing values are not allowed in the repsonse in BRTs ##
##   Therefore: pare down the master dataframe to include ##
##   only rows where there are values for subniv temp     ##
############################################################

subnivium <- subnivium[!is.na(subnivium$min.subniv),]


#### Model each treatment separately ####

# sub0 <- subnivium[subnivium$Treat == "0",]
# sub3 <- subnivium[subnivium$Treat == "3",]
# sub5 <- subnivium[subnivium$Treat == "5",]
subext <- subnivium[subnivium$Treat == "ext",]

#Add ID column to each dataframe to aid in subsetting
# sub0$ID <- seq(from=1, to=length(sub0$Date), by=1)
# sub3$ID <- seq(from=1, to=length(sub3$Date), by=1)
# sub5$ID <- seq(from=1, to=length(sub5$Date), by=1)
subext$ID <- seq(from=1, to=length(subext$Date), by=1)

rm(subnivium)

######################################################################################
####                              External Model                                 ####
######################################################################################


# 1: 50 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 20 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 20 mins

for (i in 1:50) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it50 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                            RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it50$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it50$Deviance <- NA
evaluation.df_it50$RMSE <- NA


for (j in 16:65) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it50$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it50$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it50, "50 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it50)

################################################################################


# 2: 25 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 10 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 10 mins

for (i in 1:25) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it25 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it25$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it25$Deviance <- NA
evaluation.df_it25$RMSE <- NA


for (j in 16:40) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it25$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it25$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it25, "25 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it25)


################################################################################


# 3: 75 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 30 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 30 mins

for (i in 1:75) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it75 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it75$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it75$Deviance <- NA
evaluation.df_it75$RMSE <- NA


for (j in 16:90) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it75$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it75$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it75, "75 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it75)


################################################################################


# 4: 100 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 40 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 40 mins

for (i in 1:100) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it100 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                 RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it100$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it100$Deviance <- NA
evaluation.df_it100$RMSE <- NA


for (j in 16:115) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it100$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it100$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 100 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it100, "100 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it100)



################################################################################


# 5: 150 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 60 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 40 mins

for (i in 1:150) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it150 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                  RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it150$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it150$Deviance <- NA
evaluation.df_it150$RMSE <- NA


for (j in 16:165) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it150$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it150$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 150 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it150, "150 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it150)


################################################################################


# 6: 200 iterations
# TC = 5, LR = 0.005, BF = 0.65
# Takes about 80 minutes

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 80 mins

for (i in 1:200) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(modext, subext.test, n.trees = modext$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", i, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  print(i)
  
}

end.time <- Sys.time()
end.time - start.time


# calculate Root Mean Square Error and deviance for each column, being sure to
# Specify the BRT model settings that were used in the creation
# This way it's possible to compare different BRT models to try to reduce 
# overfititng.

# Create a dataframe to store the error values in
error.df <- data.frame(Error = numeric(length(subext.boot$Date)))

# Assign all values in the dataframe NA
error.df$Error <- NA

# Create a dataframe to store the deviance and RMSE in
evaluation.df_it200 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                  RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_it200$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_it200$Deviance <- NA
evaluation.df_it200$RMSE <- NA


for (j in 16:215) {   # prediction columns start at column 16
  for (i in 1:length(subext.boot$min.subniv)) { # row index
    # for (k in 1:sum(!is.na(subext.boot[,j]))) { # for each 323 predictions
    
    #Root Mean Square Error: represents the sample standard deviation of the 
    # differences between predicted values and observed values (residuals)
    # The units are the same as the quantity being estimated (degrees)
    # The closer to zero, the more accurate the predictions
    # Step 1: Calculate Error = actual - predicted
    # if the cell is not NA, find the difference btw observed and predicted
    
    if (!is.na(subext.boot[i,j])) {
      #Calculate the error and store it in a dataframe
      error.df$Error[i] <- subext.boot$min.subniv[i] - subext.boot[i,j]
      
    } else {
      
      if (!is.na(subext.boot[i,j])) {
        # Add NA to the dataframe
        error.df$Error[i] <- NA
      }
    }
    
  } # close the i loop
  
  # Calculate RMSE
  evaluation.df_it200$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_it200$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 200 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")
write.csv(evaluation.df_it200, "200 Samples Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_it200)
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
