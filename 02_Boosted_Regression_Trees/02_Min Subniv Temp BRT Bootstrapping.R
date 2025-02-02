<<<<<<< HEAD
##################################################################################
##############    Bootstrapping Using 70/30 split with       ######################
##############  gbm.step for BRT models of minimum subniv    ######################
##############    temperature. Extracts RMSE & Deviance      ######################
#############     to compare the difference btw models      #######################
###################################################################################

# Author: Kimberly Thompson

### This code helps determine the best model settings. Diagnostics run from previous
### scripts help to confine which combinations of settings to test, then these 
### repetitive samples help to compare the different models to make an informed 
### decision of which models to proceed with.

# Creates the following datasets with 1000 runs of each settings combination and 
# the associated deviance and RMSE
# 'TC3.LR01.BF55 Deviance and RMSE.csv'
# 'TC4.LR01.BF60 Deviance and RMSE.csv'
# 'TC5.LR005.BF65 Deviance and RMSE.csv'
# 'TC6.LR01.BF80 Deviance and RMSE.csv'
# 'TC7.LR005.BF80 Deviance and RMSE.csv'


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


# 1: TC = 3, LR = 0.01, BF = 0.55
# Takes about 3.78 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.78 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 3, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.55)
  
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
evaluation.df_tc3 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                            RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc3$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc3$Deviance <- NA
evaluation.df_tc3$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc3$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc3$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc3, "TC3.LR01.BF55 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc3)

################################################################################


# 2: TC = 4, LR = 0.01, BF = 0.60
# Takes about 3.75 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.75 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 4, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.60)
  
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
evaluation.df_tc4 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc4$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc4$Deviance <- NA
evaluation.df_tc4$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc4$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc4$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc4, "TC4.LR01.BF60 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc4)


################################################################################


# 3: TC = 5, LR = 0.005, BF = 0.65
# Takes about 6.29 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about  6.29hours

for (i in 1:1000) {
  
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
evaluation.df_tc5 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc5$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc5$Deviance <- NA
evaluation.df_tc5$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc5$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc5$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc5, "TC5.LR005.BF65 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc5)



################################################################################


# 4: TC = 6, LR = 0.01, BF = 0.80
# Takes about 3.9 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.9 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 6, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.80)
  
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
evaluation.df_tc6 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc6$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc6$Deviance <- NA
evaluation.df_tc6$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc6$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc6$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc6, "TC6.LR01.BF80 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc6)



################################################################################


# 5: TC = 7, LR = 0.005, BF = 0.80
# Takes about 6.79 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 6.79 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 7, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.80)
  
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
evaluation.df_tc7 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc7$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc7$Deviance <- NA
evaluation.df_tc7$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc7$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc7$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
#  setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc7, "TC7.LR005.BF80 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc7)

=======
##################################################################################
##############    Bootstrapping Using 70/30 split with       ######################
##############  gbm.step for BRT models of minimum subniv    ######################
##############    temperature. Extracts RMSE & Deviance      ######################
#############     to compare the difference btw models      #######################
###################################################################################

# Author: Kimberly Thompson

### This code helps determine the best model settings. Diagnostics run from previous
### scripts help to confine which combinations of settings to test, then these 
### repetitive samples help to compare the different models to make an informed 
### decision of which models to proceed with.

# Creates the following datasets with 1000 runs of each settings combination and 
# the associated deviance and RMSE
# 'TC3.LR01.BF55 Deviance and RMSE.csv'
# 'TC4.LR01.BF60 Deviance and RMSE.csv'
# 'TC5.LR005.BF65 Deviance and RMSE.csv'
# 'TC6.LR01.BF80 Deviance and RMSE.csv'
# 'TC7.LR005.BF80 Deviance and RMSE.csv'


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


# 1: TC = 3, LR = 0.01, BF = 0.55
# Takes about 3.78 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.78 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 3, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.55)
  
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
evaluation.df_tc3 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                            RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc3$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc3$Deviance <- NA
evaluation.df_tc3$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc3$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc3$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc3, "TC3.LR01.BF55 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc3)

################################################################################


# 2: TC = 4, LR = 0.01, BF = 0.60
# Takes about 3.75 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.75 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 4, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.60)
  
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
evaluation.df_tc4 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc4$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc4$Deviance <- NA
evaluation.df_tc4$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc4$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc4$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc4, "TC4.LR01.BF60 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc4)


################################################################################


# 3: TC = 5, LR = 0.005, BF = 0.65
# Takes about 6.29 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about  6.29hours

for (i in 1:1000) {
  
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
evaluation.df_tc5 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc5$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc5$Deviance <- NA
evaluation.df_tc5$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc5$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc5$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc5, "TC5.LR005.BF65 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc5)



################################################################################


# 4: TC = 6, LR = 0.01, BF = 0.80
# Takes about 3.9 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 3.9 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 6, learning.rate = 0.01,
                     max.trees = 30000, bag.fraction = 0.80)
  
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
evaluation.df_tc6 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc6$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc6$Deviance <- NA
evaluation.df_tc6$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc6$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc6$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc6, "TC6.LR01.BF80 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc6)



################################################################################


# 5: TC = 7, LR = 0.005, BF = 0.80
# Takes about 6.79 hours

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions

subext.boot <- subext


# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points

start.time <- Sys.time() # Takes about 6.79 hours

for (i in 1:1000) {
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  subext.train <- subext[ -t.parows, ]
  
  # Create testing dataset:
  subext.test <- subext[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  
  modext <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 7, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.80)
  
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
evaluation.df_tc7 <- data.frame(Iteration = integer(1000), Deviance = numeric(1000),
                                RMSE = numeric(1000))

# Assign all values in the dataframe NA, except for iteration (= # of bootstraps)
evaluation.df_tc7$Iteration <- seq(from=1, to=1000, by=1)
evaluation.df_tc7$Deviance <- NA
evaluation.df_tc7$RMSE <- NA


for (j in 16:1015) {   # prediction columns start at column 16
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
  evaluation.df_tc7$RMSE[j-15] <- 
    sqrt(mean(error.df$Error^2, na.rm=TRUE)) 
  
  #Calculate Deviance
  evaluation.df_tc7$Deviance[j-15] <- 
    calc.deviance(obs=subext.boot$min.subniv[!is.na(subext.boot[,j])],
                  pred = subext.boot[,j][!is.na(subext.boot[,j])],
                  family = "gaussian", calc.mean=TRUE) 
  
  print(j)
}

# Write the csv that contains the 1000 runs with their deviance and RMSE
setwd("01_Analysis/Model_Parameters/")
#  setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")
write.csv(evaluation.df_tc7, "TC7.LR005.BF80 Deviance and RMSE.csv", row.names = FALSE)

# Clean up workspace for next run
rm(modext, subext.train, subext.boot, subext.test, temp.pred)
rm(error.df, evaluation.df_tc7)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
