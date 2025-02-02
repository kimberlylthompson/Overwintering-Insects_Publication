<<<<<<< HEAD
#####################################################################
######                                                    ###########
######      Boosted Regression Tree Model Predictions     ###########
######                                                    ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code runs a boosted regression tree model (for each treatment:
# external, +0, +3, +5) with the selected settings based on previous 
# scripts and then for each treatment, using the rasters of the 
# spatial predictions makes predictions with 50 iterations for each
# treatment/day combination.

# Note running on a local machine takes many days.

# Rasters produced:
# "X12.01.2016_ext_sample1.tif"
# "X12.01.2016_H0_sample1.tif"
# "X12.01.2016_H3_sample1.tif"
# "X12.01.2016_H5_sample1.tif"

# for each day from 12.01.2016 through 03.31.2017
# and for sample1 through sample50





# These predictions are based on BRTs that include a house-specific measure of snow density
# derived from our own depth measurements and SNODAS external density.

# Using the following predictors (names must be the same as in the BRT model)
# Cover - categorical raster
# Tairmax - continuous raster
# Tairmin - continuous raster
# Snowmed - continous raster
# Density - continous raster
# Wind - continuous raster

# Predicting to the following models
# External
# House 0
# House 3
# House 5

# Predicting each day from Dec 1 - March 31 (121 days) and 
# 50 times for each day (# of bootstrap samples) 

# So each loop will first run the BRT model, then use it to predict


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(dismo)
library(gbm)

##############################################################
########    data loading            ##########################
##############################################################

setwd("00_Data")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data")
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

sub0 <- subnivium[subnivium$Treat == "0",]
sub3 <- subnivium[subnivium$Treat == "3",]
sub5 <- subnivium[subnivium$Treat == "5",]
subext <- subnivium[subnivium$Treat == "ext",]

#Add ID column to each dataframe to aid in subsetting
sub0$ID <- seq(from=1, to=length(sub0$Date), by=1)
sub3$ID <- seq(from=1, to=length(sub3$Date), by=1)
sub5$ID <- seq(from=1, to=length(sub5$Date), by=1)
subext$ID <- seq(from=1, to=length(subext$Date), by=1)

rm(subnivium)





######################################################################################
####            SETTING UP THE CATEGORICAL PREDICTOR                            ####
######################################################################################

# Read in the land cover raster
# Set directory to where land cover raster resides
setwd("01_Analysis/Spatial Predictors")
# setwd("G:/My Drive/Ch 2 Climate/Analysis/Land Cover")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Land Cover")
Cover <- raster::raster( "NLCD_Great Lakes Area_reclassified_1kmresamp.tif" )

# Convert it to a categorical raster
Cover <- as.factor(Cover)
is.factor(Cover)

x <- levels(Cover)[[1]]

x

x$code <- c("Dec", "Con", "Open", "Urban", "Water", "Unclassified")

levels(Cover) <- x

levels(Cover)

Cover


# Make a list of the predictor stacks
path <- "01_Analysis/Spatial Predictors/Daily Stacks"
# path <- "G:/My Drive/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth/"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth/"
predictor.list <- list.files(path, pattern = "X")

# Predictor stacks will include the following layers: "Wind", "Tairmax", "Tairmin", "Snowmed", "density"
# from the following sources:
# WIND - NARR WIND
# SNOWMED - SNODAS
# TAIR MAX - DAYMET
# TAIR MIN - DAYMET
# DENSITY - SNODAS (derived from SWE and depth)


######################################################################################
####                          EXTERNAL SUBNIVIUM                                  ####
######################################################################################

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions
subext.boot <- subext

# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_ext <- Sys.time() # Takes about 8.9 days

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
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
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    # predictors <- terra :: rast(predictor.list[[i]])
    
    # Convert Cover to terra
    # Cover <- terra :: rast(Cover)
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    # predictors <- c(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    ext_predict <- raster :: predict(predictors, modext, n.trees=modext$gbm.call$best.trees,
                           type="response")
    
    ext_predict <- terra :: predict(predictors, modext, n.trees=modext$gbm.call$best.trees,
                                     type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/External")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(ext_predict, paste(partialname, "_ext", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_ext <- Sys.time()
end_time_ext - start_time_ext

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(subext.boot, "External - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

# Clean up workspace
rm(subext, subext.boot, subext.test, subext.train, ext_predict, modext,
   predictors, temp.pred, end_time_ext, i, j, M, partialname, predcall, preds,
   t.parows, TP, start_time_ext)


######################################################################################
####                                    HOUSE 0                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub0.boot <- sub0

# Total number of observations:
M <- max( sub0$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_0 <- Sys.time() # Takes about 7.5 days

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub0.train <- sub0[ -t.parows, ]
  
  # Create testing dataset:
  sub0.test <- sub0[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod0 <- gbm.step(data=sub0.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod0, sub0.test, n.trees = mod0$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub0.boot dataframe by matching ID#s
  sub0.boot <- merge(sub0.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H0_predict <- raster :: predict(predictors, mod0, n.trees=mod0$gbm.call$best.trees,
                           type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 0")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 0")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H0_predict, paste(partialname, "_H0", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_0 <- Sys.time()
end_time_0 - start_time_0

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 0")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub0.boot, "House 0 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

# Clean up workspace
rm(sub0, sub0.boot, sub0.test, sub0.train, H0_predict, mod0,
   predictors, temp.pred, end_time_0, i, j, M, partialname, predcall, preds,
   t.parows, TP, start_time_0)

######################################################################################
####                                    HOUSE 3                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub3.boot <- sub3

# Total number of observations:
M <- max( sub3$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_3 <- Sys.time() # Takes about 7.03

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub3.train <- sub3[ -t.parows, ]
  
  # Create testing dataset:
  sub3.test <- sub3[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod3 <- gbm.step(data=sub3.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                   max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod3, sub3.test, n.trees = mod3$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub3.boot dataframe by matching ID#s
  sub3.boot <- merge(sub3.boot, temp.pred[, c("ID", predcall)],
                     by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H3_predict <- raster :: predict(predictors, mod3, n.trees=mod3$gbm.call$best.trees,
                           type="response")
    
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 3")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H3_predict, paste(partialname, "_H3", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_3 <- Sys.time()
end_time_3 - start_time_3

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub3.boot, "House 3 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)




######################################################################################
####                                    HOUSE 5                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub5.boot <- sub5

# Total number of observations:
M <- max( sub5$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time <- Sys.time() # Takes about 10 days

# First part of loop - Build BRT model from 70% of the data

for(j in 22:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub5.train <- sub5[ -t.parows, ]
  
  # Create testing dataset:
  sub5.test <- sub5[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod5 <- gbm.step(data=sub5.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                   max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod5, sub5.test, n.trees = mod5$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub5.boot dataframe by matching ID#s
  sub5.boot <- merge(sub5.boot, temp.pred[, c("ID", predcall)],
                     by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H5_predict <- raster :: predict(predictors, mod5, n.trees=mod5$gbm.call$best.trees,
                          type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 5")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H5_predict, paste(partialname, "_H5", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time <- Sys.time()
end_time - start_time

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub5.boot_full, "House 5 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

=======
#####################################################################
######                                                    ###########
######      Boosted Regression Tree Model Predictions     ###########
######                                                    ###########
######                                                    ###########
#####################################################################

# Author: Kimberly Thompson

# This code runs a boosted regression tree model (for each treatment:
# external, +0, +3, +5) with the selected settings based on previous 
# scripts and then for each treatment, using the rasters of the 
# spatial predictions makes predictions with 50 iterations for each
# treatment/day combination.

# Note running on a local machine takes many days.

# Rasters produced:
# "X12.01.2016_ext_sample1.tif"
# "X12.01.2016_H0_sample1.tif"
# "X12.01.2016_H3_sample1.tif"
# "X12.01.2016_H5_sample1.tif"

# for each day from 12.01.2016 through 03.31.2017
# and for sample1 through sample50





# These predictions are based on BRTs that include a house-specific measure of snow density
# derived from our own depth measurements and SNODAS external density.

# Using the following predictors (names must be the same as in the BRT model)
# Cover - categorical raster
# Tairmax - continuous raster
# Tairmin - continuous raster
# Snowmed - continous raster
# Density - continous raster
# Wind - continuous raster

# Predicting to the following models
# External
# House 0
# House 3
# House 5

# Predicting each day from Dec 1 - March 31 (121 days) and 
# 50 times for each day (# of bootstrap samples) 

# So each loop will first run the BRT model, then use it to predict


########## clean workspace and load required packages ####################
###########################################################################
#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
#set working dir() 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(dismo)
library(gbm)

##############################################################
########    data loading            ##########################
##############################################################

setwd("00_Data")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data")
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

sub0 <- subnivium[subnivium$Treat == "0",]
sub3 <- subnivium[subnivium$Treat == "3",]
sub5 <- subnivium[subnivium$Treat == "5",]
subext <- subnivium[subnivium$Treat == "ext",]

#Add ID column to each dataframe to aid in subsetting
sub0$ID <- seq(from=1, to=length(sub0$Date), by=1)
sub3$ID <- seq(from=1, to=length(sub3$Date), by=1)
sub5$ID <- seq(from=1, to=length(sub5$Date), by=1)
subext$ID <- seq(from=1, to=length(subext$Date), by=1)

rm(subnivium)





######################################################################################
####            SETTING UP THE CATEGORICAL PREDICTOR                            ####
######################################################################################

# Read in the land cover raster
# Set directory to where land cover raster resides
setwd("01_Analysis/Spatial Predictors")
# setwd("G:/My Drive/Ch 2 Climate/Analysis/Land Cover")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Land Cover")
Cover <- raster::raster( "NLCD_Great Lakes Area_reclassified_1kmresamp.tif" )

# Convert it to a categorical raster
Cover <- as.factor(Cover)
is.factor(Cover)

x <- levels(Cover)[[1]]

x

x$code <- c("Dec", "Con", "Open", "Urban", "Water", "Unclassified")

levels(Cover) <- x

levels(Cover)

Cover


# Make a list of the predictor stacks
path <- "01_Analysis/Spatial Predictors/Daily Stacks"
# path <- "G:/My Drive/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth/"
# path <- "L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth/"
predictor.list <- list.files(path, pattern = "X")

# Predictor stacks will include the following layers: "Wind", "Tairmax", "Tairmin", "Snowmed", "density"
# from the following sources:
# WIND - NARR WIND
# SNOWMED - SNODAS
# TAIR MAX - DAYMET
# TAIR MIN - DAYMET
# DENSITY - SNODAS (derived from SWE and depth)


######################################################################################
####                          EXTERNAL SUBNIVIUM                                  ####
######################################################################################

# Create a new dataframe keeping all the information from subext, in which
# to store the bootstrapped predictions
subext.boot <- subext

# Total number of observations:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_ext <- Sys.time() # Takes about 8.9 days

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
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
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the subext.boot dataframe by matching ID#s
  subext.boot <- merge(subext.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    # predictors <- terra :: rast(predictor.list[[i]])
    
    # Convert Cover to terra
    # Cover <- terra :: rast(Cover)
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    # predictors <- c(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    ext_predict <- raster :: predict(predictors, modext, n.trees=modext$gbm.call$best.trees,
                           type="response")
    
    ext_predict <- terra :: predict(predictors, modext, n.trees=modext$gbm.call$best.trees,
                                     type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/External")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(ext_predict, paste(partialname, "_ext", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_ext <- Sys.time()
end_time_ext - start_time_ext

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/External")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(subext.boot, "External - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

# Clean up workspace
rm(subext, subext.boot, subext.test, subext.train, ext_predict, modext,
   predictors, temp.pred, end_time_ext, i, j, M, partialname, predcall, preds,
   t.parows, TP, start_time_ext)


######################################################################################
####                                    HOUSE 0                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub0.boot <- sub0

# Total number of observations:
M <- max( sub0$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_0 <- Sys.time() # Takes about 7.5 days

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub0.train <- sub0[ -t.parows, ]
  
  # Create testing dataset:
  sub0.test <- sub0[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod0 <- gbm.step(data=sub0.train, gbm.x = c(6:11), gbm.y = 14,
                     family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                     max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod0, sub0.test, n.trees = mod0$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub0.boot dataframe by matching ID#s
  sub0.boot <- merge(sub0.boot, temp.pred[, c("ID", predcall)],
                       by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H0_predict <- raster :: predict(predictors, mod0, n.trees=mod0$gbm.call$best.trees,
                           type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 0")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 0")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H0_predict, paste(partialname, "_H0", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_0 <- Sys.time()
end_time_0 - start_time_0

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 0")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub0.boot, "House 0 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

# Clean up workspace
rm(sub0, sub0.boot, sub0.test, sub0.train, H0_predict, mod0,
   predictors, temp.pred, end_time_0, i, j, M, partialname, predcall, preds,
   t.parows, TP, start_time_0)

######################################################################################
####                                    HOUSE 3                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub3.boot <- sub3

# Total number of observations:
M <- max( sub3$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time_3 <- Sys.time() # Takes about 7.03

# First part of loop - Build BRT model from 70% of the data

for(j in 1:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub3.train <- sub3[ -t.parows, ]
  
  # Create testing dataset:
  sub3.test <- sub3[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod3 <- gbm.step(data=sub3.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                   max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod3, sub3.test, n.trees = mod3$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub3.boot dataframe by matching ID#s
  sub3.boot <- merge(sub3.boot, temp.pred[, c("ID", predcall)],
                     by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H3_predict <- raster :: predict(predictors, mod3, n.trees=mod3$gbm.call$best.trees,
                           type="response")
    
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 3")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H3_predict, paste(partialname, "_H3", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time_3 <- Sys.time()
end_time_3 - start_time_3

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 3")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub3.boot, "House 3 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)




######################################################################################
####                                    HOUSE 5                                   ####
######################################################################################

# Create a new dataframe keeping all the information from sub0, in which
# to store the bootstrapped predictions
sub5.boot <- sub5

# Total number of observations:
M <- max( sub5$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points


start_time <- Sys.time() # Takes about 10 days

# First part of loop - Build BRT model from 70% of the data

for(j in 22:50) { # the number of bootstrap samples
  
  # Select which rows to use for testing:
  t.parows <- sample( x = 1:M, size = TP, replace = FALSE )
  
  # Remaining rows will be used for training the model:
  sub5.train <- sub5[ -t.parows, ]
  
  # Create testing dataset:
  sub5.test <- sub5[ t.parows, ]
  
  # Run the BRT model (setting identified in prior script)
  mod5 <- gbm.step(data=sub5.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                   max.trees = 30000, bag.fraction = 0.65)
  
  # Make the vector of predictions from the model.
  preds <- predict.gbm(mod5, sub5.test, n.trees = mod5$gbm.call$best.trees,
                       type = "response")
  
  # Make a temporary dataframe that has the ID number and the prediction
  temp.pred <- data.frame(cbind(t.parows, preds))
  
  #Change column name from t.parows to ID
  predcall <- paste("preds", "_", j, sep = "")
  names(temp.pred) <- c("ID", predcall)
  
  # Add the predicted values to the sub5.boot dataframe by matching ID#s
  sub5.boot <- merge(sub5.boot, temp.pred[, c("ID", predcall)],
                     by = "ID", all.x = TRUE)
  
  # Spatial Predictions
  for (i in 1:length(predictor.list)) {
    
    # Read in the continuous predictors
    setwd(path)
    # setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/Final Predictor Data/Stacks_1 km res_snodas for depth")
    predictors <- stack(predictor.list[[i]])
    
    # Add the Cover layer to the stack
    predictors <- addLayer(predictors, Cover)
    
    # Rename the layers in the predictor list (order known from the code for creating raster stack for each day)
    names(predictors) <- c("Wind", "Tairmax", "Tairmin", "Snowmed", "density", "Cover")
    
    # Make a prediction raster from the BRT model
    H5_predict <- raster :: predict(predictors, mod5, n.trees=mod5$gbm.call$best.trees,
                          type="response")
    
    # Write resulting raster predictions
    setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
    
    # Write the resulting raster prediction to gaia directory
    # setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/House 5")
    
    # To get the file name, split the string of the predictor list
    partialname <- substr(unlist(predictor.list[[i]]), 1, 11)
    
    writeRaster(H5_predict, paste(partialname, "_H5", "_sample", j, ".tif", sep = ""), overwrite=TRUE)
    
    print(i)
    
  }
  
  print(j)
}

end_time <- Sys.time()
end_time - start_time

# Write the bootstrap dataframe with the predicted values
setwd("01_Analysis/Spatial Predictions/Uncorrected/House 5")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions")
write.csv(sub5.boot_full, "House 5 - Prediction Dataframe_50 Samples.csv", row.names = FALSE)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
