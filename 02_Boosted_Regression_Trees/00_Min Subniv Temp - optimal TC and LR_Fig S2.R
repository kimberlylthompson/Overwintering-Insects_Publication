<<<<<<< HEAD
####################################################################################
###### Analysis of winter 2016-17, December - March min subnivium temperature ######
#####     data using Boosted Regression Trees -      #######       
#####################################################################################

# Author: Kimberly Thompson

# This script determines the optimal tree complexity and 
# learning rate for BRT models (which will predict the ground temperatures
# across the winter season)


# Plots created
# Figure S2





# Response: Daily minimum subnivium temperature

# Predictors: Min and Max air temp, median snow depth, mean snow density, wind speed
#             land cover

# Density: a site & house-specific density that comes from
# our own measures of depth, and SNODAS's measure of external density. 
# Explanation of its derivation in the supplementary materials in chapter 2


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

subnivium<-read.csv(paste(path, "Subniv Temps and Predictors_complete.csv",
                          sep = ""),
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


######################################################################################
####                              Model Explanation                               ####
######################################################################################

# Within each treatment dataset, two sets of data are necessary,
# one for model training (building) and one for model testing (evaluation).
# Starting with approximately 70% of data for model building and 30% for model testing.

## First, identify the optimal number of trees. ##

## As per the article by Elith and Leathwick ##
## found here, https://cran.r-project.org/web/packages/dismo/vignettes/brt.pdf, look at the ##
## total number of observations you have and the total number of predictors, and decide whether ##
## you think there is enough data to model interactions with reasonable complexity. They say that if yes, ##
## then a learning rate of 0.01 could be a reasonable starting point.                       ##

# data = training dataset, gbm.x = indices of names of predictor columns in data
# gbm.y = index of response variable
# family = use Gaussian because histograms of subnivium temps show normal distributions
# tree complexity = set the complexity of individual trees (models interactions --> 2 = two-way; 3 = three-way)
# learning rate = sets the weight applied to individual trees (lower the learning rate, the better performance in model building)
# bag fraction = controls the stochasticity in the model, bag.frac = 1 results in deterministic model, optimal is between 0.5-0.75 for pres/abs data

# Keep in mind these models are stochastic and so will be slightly different each time you run them unless you make them deterministic
# by using a bag fraction of 1. 

# Goal is the joint optimization of the number of trees, the learning rate, and tree complexity
# (and minimal predictive deviance)

# IMPORTANT: Splitting the data into training and testing data is necessary in order to more broadly explore
# the optimal combination of number of trees, learning rate, and tree complexity
# Systematically alter tc, lr, and the bag fraction and compare the results.

# Since the external treatment has the most complexity associated with it, do this systematic approach on
# this treatment, and then apply that to the other treatments where things like wind are minimized.



######################################################################################
####  FINDING THE IDEAL SETTINGS FOR THE MODEL USING THE EXTERNAL PROBE DATASET  ####
####                           For MIN SUBNIVIUM TEMPERATURE                     ####
######################################################################################

#  1. Determine parameters using gbm.step with 70/30 training/testing


#####  Split data into training and testing datasets with approximately 70/30 split

# Total number of observations in subnivium data:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points
# Select which rows to use for testing for our subnivium/absence data:
t.parows <- sample( x = 1:M, size = TP, replace = FALSE )

subext.train <- subext[ -t.parows, ]

# View
head( subext.train ); dim( subext.train )

# Create testing dataset:
subext.test <- subext[ t.parows, ]

#### Systematic testing of different numbers of trees and learning rates

# Create a list of learning rates to test
lr <- c(0.05, 0.01, 0.005, 0.001, 0.0005)
tree.list <- seq(100, 30000, by=100)

# Depending on the number of trees fitted, predicting with more than this number will throw
# a warning message that 'Number of trees not specified or exceeded number fit so far.'
# This is fine - for trees above the ideal number it just repeats the predictions using the 
# optimal number of trees. This is what I want to happen - if it doesn't it means that the
# test needs more trees.


################ TC = 1 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc1<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 1, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the model - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc1[,i])) {
    deviance.tc1[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc1, na.rm=TRUE)
which(deviance.tc1$lr05 == min(deviance.tc1, na.rm=TRUE))
deviance.tc1[5,]
#Minimum is 4.85  at 500 trees at lr of 0.05

# Graph the results of the learning rate comparison for tc=1
tc1 <- ggplot() +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc1[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(a)",
           size=7) +
  annotate("text", x = 8000, y = 9.5, label = "tc = 1",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9.5, label = "min=4.85, lr=0.05",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 9, label = "nt=500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc1 nt30000.jpg", plot=tc1, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 2 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc2<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 2, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc2[,i])) {
    deviance.tc2[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc2, na.rm=TRUE)
which(deviance.tc2$lr05 == min(deviance.tc2, na.rm=TRUE))
deviance.tc2[9,]
#Minimum is 2.96 at 900 trees at lr of 0.05

# Graph the results of the learning rate comparison for tc=2
tc2 <- ggplot() +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc2[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=900, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(b)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 2",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.96, lr=0.05",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=900",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc2 nt30000.jpg", plot=tc2, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 3 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc3<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 3, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc3[,i])) {
    deviance.tc3[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc3, na.rm=TRUE)
which(deviance.tc3$lr01 == min(deviance.tc3, na.rm=TRUE))
deviance.tc3[31,]
#Minimum is 2.64 at 3100 trees at lr of 0.01


# Graph the results of the learning rate comparison for tc=3
tc3 <- ggplot() +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc3[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=3100, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(c)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 3",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.64, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=3100",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc3 nt30000.jpg", plot=tc3, device = "jpeg",
       width=7, height=5, dpi=300)


################ TC = 4 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc4<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 4, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc4[,i])) {
    deviance.tc4[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc4, na.rm=TRUE)
which(deviance.tc4$lr01 == min(deviance.tc4, na.rm=TRUE))
deviance.tc4[5,]
#Minimum is 2.46 at 2500 trees at lr of 0.01


# Graph the results of the learning rate comparison for tc=4
tc4 <- ggplot() +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc4[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=2500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(d)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 4",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.46, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=2500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc4 nt30000.jpg", plot=tc4, device = "jpeg",
       width=7, height=5, dpi=300)


################ TC = 5 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc5<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc5[,i])) {
    deviance.tc5[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc5, na.rm=TRUE)
which(deviance.tc5$lr005 == min(deviance.tc5, na.rm=TRUE))
deviance.tc5[4,]
#Minimum is 2.35 at 3500 trees at lr of 0.005

# Graph the results of the learning rate comparison for tc=5
tc5 <- ggplot() +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc5[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=3500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(e)",
           size=7) +
  annotate("text", x = 6500, y = 9, label = "tc = 5",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.35, lr=0.005",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=3500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc5 nt30000.jpg", plot=tc5, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 6 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc6<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 6, learning.rate = lr[i],
                   n.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc6[,i])) {
    deviance.tc6[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc6, na.rm=TRUE)
which(deviance.tc6$lr01 == min(deviance.tc6, na.rm=TRUE))
deviance.tc6[52,]
#Minimum is 2.2 at 5200 trees at lr of 0.01

# Graph the results of the learning rate comparison for tc=6
tc6 <- ggplot() +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc6[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=5200, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(f)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 6",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.20, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=5200",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc6 nt30000.jpg", plot=tc6, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 7 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc7<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 7, learning.rate = lr[i],
                   n.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc7[,i])) {
    deviance.tc7[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc7, na.rm=TRUE)
which(deviance.tc7$lr005 == min(deviance.tc7, na.rm=TRUE))
deviance.tc7[112,]
#Minimum is 2.15 at 11200 trees at lr of 0.005

# Graph the results of the learning rate comparison for tc=7
tc7 <- ggplot() +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc7[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=11200, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(g)",
           size=7) +
  annotate("text", x = 7500, y = 9, label = "tc = 7",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.15, lr=0.005",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=11200",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc7 nt30000.jpg", plot=tc7, device = "jpeg",
       width=7, height=5, dpi=300)



=======
####################################################################################
###### Analysis of winter 2016-17, December - March min subnivium temperature ######
#####     data using Boosted Regression Trees -      #######       
#####################################################################################

# Author: Kimberly Thompson

# This script determines the optimal tree complexity and 
# learning rate for BRT models (which will predict the ground temperatures
# across the winter season)


# Plots created
# Figure S2





# Response: Daily minimum subnivium temperature

# Predictors: Min and Max air temp, median snow depth, mean snow density, wind speed
#             land cover

# Density: a site & house-specific density that comes from
# our own measures of depth, and SNODAS's measure of external density. 
# Explanation of its derivation in the supplementary materials in chapter 2


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

subnivium<-read.csv(paste(path, "Subniv Temps and Predictors_complete.csv",
                          sep = ""),
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


######################################################################################
####                              Model Explanation                               ####
######################################################################################

# Within each treatment dataset, two sets of data are necessary,
# one for model training (building) and one for model testing (evaluation).
# Starting with approximately 70% of data for model building and 30% for model testing.

## First, identify the optimal number of trees. ##

## As per the article by Elith and Leathwick ##
## found here, https://cran.r-project.org/web/packages/dismo/vignettes/brt.pdf, look at the ##
## total number of observations you have and the total number of predictors, and decide whether ##
## you think there is enough data to model interactions with reasonable complexity. They say that if yes, ##
## then a learning rate of 0.01 could be a reasonable starting point.                       ##

# data = training dataset, gbm.x = indices of names of predictor columns in data
# gbm.y = index of response variable
# family = use Gaussian because histograms of subnivium temps show normal distributions
# tree complexity = set the complexity of individual trees (models interactions --> 2 = two-way; 3 = three-way)
# learning rate = sets the weight applied to individual trees (lower the learning rate, the better performance in model building)
# bag fraction = controls the stochasticity in the model, bag.frac = 1 results in deterministic model, optimal is between 0.5-0.75 for pres/abs data

# Keep in mind these models are stochastic and so will be slightly different each time you run them unless you make them deterministic
# by using a bag fraction of 1. 

# Goal is the joint optimization of the number of trees, the learning rate, and tree complexity
# (and minimal predictive deviance)

# IMPORTANT: Splitting the data into training and testing data is necessary in order to more broadly explore
# the optimal combination of number of trees, learning rate, and tree complexity
# Systematically alter tc, lr, and the bag fraction and compare the results.

# Since the external treatment has the most complexity associated with it, do this systematic approach on
# this treatment, and then apply that to the other treatments where things like wind are minimized.



######################################################################################
####  FINDING THE IDEAL SETTINGS FOR THE MODEL USING THE EXTERNAL PROBE DATASET  ####
####                           For MIN SUBNIVIUM TEMPERATURE                     ####
######################################################################################

#  1. Determine parameters using gbm.step with 70/30 training/testing


#####  Split data into training and testing datasets with approximately 70/30 split

# Total number of observations in subnivium data:
M <- max( subext$ID )

# Define number of data points to use for testing:
TP <- round(M*0.30) # ~30% of data points
# Select which rows to use for testing for our subnivium/absence data:
t.parows <- sample( x = 1:M, size = TP, replace = FALSE )

subext.train <- subext[ -t.parows, ]

# View
head( subext.train ); dim( subext.train )

# Create testing dataset:
subext.test <- subext[ t.parows, ]

#### Systematic testing of different numbers of trees and learning rates

# Create a list of learning rates to test
lr <- c(0.05, 0.01, 0.005, 0.001, 0.0005)
tree.list <- seq(100, 30000, by=100)

# Depending on the number of trees fitted, predicting with more than this number will throw
# a warning message that 'Number of trees not specified or exceeded number fit so far.'
# This is fine - for trees above the ideal number it just repeats the predictions using the 
# optimal number of trees. This is what I want to happen - if it doesn't it means that the
# test needs more trees.


################ TC = 1 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc1<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 1, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the model - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc1[,i])) {
    deviance.tc1[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc1, na.rm=TRUE)
which(deviance.tc1$lr05 == min(deviance.tc1, na.rm=TRUE))
deviance.tc1[5,]
#Minimum is 4.85  at 500 trees at lr of 0.05

# Graph the results of the learning rate comparison for tc=1
tc1 <- ggplot() +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc1, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc1[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(a)",
           size=7) +
  annotate("text", x = 8000, y = 9.5, label = "tc = 1",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9.5, label = "min=4.85, lr=0.05",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 9, label = "nt=500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc1 nt30000.jpg", plot=tc1, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 2 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc2<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 2, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc2[,i])) {
    deviance.tc2[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc2, na.rm=TRUE)
which(deviance.tc2$lr05 == min(deviance.tc2, na.rm=TRUE))
deviance.tc2[9,]
#Minimum is 2.96 at 900 trees at lr of 0.05

# Graph the results of the learning rate comparison for tc=2
tc2 <- ggplot() +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc2, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc2[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=900, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(b)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 2",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.96, lr=0.05",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=900",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc2 nt30000.jpg", plot=tc2, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 3 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc3<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 3, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc3[,i])) {
    deviance.tc3[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc3, na.rm=TRUE)
which(deviance.tc3$lr01 == min(deviance.tc3, na.rm=TRUE))
deviance.tc3[31,]
#Minimum is 2.64 at 3100 trees at lr of 0.01


# Graph the results of the learning rate comparison for tc=3
tc3 <- ggplot() +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc3, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc3[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=3100, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(c)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 3",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.64, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=3100",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc3 nt30000.jpg", plot=tc3, device = "jpeg",
       width=7, height=5, dpi=300)


################ TC = 4 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc4<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 4, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc4[,i])) {
    deviance.tc4[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc4, na.rm=TRUE)
which(deviance.tc4$lr01 == min(deviance.tc4, na.rm=TRUE))
deviance.tc4[5,]
#Minimum is 2.46 at 2500 trees at lr of 0.01


# Graph the results of the learning rate comparison for tc=4
tc4 <- ggplot() +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc4, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc4[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=2500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(d)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 4",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.46, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=2500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc4 nt30000.jpg", plot=tc4, device = "jpeg",
       width=7, height=5, dpi=300)


################ TC = 5 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc5<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 5, learning.rate = lr[i],
                   max.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc5[,i])) {
    deviance.tc5[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc5, na.rm=TRUE)
which(deviance.tc5$lr005 == min(deviance.tc5, na.rm=TRUE))
deviance.tc5[4,]
#Minimum is 2.35 at 3500 trees at lr of 0.005

# Graph the results of the learning rate comparison for tc=5
tc5 <- ggplot() +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc5, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc5[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=3500, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(e)",
           size=7) +
  annotate("text", x = 6500, y = 9, label = "tc = 5",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.35, lr=0.005",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=3500",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc5 nt30000.jpg", plot=tc5, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 6 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc6<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 6, learning.rate = lr[i],
                   n.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc6[,i])) {
    deviance.tc6[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc6, na.rm=TRUE)
which(deviance.tc6$lr01 == min(deviance.tc6, na.rm=TRUE))
deviance.tc6[52,]
#Minimum is 2.2 at 5200 trees at lr of 0.01

# Graph the results of the learning rate comparison for tc=6
tc6 <- ggplot() +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc6, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc6[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=5200, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(f)",
           size=7) +
  annotate("text", x = 8000, y = 9, label = "tc = 6",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.20, lr=0.01",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=5200",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc6 nt30000.jpg", plot=tc6, device = "jpeg",
       width=7, height=5, dpi=300)

################ TC = 7 ###########################

#Create an empty dataframe in which to store the predicted deviances
deviance.tc7<-data.frame(lr05=numeric(300), lr01=numeric(300), lr005=numeric(300), lr001=numeric(300),
                         lr0005=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(lr)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 7, learning.rate = lr[i],
                   n.trees = 30000)
  
  # Create matrix of predictions, each column = predictions from the mode - for example, the predictions
  # in column 5 are for tree.list[5]=500 trees
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.tc7[,i])) {
    deviance.tc7[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.tc7, na.rm=TRUE)
which(deviance.tc7$lr005 == min(deviance.tc7, na.rm=TRUE))
deviance.tc7[112,]
#Minimum is 2.15 at 11200 trees at lr of 0.005

# Graph the results of the learning rate comparison for tc=7
tc7 <- ggplot() +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr05, linetype="0.05"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr01, linetype="0.01"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr005, linetype="0.005"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr001, linetype="0.001"), size=1) +
  geom_line(data=deviance.tc7, aes(x=nt, y=lr0005, linetype="0.0005"), size=1) +
  geom_hline(yintercept=min(deviance.tc7[,c(1:5)]), linetype="solid", color="red") +
  geom_vline(xintercept=11200, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_linetype_manual(name='', values = c('0.05' = "twodash", 
                                            '0.01' = "solid",
                                            '0.005' = "longdash",
                                            '0.001' = "dotted",
                                            '0.0005' = "dotdash"),
                        labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3)))) +
  annotate("text", x = 29000, y = 10, label = "(g)",
           size=7) +
  annotate("text", x = 7500, y = 9, label = "tc = 7",
           fontface="italic", size=7) +
  annotate("text", x = 20500, y = 9, label = "min=2.15, lr=0.005",
           fontface="italic", size=7) +
  annotate("text", x = 19500, y = 8, label = "nt=11200",
           fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Deviance tc7 nt30000.jpg", plot=tc7, device = "jpeg",
       width=7, height=5, dpi=300)



>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
