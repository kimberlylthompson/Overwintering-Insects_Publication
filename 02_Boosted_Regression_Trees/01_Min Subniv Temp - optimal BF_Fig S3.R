<<<<<<< HEAD
####################################################################################
###### Analysis of winter 2016-17, December - March min subnivium temperature ######
#####     data using Boosted Regression Trees - This script determines        #######
####               the optimal bag fraction for the BRT models                #######       
#####################################################################################

# Author: Kimberly Thompson

# This script determines the optimal bag fraction for BRT models.

# Plots created
# Figure S3


# Response: Minimum subnivium temperature

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


#####  Split data into training and testing datasets with approximately 70/30 split

# Total number of sites sampled in subnivium/absence data:
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

tree.list <- seq(100, 30000, by=100)

######################### BAG FRACTION = STOCHASTICITY ###############################

# Now vary the bag fraction 
bf<-c(0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85)

### Candidate 1 ###

#for the first potential tc and lr combination (tc=3 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                        bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                        bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                        bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 3, learning.rate = 0.01,
                   max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf[,i])) {
    deviance.bf[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf, na.rm=TRUE)
min(deviance.bf[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf[,7] == min(deviance.bf, na.rm=TRUE))
deviance.bf[29,]
#bf = 0.55
 

# Graph the results of the bag fraction comparison
bf <- ggplot() +
  geom_line(data=deviance.bf, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2900, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
  # annotate("text", x = 2200, y = 1, label = "bf = 0.45",
  #          fontface="italic", size=7) +
  # annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
  #          fontface="italic", size=7) +
  # annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
  #          fontface="italic", size=7)
  
setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC3.jpg", plot=bf, device = "jpeg",
       width=7, height=5, dpi=300)


rm(deviance.bf, pred, mod)

### Candidate 2 ###

#for the first potential tc and lr combination (tc=4 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf4<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                        bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                        bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                        bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 4, learning.rate = 0.01,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf4[,i])) {
    deviance.bf4[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf4, na.rm=TRUE)
min(deviance.bf4[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf4[,8] == min(deviance.bf4, na.rm=TRUE))
deviance.bf4[26,]
#bf = 0.60


# Graph the results of the bag fraction comparison
bf4 <- ggplot() +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf4[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2600, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC4.jpg", plot=bf4, device = "jpeg",
       width=7, height=5, dpi=300)




### Candidate 3 ###

#for the first potential tc and lr combination (tc=5 and lr=0.005)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf5<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf5[,i])) {
    deviance.bf5[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf5, na.rm=TRUE)
min(deviance.bf5[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf5[,9] == min(deviance.bf5, na.rm=TRUE))
deviance.bf5[38,]
#bf = 0.65


bf5 <- ggplot() +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf5[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=3800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC5.jpg", plot=bf5, device = "jpeg",
       width=7, height=5, dpi=300)



### Candidate 4 ###

#for the first potential tc and lr combination (tc=6 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf6<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 6, learning.rate = 0.01,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf6[,i])) {
    deviance.bf6[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf6, na.rm=TRUE)
min(deviance.bf6[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf6[,12] == min(deviance.bf6, na.rm=TRUE))
deviance.bf6[38,]
#bf = 0.65


bf6 <- ggplot() +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf6[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=3800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC6.jpg", plot=bf6, device = "jpeg",
       width=7, height=5, dpi=300)


### Candidate 5 ###

#for the first potential tc and lr combination (tc=7 and lr=0.005)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf7<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 7, learning.rate = 0.005,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf7[,i])) {
    deviance.bf7[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf7, na.rm=TRUE)
min(deviance.bf7[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf7[,12] == min(deviance.bf7, na.rm=TRUE))
deviance.bf7[28,]
#bf = 0.65


bf7 <- ggplot() +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf7[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC7.jpg", plot=bf7, device = "jpeg",
       width=7, height=5, dpi=300)

=======
####################################################################################
###### Analysis of winter 2016-17, December - March min subnivium temperature ######
#####     data using Boosted Regression Trees - This script determines        #######
####               the optimal bag fraction for the BRT models                #######       
#####################################################################################

# Author: Kimberly Thompson

# This script determines the optimal bag fraction for BRT models.

# Plots created
# Figure S3


# Response: Minimum subnivium temperature

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


#####  Split data into training and testing datasets with approximately 70/30 split

# Total number of sites sampled in subnivium/absence data:
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

tree.list <- seq(100, 30000, by=100)

######################### BAG FRACTION = STOCHASTICITY ###############################

# Now vary the bag fraction 
bf<-c(0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85)

### Candidate 1 ###

#for the first potential tc and lr combination (tc=3 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                        bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                        bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                        bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                   family = "gaussian", tree.complexity = 3, learning.rate = 0.01,
                   max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf[,i])) {
    deviance.bf[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf, na.rm=TRUE)
min(deviance.bf[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf[,7] == min(deviance.bf, na.rm=TRUE))
deviance.bf[29,]
#bf = 0.55
 

# Graph the results of the bag fraction comparison
bf <- ggplot() +
  geom_line(data=deviance.bf, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2900, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
  # annotate("text", x = 2200, y = 1, label = "bf = 0.45",
  #          fontface="italic", size=7) +
  # annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
  #          fontface="italic", size=7) +
  # annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
  #          fontface="italic", size=7)
  
setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC3.jpg", plot=bf, device = "jpeg",
       width=7, height=5, dpi=300)


rm(deviance.bf, pred, mod)

### Candidate 2 ###

#for the first potential tc and lr combination (tc=4 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf4<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                        bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                        bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                        bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 4, learning.rate = 0.01,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf4[,i])) {
    deviance.bf4[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf4, na.rm=TRUE)
min(deviance.bf4[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf4[,8] == min(deviance.bf4, na.rm=TRUE))
deviance.bf4[26,]
#bf = 0.60


# Graph the results of the bag fraction comparison
bf4 <- ggplot() +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf4, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf4[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2600, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC4.jpg", plot=bf4, device = "jpeg",
       width=7, height=5, dpi=300)




### Candidate 3 ###

#for the first potential tc and lr combination (tc=5 and lr=0.005)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf5<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 5, learning.rate = 0.005,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf5[,i])) {
    deviance.bf5[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf5, na.rm=TRUE)
min(deviance.bf5[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf5[,9] == min(deviance.bf5, na.rm=TRUE))
deviance.bf5[38,]
#bf = 0.65


bf5 <- ggplot() +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf5, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf5[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=3800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC5.jpg", plot=bf5, device = "jpeg",
       width=7, height=5, dpi=300)



### Candidate 4 ###

#for the first potential tc and lr combination (tc=6 and lr=0.01)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf6<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 6, learning.rate = 0.01,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf6[,i])) {
    deviance.bf6[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf6, na.rm=TRUE)
min(deviance.bf6[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf6[,12] == min(deviance.bf6, na.rm=TRUE))
deviance.bf6[38,]
#bf = 0.65


bf6 <- ggplot() +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf6, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf6[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=3800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC6.jpg", plot=bf6, device = "jpeg",
       width=7, height=5, dpi=300)


### Candidate 5 ###

#for the first potential tc and lr combination (tc=7 and lr=0.005)
#Create an empty dataframe in which to store the predicted deviances
deviance.bf7<-data.frame(bf25=numeric(300), bf30=numeric(300), bf35=numeric(300), bf40=numeric(300),
                         bf45=numeric(300), bf50=numeric(300), bf55=numeric(300), bf60=numeric(300),
                         bf65=numeric(300), bf70=numeric(300), bf75=numeric(300), bf80=numeric(300),
                         bf85=numeric(300), nt=seq(100, 30000, by=100))

# Model each different learning rate and store them in a data frame
for (i in 1:length(bf)) {
  
  mod <- gbm.step(data=subext.train, gbm.x = c(6:11), gbm.y = 14,
                  family = "gaussian", tree.complexity = 7, learning.rate = 0.005,
                  max.trees = 30000, bag.fraction = bf[i])
  
  # Create matrix of predictions, each column = predictions from the mode
  pred <- gbm :: predict.gbm(mod, subext.test, n.trees = tree.list, type="response")
  
  # Calculate deviance of all of these results and store them in a dataframe:
  for (j in 1:length(deviance.bf7[,i])) {
    deviance.bf7[,i][j] <- calc.deviance(subext.test$min.subniv, pred[,j], family="gaussian", calc.mean = T)
  }
}

min(deviance.bf7, na.rm=TRUE)
min(deviance.bf7[deviance.bf != 0], na.rm = TRUE)
which(deviance.bf7[,12] == min(deviance.bf7, na.rm=TRUE))
deviance.bf7[28,]
#bf = 0.65


bf7 <- ggplot() +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf25, color="bf25"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf30, color="bf30"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf35, color="bf35"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf40, color="bf40"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf45, color="bf45"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf50, color="bf50"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf55, color="bf55"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf60, color="bf60"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf65, color="bf65"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf70, color="bf70"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf75, color="bf75"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf80, color="bf80"), size=1) +
  geom_line(data=deviance.bf7, aes(x=nt, y=bf85, color="bf85"), size=1) +
  geom_hline(yintercept=min(deviance.bf7[,c(1:11)], na.rm=TRUE), linetype="solid", color="red") +
  geom_vline(xintercept=2800, linetype="solid", color="green") +
  #geom_errorbar(width=0.2, size=2) + geom_point(size=8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold")) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\nNumber of Trees", limits=c(0, 30000),
                     breaks=c(0, 10000, 20000, 30000)) +
  scale_y_continuous(name="Predictive Deviance\n") +
  scale_colour_manual(name='', values = c('bf25' = "red", 
                                          'bf30' = "orange",
                                          'bf35' = "yellow",
                                          'bf40' = "green",
                                          'bf45' = "blue",
                                          'bf50' = "purple",
                                          'bf55' = "darkgreen",
                                          'bf60' = "turquoise2",
                                          'bf65' = "brown3",
                                          'bf70' = "darkgoldenrod3",
                                          'bf75' = "hotpink",
                                          'bf80' = "navyblue",
                                          'bf85' = "darkred"), 
                      labels = c("0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
                                 "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85")) +
  theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme(legend.text = element_text(size=14)) +
  guides(colour = guide_legend(override.aes = (list(size=3))))
# annotate("text", x = 2200, y = 1, label = "bf = 0.45",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 1, label = "min=0.65, lr=0.01",
#          fontface="italic", size=7) +
# annotate("text", x = 12500, y = 0.9, label = "nt=5400, tc=3",
#          fontface="italic", size=7)


setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Model Settings")
ggsave("Ext - Deviance BF nt30000_minsubniv_TC7.jpg", plot=bf7, device = "jpeg",
       width=7, height=5, dpi=300)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
