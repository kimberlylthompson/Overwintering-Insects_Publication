<<<<<<< HEAD
####################################################################################
######    This script compares the deviance and root mean square error        ######
#####     of the BRT model with TC5, LR0.005, and BF65 for min subnivium temp ######
#####                 run a different number of times                         #######
####      to determine how sensitive the predictions are to different         #######
#####                          numbers of iterations                          #######       
#####################################################################################

# Author: Kimberly Thompson


# This script compares deviance and RMSE for BRT models run with different 
# numbers of iterations.

# It first merges the deviance and RMSE for each number of iterations into 1 dataframe.


# Datasets created:
# 'Master_TC5 Samples errors_min subniv temp.csv'

# Plots created:
# Figure S6
# Figure S7

# Tests within provide data for tables
# Table S2
# Table S3







# All BRT models were run with a 70/30 training/testing data split.

# The different bootstrap samples are:
# 25, 50, 75, 100, 150, 200, and 1000

# The goal is to have one of these smaller sample sizes have a mean deviance and RMSE that
# is NOT significantly different from the mean of the 1000 samples

#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory


# Load libraries
library(lubridate)
library(dplyr)
# library(plyr)
library(ggplot2)
library(reshape)



# Set working directory
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")

# List the files
BRTerrors <- list.files(getwd(), pattern = "Samples")

# Create a new dataframe to add each dataframe to
evaluation.df <- data.frame(Iteration = integer(), Deviance = numeric(),
                            RMSE = numeric(), Model = character())

# Loop to add each models iterations to the evaluation dataframe
for(i in 1:length(BRTerrors)) {
  
  # Read in the csv file
  individ.file <- read.csv(BRTerrors[i], header = TRUE)
  
  # Add a column for the type of model and fill it with the file name
  # But take off the .csv
  individ.file$Model <- gsub(" Deviance and RMSE.csv", "", BRTerrors[i])
  
  evaluation.df <- rbind(evaluation.df, individ.file)
  
  print(i)
}

# Remove the rows where RMSE is NA (these exist because in the previous script where
# I generate these dataframes I forgot to specify that the length of these columns
# was different from 1000)

evaluation.df <- evaluation.df[is.na(evaluation.df$RMSE) == FALSE, ]

# Write the master BRT errors file
write.csv(evaluation.df, "Master_TC5 Samples errors_min subniv temp.csv", row.names = FALSE)


#######################################################################

# Testing for significant differences between the Deviances and RMSEs of different
# models

# Deviances are close to normally-distributed
hist(evaluation.df$Deviance)
# RMSEs are close to normally-distributed
hist(evaluation.df$RMSE)

##### For Deviance comparison, use one-way ANOVA
# Testing for differences in the means

# Convert Model column to factor
evaluation.df$Model <- as.factor(evaluation.df$Model)

# Check the 8 levels
levels(evaluation.df$Model)

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  dplyr :: summarise(
    count = n(),
    mean = mean(Deviance, na.rm = TRUE),
    sd = sd(Deviance, na.rm = TRUE)
  )

level_order <- c('25 Samples', '50 Samples', '75 Samples', '100 Samples', '150 Samples', '1000 Samples') #this vector might be useful for other plots/analyses

ggplot(iris, aes(x = factor(Species, level = level_order), y = Petal.Width)) + geom_col()

# Visualize data
comparison.dev <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=factor(Model, level = level_order), y=Deviance),
               outlier.colour = "black", outlier.shape = 8,
               outlier.size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold", angle=45, hjust = 1)) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Predictive Deviance\n")

comparison.dev

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Iterations")
ggsave("Deviance Boxplot of iteration tests.jpg", plot=comparison.dev, device = "jpeg",
       width=7, height=5, dpi=300)

# conduct the one-way anova test
dev.aov <- aov(Deviance ~ Model, data = evaluation.df)

# Summary of the analysis
summary(dev.aov)

# Compute the Tukey Honest significant Diffrences to perform pairwise comparisons
TukeyHSD(dev.aov)


####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(dev.aov, 1)

#Levene's test
library(car)
leveneTest(Deviance ~ Model, data = evaluation.df)


# Normality of residuals assumption
plot(dev.aov, 2)




##### For RMSE comparison, assumptions of ANOVA are met

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  dplyr :: summarise(
    count = n(),
    mean = mean(RMSE, na.rm = TRUE),
    sd = sd(RMSE, na.rm = TRUE)
  )

# Visualize data
comparison.rmse <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=factor(Model, level = level_order), y=RMSE),
               outlier.colour = "black", outlier.shape = 8,
               outlier.size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold", angle=45, hjust = 1)) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="RMSE (°C)\n")

comparison.rmse

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Iterations")
ggsave("RMSE Boxplot of iteration tests.jpg", plot=comparison.rmse, device = "jpeg",
       width=7, height=5, dpi=300)

# Conduct the one-way anova test
rmse.aov <- aov(RMSE ~ Model, data = evaluation.df)


# Summary of the analysis
summary(rmse.aov)

# Compute the Tukey Honest significant Diffrences to perform pairwise comparisons
TukeyHSD(rmse.aov)

####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(rmse.aov, 1)

#Levene's test
leveneTest(RMSE ~ Model, data = evaluation.df)
# ok good

# Normality of residuals assumption
plot(dev.aov, 2)




##########################################################################################

# Testing for differences in distributions

# Deviance

# 25 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "25 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 50 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "50 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 75 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "75 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 100 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "100 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different


# 150 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "150 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different



# RMSE

# 25 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "25 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 50 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "50 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 75 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "75 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 100 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "100 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different


# 150 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "150 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])


=======
####################################################################################
######    This script compares the deviance and root mean square error        ######
#####     of the BRT model with TC5, LR0.005, and BF65 for min subnivium temp ######
#####                 run a different number of times                         #######
####      to determine how sensitive the predictions are to different         #######
#####                          numbers of iterations                          #######       
#####################################################################################

# Author: Kimberly Thompson


# This script compares deviance and RMSE for BRT models run with different 
# numbers of iterations.

# It first merges the deviance and RMSE for each number of iterations into 1 dataframe.


# Datasets created:
# 'Master_TC5 Samples errors_min subniv temp.csv'

# Plots created:
# Figure S6
# Figure S7

# Tests within provide data for tables
# Table S2
# Table S3







# All BRT models were run with a 70/30 training/testing data split.

# The different bootstrap samples are:
# 25, 50, 75, 100, 150, 200, and 1000

# The goal is to have one of these smaller sample sizes have a mean deviance and RMSE that
# is NOT significantly different from the mean of the 1000 samples

#####clean workspace to improve efficiency: ###
rm(list = ls() ) 
gc() #releases memory


# Load libraries
library(lubridate)
library(dplyr)
# library(plyr)
library(ggplot2)
library(reshape)



# Set working directory
setwd("01_Analysis/Model_Parameters/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Testing Number of Predictions Needed")

# List the files
BRTerrors <- list.files(getwd(), pattern = "Samples")

# Create a new dataframe to add each dataframe to
evaluation.df <- data.frame(Iteration = integer(), Deviance = numeric(),
                            RMSE = numeric(), Model = character())

# Loop to add each models iterations to the evaluation dataframe
for(i in 1:length(BRTerrors)) {
  
  # Read in the csv file
  individ.file <- read.csv(BRTerrors[i], header = TRUE)
  
  # Add a column for the type of model and fill it with the file name
  # But take off the .csv
  individ.file$Model <- gsub(" Deviance and RMSE.csv", "", BRTerrors[i])
  
  evaluation.df <- rbind(evaluation.df, individ.file)
  
  print(i)
}

# Remove the rows where RMSE is NA (these exist because in the previous script where
# I generate these dataframes I forgot to specify that the length of these columns
# was different from 1000)

evaluation.df <- evaluation.df[is.na(evaluation.df$RMSE) == FALSE, ]

# Write the master BRT errors file
write.csv(evaluation.df, "Master_TC5 Samples errors_min subniv temp.csv", row.names = FALSE)


#######################################################################

# Testing for significant differences between the Deviances and RMSEs of different
# models

# Deviances are close to normally-distributed
hist(evaluation.df$Deviance)
# RMSEs are close to normally-distributed
hist(evaluation.df$RMSE)

##### For Deviance comparison, use one-way ANOVA
# Testing for differences in the means

# Convert Model column to factor
evaluation.df$Model <- as.factor(evaluation.df$Model)

# Check the 8 levels
levels(evaluation.df$Model)

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  dplyr :: summarise(
    count = n(),
    mean = mean(Deviance, na.rm = TRUE),
    sd = sd(Deviance, na.rm = TRUE)
  )

level_order <- c('25 Samples', '50 Samples', '75 Samples', '100 Samples', '150 Samples', '1000 Samples') #this vector might be useful for other plots/analyses

ggplot(iris, aes(x = factor(Species, level = level_order), y = Petal.Width)) + geom_col()

# Visualize data
comparison.dev <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=factor(Model, level = level_order), y=Deviance),
               outlier.colour = "black", outlier.shape = 8,
               outlier.size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold", angle=45, hjust = 1)) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Predictive Deviance\n")

comparison.dev

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Iterations")
ggsave("Deviance Boxplot of iteration tests.jpg", plot=comparison.dev, device = "jpeg",
       width=7, height=5, dpi=300)

# conduct the one-way anova test
dev.aov <- aov(Deviance ~ Model, data = evaluation.df)

# Summary of the analysis
summary(dev.aov)

# Compute the Tukey Honest significant Diffrences to perform pairwise comparisons
TukeyHSD(dev.aov)


####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(dev.aov, 1)

#Levene's test
library(car)
leveneTest(Deviance ~ Model, data = evaluation.df)


# Normality of residuals assumption
plot(dev.aov, 2)




##### For RMSE comparison, assumptions of ANOVA are met

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  dplyr :: summarise(
    count = n(),
    mean = mean(RMSE, na.rm = TRUE),
    sd = sd(RMSE, na.rm = TRUE)
  )

# Visualize data
comparison.rmse <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=factor(Model, level = level_order), y=RMSE),
               outlier.colour = "black", outlier.shape = 8,
               outlier.size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(size=20, face="bold", angle=45, hjust = 1)) +
  theme(axis.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="RMSE (°C)\n")

comparison.rmse

setwd("01_Analysis/Model_Plotting/")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Iterations")
ggsave("RMSE Boxplot of iteration tests.jpg", plot=comparison.rmse, device = "jpeg",
       width=7, height=5, dpi=300)

# Conduct the one-way anova test
rmse.aov <- aov(RMSE ~ Model, data = evaluation.df)


# Summary of the analysis
summary(rmse.aov)

# Compute the Tukey Honest significant Diffrences to perform pairwise comparisons
TukeyHSD(rmse.aov)

####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(rmse.aov, 1)

#Levene's test
leveneTest(RMSE ~ Model, data = evaluation.df)
# ok good

# Normality of residuals assumption
plot(dev.aov, 2)




##########################################################################################

# Testing for differences in distributions

# Deviance

# 25 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "25 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 50 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "50 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 75 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "75 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 100 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "100 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different


# 150 VS 1000
ks.test(evaluation.df$Deviance[evaluation.df$Model == "150 Samples"],
        evaluation.df$Deviance[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different



# RMSE

# 25 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "25 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 50 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "50 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 75 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "75 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different

# 100 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "100 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])
# NOT Significantly different


# 150 VS 1000
ks.test(evaluation.df$RMSE[evaluation.df$Model == "150 Samples"],
        evaluation.df$RMSE[evaluation.df$Model == "1000 Samples"])


>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
