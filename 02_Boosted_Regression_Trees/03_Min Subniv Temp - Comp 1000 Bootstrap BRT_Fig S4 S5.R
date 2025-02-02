<<<<<<< HEAD
####################################################################################
######    This script compares the deviance and root mean square error        ######
#####     of the different BRT models for subnivium temperature               ######
#####                 each of which was run 1000 times                        #######
####      to determine how sensitive the predictions are to different         #######
#####     BRT parameters (tree complexity, learning rate, bag fraction)       #######       
#####################################################################################

# Author: Kimberly Thompson

# This script compares the deviance and RMSE for 1000 bootstrap samples of each 
# combination of model settings.
# Errors for each combination of settings are first merged

# Deviance and RMSe come from the following data:
# 'TC3.LR01.BF55 Deviance and RMSE.csv'
# 'TC4.LR01.BF60 Deviance and RMSE.csv'
# 'TC5.LR005.BF65 Deviance and RMSE.csv'
# 'TC6.LR01.BF80 Deviance and RMSE.csv'
# 'TC7.LR005.BF80 Deviance and RMSE.csv'


# Produces datasets
# 'Master_BRT errors_min subniv temp.csv'

# Produces plots
# Figure S4
# Figure S5

# Tests within provide data for tables
# Table S1



# All BRT models were run with a 70/30 training/testing data split.

# The different models tested are as follows:
# 1.	TC 3 / LR 0.01 / BF 0.55 
# 2.	TC 4 / LR 0.01 / BF 0.60
# 3.	TC 5 / LR 0.005 / BF 0.65 
# 4.	TC 6 / LR 0.01 / BF0.80  
# 5.	TC 7 / LR 0.005 / BF 0.80 

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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")

# List the files
BRTerrors <- list.files(getwd(), pattern = "TC")

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

# Write the master BRT errors file
setwd("01_Analysis/Model_Parameters/")
write.csv(evaluation.df, "Master_BRT errors_min subniv temp.csv", row.names = FALSE)


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

# Visualize data
comparison.dev <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=Model, y=Deviance),
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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Models")
ggsave("Deviance Boxplot of 1000 iteration tests.jpg", plot=comparison.dev, device = "jpeg",
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




##### For RMSE comparison, it is not possible to use ANOVA bc variances are not
##### homogeneous. So we need the Welch one-way test which does not assume
##### equal variances for all groups.

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  summarise(
    count = n(),
    mean = mean(RMSE, na.rm = TRUE),
    sd = sd(RMSE, na.rm = TRUE)
  )

# Visualize data
comparison.rmse <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=Model, y=RMSE),
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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Models")
ggsave("RMSE Boxplot of 1000 iteration tests.jpg", plot=comparison.rmse, device = "jpeg",
       width=7, height=5, dpi=300)

# Conduct the one-way anova test
rmse.aov <- aov(RMSE ~ Model, data = evaluation.df)


# Summary of the analysis
summary(rmse.aov)

####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(rmse.aov, 1)

#Levene's test
leveneTest(RMSE ~ Model, data = evaluation.df)
# violated!

# Normality of residuals assumption
plot(dev.aov, 2)

# Because the homogeneity of variances is violated, use Welch
rmse.welch <- oneway.test(RMSE ~ Model, data = evaluation.df)

rmse.welch

# Pairwise t-test with no assumption of equal variances
pairwise.t.test(evaluation.df$RMSE, evaluation.df$Model,
                p.adjust.method = "BH", pool.sd = FALSE)


##########################################################################################

# Testing for differences in distributions

# Deviance

# 3 VS 4
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC3.LR01.BF55"],
        evaluation.df$Deviance[evaluation.df$Model == "TC4.LR01.BF60"])
# Significantly different

# 4 VS 5
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC4.LR01.BF60"],
        evaluation.df$Deviance[evaluation.df$Model == "TC5.LR005.BF65"])
# Significantly different

# 5 VS 6
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC5.LR005.BF65"],
        evaluation.df$Deviance[evaluation.df$Model == "TC6.LR01.BF80"])
# Significantly different


# 6 VS 7
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC7.LR005.BF80"],
        evaluation.df$Deviance[evaluation.df$Model == "TC6.LR01.BF80"])
# Significantly different






=======
####################################################################################
######    This script compares the deviance and root mean square error        ######
#####     of the different BRT models for subnivium temperature               ######
#####                 each of which was run 1000 times                        #######
####      to determine how sensitive the predictions are to different         #######
#####     BRT parameters (tree complexity, learning rate, bag fraction)       #######       
#####################################################################################

# Author: Kimberly Thompson

# This script compares the deviance and RMSE for 1000 bootstrap samples of each 
# combination of model settings.
# Errors for each combination of settings are first merged

# Deviance and RMSe come from the following data:
# 'TC3.LR01.BF55 Deviance and RMSE.csv'
# 'TC4.LR01.BF60 Deviance and RMSE.csv'
# 'TC5.LR005.BF65 Deviance and RMSE.csv'
# 'TC6.LR01.BF80 Deviance and RMSE.csv'
# 'TC7.LR005.BF80 Deviance and RMSE.csv'


# Produces datasets
# 'Master_BRT errors_min subniv temp.csv'

# Produces plots
# Figure S4
# Figure S5

# Tests within provide data for tables
# Table S1



# All BRT models were run with a 70/30 training/testing data split.

# The different models tested are as follows:
# 1.	TC 3 / LR 0.01 / BF 0.55 
# 2.	TC 4 / LR 0.01 / BF 0.60
# 3.	TC 5 / LR 0.005 / BF 0.65 
# 4.	TC 6 / LR 0.01 / BF0.80  
# 5.	TC 7 / LR 0.005 / BF 0.80 

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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Results/Boosted Regression Trees/Model Comparisons")

# List the files
BRTerrors <- list.files(getwd(), pattern = "TC")

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

# Write the master BRT errors file
setwd("01_Analysis/Model_Parameters/")
write.csv(evaluation.df, "Master_BRT errors_min subniv temp.csv", row.names = FALSE)


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

# Visualize data
comparison.dev <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=Model, y=Deviance),
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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Models")
ggsave("Deviance Boxplot of 1000 iteration tests.jpg", plot=comparison.dev, device = "jpeg",
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




##### For RMSE comparison, it is not possible to use ANOVA bc variances are not
##### homogeneous. So we need the Welch one-way test which does not assume
##### equal variances for all groups.

# Compute summary stats by groups - count, mean, sd
group_by(evaluation.df, Model) %>%
  summarise(
    count = n(),
    mean = mean(RMSE, na.rm = TRUE),
    sd = sd(RMSE, na.rm = TRUE)
  )

# Visualize data
comparison.rmse <- ggplot() +
  geom_boxplot(data=evaluation.df, aes(x=Model, y=RMSE),
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
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Comparing Models")
ggsave("RMSE Boxplot of 1000 iteration tests.jpg", plot=comparison.rmse, device = "jpeg",
       width=7, height=5, dpi=300)

# Conduct the one-way anova test
rmse.aov <- aov(RMSE ~ Model, data = evaluation.df)


# Summary of the analysis
summary(rmse.aov)

####### Check the assumptions of the ANOVA

# Homogeniety of variance assumption
plot(rmse.aov, 1)

#Levene's test
leveneTest(RMSE ~ Model, data = evaluation.df)
# violated!

# Normality of residuals assumption
plot(dev.aov, 2)

# Because the homogeneity of variances is violated, use Welch
rmse.welch <- oneway.test(RMSE ~ Model, data = evaluation.df)

rmse.welch

# Pairwise t-test with no assumption of equal variances
pairwise.t.test(evaluation.df$RMSE, evaluation.df$Model,
                p.adjust.method = "BH", pool.sd = FALSE)


##########################################################################################

# Testing for differences in distributions

# Deviance

# 3 VS 4
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC3.LR01.BF55"],
        evaluation.df$Deviance[evaluation.df$Model == "TC4.LR01.BF60"])
# Significantly different

# 4 VS 5
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC4.LR01.BF60"],
        evaluation.df$Deviance[evaluation.df$Model == "TC5.LR005.BF65"])
# Significantly different

# 5 VS 6
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC5.LR005.BF65"],
        evaluation.df$Deviance[evaluation.df$Model == "TC6.LR01.BF80"])
# Significantly different


# 6 VS 7
ks.test(evaluation.df$Deviance[evaluation.df$Model == "TC7.LR005.BF80"],
        evaluation.df$Deviance[evaluation.df$Model == "TC6.LR01.BF80"])
# Significantly different






>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
