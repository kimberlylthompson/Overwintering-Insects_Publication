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
library(ggpubr)



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
###    observed v. Predicted      ###
###          Plotting             ###
#####################################

external.plot <- ggplot() +
  geom_point(data = external, aes(x = min.subniv, y = summarized.pred)) +
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


#####################################
###                               ###
###    observed v. Predicted      ###
###     Line graphs by site       ###
#####################################

by.date <- house0 %>%
  select(Date, Loc, min.subniv, summarized.pred) %>%
  pivot_longer(cols = c(min.subniv, summarized.pred),
               names_to = "Type",
               values_to = "Temperature")

by.date$Type2 <- NA

for (i in 1:length(by.date$Date)) {
  if(by.date$Type[i] == "min.subniv") {
    by.date$Type2[i] <- "Observed"
  } else {
    by.date$Type2[i] <- "Predicted"
  }
  print(i)
}

by.date$Category <- NA

for (i in 1:length(by.date$Category)) {
  if(by.date$Loc[i] == "A") {
    by.date$Category[i] <- "Low Latitude Deciduous"
  } else {
    if(by.date$Loc[i] == "HH") {
      by.date$Category[i] <- "Mid Latitude Deciduous"
    } else {
      if(by.date$Loc[i] == "MTD") {
        by.date$Category[i] <- "High Latitude Deciduous"
      } else {
        if(by.date$Loc[i] == "SW") {
          by.date$Category[i] <- "Low Latitude Open"
        } else {
          if(by.date$Loc[i] == "TH") {
            by.date$Category[i] <- "Mid Latitude Open"
          } else {
            if(by.date$Loc[i] == "MTOP") {
              by.date$Category[i] <- "High Latitude Open"
            } else {
              if(by.date$Loc[i] == "M") {
                by.date$Category[i] <- "Low Latitude Conifer"
              } else {
                if(by.date$Loc[i] == "L") {
                  by.date$Category[i] <- "Mid Latitude Conifer"
                } else {
                  if(by.date$Loc[i] == "C") {
                    by.date$Category[i] <- "High Latitude Conifer"
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  print(i)
}

by.date$Category <- factor(by.date$Category,
                           levels = c("Low Latitude Deciduous",
                                      "Low Latitude Conifer",
                                      "Low Latitude Open",
                                      "Mid Latitude Deciduous",
                                      "Mid Latitude Conifer",
                                      "Mid Latitude Open",
                                      "High Latitude Deciduous",
                                      "High Latitude Conifer",
                                      "High Latitude Open"))


h0.lineplot <- ggplot(data = by.date) +
  geom_line(aes(x = Date, y = Temperature, color = Type2, group = Type2),
            linewidth = 1) +
  scale_y_continuous(lim = c(-19, 8.5), name = "Subnivium Temperature (°C)",
                     breaks = c(-18, -12, -6, 0, 6)) +
  scale_x_discrete(name = "", breaks = c("2016-12-01",
                                         "2017-01-01",
                                         "2017-02-01",
                                         "2017-03-01"),
                   labels = c("Dec 1",
                              "Jan 1",
                              "Feb 1",
                              "Mar 1")) +
  facet_wrap(~ Category, nrow = 3, ncol = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(none)) +
  guides(color = guide_legend(override.aes = list(size = 3),
                              title = NULL))
  

