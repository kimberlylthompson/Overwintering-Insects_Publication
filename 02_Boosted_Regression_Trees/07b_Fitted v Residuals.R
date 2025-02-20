###############################################################################
#####                                                                     #####
#####                 Examining whether any dependence exists             #####
#####                         in BRT model residuals                      #####
#####                       (Model evaluation part 2)                     #####
###############################################################################

# This script creates plots of residual vs. fitted values to evaluate
# whether there is overfitting. We are looking for random scatter around
# the zero line.


# Clean workspace and load required packages
rm(list = ls() ) 
gc() #releases memory

library(tidyverse)
library(ggplot2)
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
###            Plotting           ###
###                               ###
#####################################

# Loop through each treatment
for (j in 1:4) {
  
  if(j == 1) {
    master <- external
    graph.title <- "external"
  } else {
    if(j == 2) {
      master <- house0
      graph.title <- "house0"
    } else {
      if(j == 3) {
        master <- house3
        graph.title <- "house3"
      } else {
        if(j == 4) {
          master <- house5
          graph.title <- "house5"
        }
      }
    }
  }

resid.plot <- ggplot(data = master) +
  geom_point(aes(x = summarized.pred, y = summarized.resid),
            size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(lim = c(-10, 7), name = "Residuals",
                     breaks = c(-10, -5, 0, 5)) +
  scale_x_continuous(lim = c(-15, 7), name = "Fitted Values",
                   breaks = c(-15, -10, -5, 0, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

setwd("H:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Boosted Regression Trees/Residuals")
ggsave(paste("Residual plot_", graph.title, ".jpg", sep = ""),
       resid.plot, device = "jpeg",
       width = 7, height = 5, dpi = 300)

print(j)

}


#####################################
###                               ###
###            GGArrange          ###
###                               ###
#####################################

resid.ext <- ggplot(data = external) +
  geom_point(aes(x = summarized.pred, y = summarized.resid),
             size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(lim = c(-10, 7), name = "Residuals",
                     breaks = c(-10, -5, 0, 5)) +
  scale_x_continuous(lim = c(-15, 7), name = "Fitted Values",
                     breaks = c(-15, -10, -5, 0, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

resid.h0 <- ggplot(data = house0) +
  geom_point(aes(x = summarized.pred, y = summarized.resid),
             size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(lim = c(-10, 7), name = "Residuals",
                     breaks = c(-10, -5, 0, 5)) +
  scale_x_continuous(lim = c(-15, 7), name = "Fitted Values",
                     breaks = c(-15, -10, -5, 0, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

resid.h3 <- ggplot(data = house3) +
  geom_point(aes(x = summarized.pred, y = summarized.resid),
             size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(lim = c(-10, 7), name = "Residuals",
                     breaks = c(-10, -5, 0, 5)) +
  scale_x_continuous(lim = c(-15, 7), name = "Fitted Values",
                     breaks = c(-15, -10, -5, 0, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

resid.h5 <- ggplot(data = house5) +
  geom_point(aes(x = summarized.pred, y = summarized.resid),
             size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(lim = c(-10, 7), name = "Residuals",
                     breaks = c(-10, -5, 0, 5)) +
  scale_x_continuous(lim = c(-15, 7), name = "Fitted Values",
                     breaks = c(-15, -10, -5, 0, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))


ggarrange(plotlist = c(resid.ext, resid.h0, resid.h3, resid.h5),
          ncol = 2, nrow = 2,
          labels = c("External Control", "+0°C", "+3°C", "+5°C"))
