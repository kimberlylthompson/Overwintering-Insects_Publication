<<<<<<< HEAD
######################################################################################
###########      Comparing the vulnerability shown by SCP results     ################
###########         with the vulnerability shown by LLT results       ################
###########                   for green leaf beetles                  ################
######################################################################################

# Author: Kimberly Thompson

# This script takes the rasters that show the longest consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Comparison between LLT and SCP based on worst-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.


# Datasets produced:
# 'Comparing LLTime with days under SCP_external - worst case.csv'
# 'Comparing LLTime with days under SCP_3Cwarmer - worst case.csv'
# 'Comparing LLTime with days under SCP_5Cwarmer - worst case.csv'


# Figures produced:
# Figure 5
# Figure S12

# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)


# Workflow - for each scenario
# 1. Reclassify LLT raster to three values (based on green yellow and red in map, and the LLT values extracted from
#      Lam and Pedigo (2000).

# 2. Then use the SCP raster to extract the number of SCP days - essentially create a dataframe for each of the 
#      scenarios.



#########################################################################
#############              Changing Cell values          ################
#########################################################################

# Green leaf beetle - From Lam and Pedigo 2000, lower confidence limit = 28.87259, upper confidence limit = 41.32808

# From the maps of most consecutive days of below 0C temperatures, resample so that:
#     1. < 28.87259 the cell gets a 0
#     2. >= 28.87259 and <= 41.32808 the cell gets a 1
#     3. > 41.32808 the cell gets a 2



########################################
####    Interior Lakes Data          ###
########################################

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)


#############################
###  Raster Loading    ######
#############################

#### LLT50 AT 0C RASTERS ######

# I have four summary measures # of consecutive stretches of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

#### I initially made these maps to apply to Bombus, but they are not specific to Bombus yet since all they show are the longest
#### consecutive days during the winter season that the temperature was at or below 0C. So I can use these same ones for Bean Leaf and
#### just change the day threshold/raster names further down in the code.

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees")

dur0_mean <- raster :: raster("LLT50_0Cdays_Mean_External.tif")
dur3_mean <- raster :: raster("LLT50_0Cdays_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("LLT50_0Cdays_Mean_5C warmer.tif")

dur0_median <- raster :: raster("LLT50_0Cdays_Median_External.tif")
dur3_median <- raster :: raster("LLT50_0Cdays_Median_3C warmer.tif")
dur5_median <- raster :: raster("LLT50_0Cdays_Median_5C warmer.tif")

dur0_min <- raster :: raster("LLT50_0Cdays_Minimum_External.tif")
dur3_min <- raster :: raster("LLT50_0Cdays_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("LLT50_0Cdays_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("LLT50_0Cdays_Maximum_External.tif")
dur3_max <- raster :: raster("LLT50_0Cdays_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("LLT50_0Cdays_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_llt <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# Change the name to Bean leaf so it saves correctly
names(rasterlist_llt) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist_llt))



#############################
###  SCP Raster Loading   ###
#############################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Bean Leaf")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Green Leaf Beetle/Worst Case")

dur0_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_External.tif")
dur3_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_scp <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                        dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)



### Getting read for loop #########

wgs84 = sp:: CRS("+init=epsg:4326")

# To help with the raster processing
# Load in the states shapefile
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#### Make adjustments to both raster stacks so I calculate the summary stats for only the extent I'll be presenting in the figures

# transform each raster into degrees so that the axes will look
# better and be clearer to understand

rasterlist_llt <- projectRaster(from=rasterlist_llt, crs =  wgs84)
rasterlist_scp <- projectRaster(from=rasterlist_scp, crs =  wgs84)

# Convert projection of states so that it matches rasters:
states <- spTransform( states, proj4string( rasterlist_llt ) )

# Define the extent for the states
newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the rasters to match
rasterlist_llt <- crop(rasterlist_llt, extent(newstates))
rasterlist_llt <- raster :: mask(rasterlist_llt, newstates)

rasterlist_scp <- crop(rasterlist_scp, extent(newstates))
rasterlist_scp <- raster :: mask(rasterlist_scp, newstates)

  
## Reclassifying the rastet 
for (i in 1:nlayers(rasterlist_llt)) { 
  
  # Reclassify the raster
  rasterlist_llt[[i]][rasterlist_llt[[i]] < 28.87259] <- 0
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] >= 28.87259 & rasterlist_llt[[i]] <= 41.32808] <- 1
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] > 41.32808] <- 2
  
  print(i)
  
}



#### Loop to create a dataframe of days under SCP that also shows whether the cells falls into below, between, or above the
#### days until LLT50.

# This loop is not actually finished! I needed to accelerate the process so only did if for the mean and then manually wrote
# the csv files

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")

for (i in 1:length(rasterlist_llt)) { # to do just the mean maps which is what I'm presenting just say 1:3 rather than
  # length(rasterlist_llt)
  
  # Convert the LLT raster to a dataframe
  lltdata <- as.data.frame(rasterlist_llt[[i]], xy=TRUE)
  
  # Convert the SCP raster to a dataframe
  scpdata <- as.data.frame(rasterlist_scp[[i]], xy=TRUE)
  
  
  # Add the LLT info to the SCP dataframe
  scpdata <- cbind(scpdata, lltdata[, 3])
  
  # Rename the columns
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_External", "LLT_Bean.Leaf_Mean_External")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_3Cwarmer", "LLT_Bean.Leaf_Mean_3Cwarmer")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_5Cwarmer", "LLT_Bean.Leaf_Mean_5Cwarmer")
  
  # write.csv("")
  
}

#External
write.csv(scpdata, "Comparing LLTime with days under SCP_external.csv", row.names = FALSE)

external <- scpdata

# 3C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_3Cwarmer.csv", row.names = FALSE)

H3 <- scpdata

# 5C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_5Cwarmer.csv", row.names = FALSE)

H5 <- scpdata


#### Long form data used in summarizing
external.summary <- Summarize(SCP_Bean.Leaf_Mean_External ~ LLT_Bean.Leaf_Mean_External, data = external)
external.summary

H3.summary <- Summarize(SCP_Bean.Leaf_Mean_3Cwarmer ~ LLT_Bean.Leaf_Mean_3Cwarmer, data = H3)
H3.summary

H5.summary <- Summarize(SCP_Bean.Leaf_Mean_5Cwarmer ~ LLT_Bean.Leaf_Mean_5Cwarmer, data = H5)
H5.summary




############################################################################################################

# Reading in dataframes - for worst case if starting from here just to make plots
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
external <- read.csv("Comparing LLTime with days under SCP_external.csv", header = TRUE)
H3 <- read.csv("Comparing LLTime with days under SCP_3Cwarmer.csv", header = TRUE)
H5 <- read.csv("Comparing LLTime with days under SCP_5Cwarmer.csv", header = TRUE)

library(viridis)

### Updated plotting - 1st each treatment without legends ###
external_graph <- ggplot() +
  geom_density(data=external, aes(x=SCP_Bean.Leaf_Mean_External, fill = as.factor(LLT_Bean.Leaf_Mean_External)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  # theme(legend.text = element_text(size=14)) +
  # theme(legend.title = element_blank()) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_External.jpg", plot=external_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H3_graph <- ggplot() +
  geom_density(data=H3, aes(x=SCP_Bean.Leaf_Mean_3Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_3Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_3Cwarmer.jpg", plot=H3_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_5Cwarmer.jpg", plot=H5_graph, device = "jpeg",
       width=7, height=5, dpi=600)



###################################
####                           ####
####      best case scenario   ####
###################################

# Reading in dataframes - for best case
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
external <- read.csv("Comparing LLTime with days under SCP_external - best case.csv", header = TRUE)
H3 <- read.csv("Comparing LLTime with days under SCP_3Cwarmer - best case.csv", header = TRUE)
H5 <- read.csv("Comparing LLTime with days under SCP_5Cwarmer - best case.csv", header = TRUE)

# Based on the density plots define the order in which the treatments should be plotted so that they're 
# most clear
external$LLT_Bean.Leaf_Mean_External <- factor(external$LLT_Bean.Leaf_Mean_External,
                                               ordered = TRUE,
                                               levels = c("1", "2", "0"))

H3$LLT_Bean.Leaf_Mean_3Cwarmer <- factor(H3$LLT_Bean.Leaf_Mean_3Cwarmer,
                                         ordered = TRUE,
                                         levels = c("1", "2", "0"))

H5$LLT_Bean.Leaf_Mean_5Cwarmer <- factor(H5$LLT_Bean.Leaf_Mean_5Cwarmer,
                                         ordered = TRUE,
                                         levels = c("1", "2", "0"))

external_graph <- ggplot() +
  geom_density(data=external, aes(x=SCP_Bean.Leaf_Mean_External, fill = as.factor(LLT_Bean.Leaf_Mean_External)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_External - best case.jpg", plot=external_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H3_graph <- ggplot() +
  geom_density(data=H3, aes(x=SCP_Bean.Leaf_Mean_3Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_3Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_3Cwarmer - best case.jpg", plot=H3_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_5Cwarmer - best case.jpg", plot=H5_graph, device = "jpeg",
       width=7, height=5, dpi=600)


###################################
####                           ####
####        LEGEND             ####
###################################

H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm")) +
  theme(legend.text = element_text(size=14)) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LEGEND.jpg", plot=H5_graph, device = "jpeg",
       width=9, height=5, dpi=600)

=======
######################################################################################
###########      Comparing the vulnerability shown by SCP results     ################
###########         with the vulnerability shown by LLT results       ################
###########                   for green leaf beetles                  ################
######################################################################################

# Author: Kimberly Thompson

# This script takes the rasters that show the longest consecutive days with ground
# temperatures at or below 0C and reclassifies them based on three categories:
# below range for 50% mortality, within range for 50% mortality, and above 
# range for 50% mortality for Bean leaf beetles.

# Comparison between LLT and SCP based on worst-case scenario for SCP.

# This is done for consecutive 0C days based on mean, median, min, and max ground
# temperatures; however, only the mean was used in the manuscript.


# Datasets produced:
# 'Comparing LLTime with days under SCP_external - worst case.csv'
# 'Comparing LLTime with days under SCP_3Cwarmer - worst case.csv'
# 'Comparing LLTime with days under SCP_5Cwarmer - worst case.csv'


# Figures produced:
# Figure 5
# Figure S12

# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(raster)
library(sp)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)


# Workflow - for each scenario
# 1. Reclassify LLT raster to three values (based on green yellow and red in map, and the LLT values extracted from
#      Lam and Pedigo (2000).

# 2. Then use the SCP raster to extract the number of SCP days - essentially create a dataframe for each of the 
#      scenarios.



#########################################################################
#############              Changing Cell values          ################
#########################################################################

# Green leaf beetle - From Lam and Pedigo 2000, lower confidence limit = 28.87259, upper confidence limit = 41.32808

# From the maps of most consecutive days of below 0C temperatures, resample so that:
#     1. < 28.87259 the cell gets a 0
#     2. >= 28.87259 and <= 41.32808 the cell gets a 1
#     3. > 41.32808 the cell gets a 2



########################################
####    Interior Lakes Data          ###
########################################

#### Read in the separating lakes file ###
setwd("01_Analysis/Spatial Predictions")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities")
separating.lakes <- read.csv("Separating Lakes.csv", header = TRUE)


#############################
###  Raster Loading    ######
#############################

#### LLT50 AT 0C RASTERS ######

# I have four summary measures # of consecutive stretches of below 0C temps for (mean, median, minimum, and maximum) 
# of the 50 bootstrapped values

#### I initially made these maps to apply to Bombus, but they are not specific to Bombus yet since all they show are the longest
#### consecutive days during the winter season that the temperature was at or below 0C. So I can use these same ones for Bean Leaf and
#### just change the day threshold/raster names further down in the code.

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Bumblebees")

dur0_mean <- raster :: raster("LLT50_0Cdays_Mean_External.tif")
dur3_mean <- raster :: raster("LLT50_0Cdays_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("LLT50_0Cdays_Mean_5C warmer.tif")

dur0_median <- raster :: raster("LLT50_0Cdays_Median_External.tif")
dur3_median <- raster :: raster("LLT50_0Cdays_Median_3C warmer.tif")
dur5_median <- raster :: raster("LLT50_0Cdays_Median_5C warmer.tif")

dur0_min <- raster :: raster("LLT50_0Cdays_Minimum_External.tif")
dur3_min <- raster :: raster("LLT50_0Cdays_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("LLT50_0Cdays_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("LLT50_0Cdays_Maximum_External.tif")
dur3_max <- raster :: raster("LLT50_0Cdays_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("LLT50_0Cdays_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_llt <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                    dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)

# Change the name to Bean leaf so it saves correctly
names(rasterlist_llt) <- gsub("Bombus", "Bean.Leaf.Beetle", names(rasterlist_llt))



#############################
###  SCP Raster Loading   ###
#############################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Bean Leaf")
# setwd("Y:/kthompson/Ch 4 Bumblebees/Spatial Predictions/Species Sensitivities/Green Leaf Beetle/Worst Case")

dur0_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_External.tif")
dur3_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_3C warmer.tif")
dur5_mean <- raster :: raster("SCP_Bean Leaf_Worst Case_Mean_5C warmer.tif")

dur0_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_External.tif")
dur3_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_3C warmer.tif")
dur5_median <- raster :: raster("SCP_Bean Leaf_Worst Case_Median_5C warmer.tif")

dur0_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_External.tif")
dur3_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_3C warmer.tif")
dur5_min <- raster :: raster("SCP_Bean Leaf_Worst Case_Minimum_5C warmer.tif")

dur0_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_External.tif")
dur3_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_3C warmer.tif")
dur5_max <- raster :: raster("SCP_Bean Leaf_Worst Case_Maximum_5C warmer.tif")


# Make a stack of the rasters
rasterlist_scp <- stack(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
                        dur0_max, dur3_max, dur5_max)

# Remove the single raster layers for a cleaner workspace
rm(dur0_mean, dur3_mean, dur5_mean, dur0_median, dur3_median, dur5_median, dur0_min, dur3_min, dur5_min,
   dur0_max, dur3_max, dur5_max)



### Getting read for loop #########

wgs84 = sp:: CRS("+init=epsg:4326")

# To help with the raster processing
# Load in the states shapefile
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 2 Climate/Analysis/GIS")
states<-rgdal::readOGR("Great_Lakes_States.shp" )


#### Make adjustments to both raster stacks so I calculate the summary stats for only the extent I'll be presenting in the figures

# transform each raster into degrees so that the axes will look
# better and be clearer to understand

rasterlist_llt <- projectRaster(from=rasterlist_llt, crs =  wgs84)
rasterlist_scp <- projectRaster(from=rasterlist_scp, crs =  wgs84)

# Convert projection of states so that it matches rasters:
states <- spTransform( states, proj4string( rasterlist_llt ) )

# Define the extent for the states
newstates <- raster::crop(states, extent(-97.2291, -75.65702, 41.5, 49.37173))

# Crop the rasters to match
rasterlist_llt <- crop(rasterlist_llt, extent(newstates))
rasterlist_llt <- raster :: mask(rasterlist_llt, newstates)

rasterlist_scp <- crop(rasterlist_scp, extent(newstates))
rasterlist_scp <- raster :: mask(rasterlist_scp, newstates)

  
## Reclassifying the rastet 
for (i in 1:nlayers(rasterlist_llt)) { 
  
  # Reclassify the raster
  rasterlist_llt[[i]][rasterlist_llt[[i]] < 28.87259] <- 0
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] >= 28.87259 & rasterlist_llt[[i]] <= 41.32808] <- 1
  
  rasterlist_llt[[i]][rasterlist_llt[[i]] > 41.32808] <- 2
  
  print(i)
  
}



#### Loop to create a dataframe of days under SCP that also shows whether the cells falls into below, between, or above the
#### days until LLT50.

# This loop is not actually finished! I needed to accelerate the process so only did if for the mean and then manually wrote
# the csv files

setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")

for (i in 1:length(rasterlist_llt)) { # to do just the mean maps which is what I'm presenting just say 1:3 rather than
  # length(rasterlist_llt)
  
  # Convert the LLT raster to a dataframe
  lltdata <- as.data.frame(rasterlist_llt[[i]], xy=TRUE)
  
  # Convert the SCP raster to a dataframe
  scpdata <- as.data.frame(rasterlist_scp[[i]], xy=TRUE)
  
  
  # Add the LLT info to the SCP dataframe
  scpdata <- cbind(scpdata, lltdata[, 3])
  
  # Rename the columns
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_External", "LLT_Bean.Leaf_Mean_External")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_3Cwarmer", "LLT_Bean.Leaf_Mean_3Cwarmer")
  names(scpdata) <- c("x", "y", "SCP_Bean.Leaf_Mean_5Cwarmer", "LLT_Bean.Leaf_Mean_5Cwarmer")
  
  # write.csv("")
  
}

#External
write.csv(scpdata, "Comparing LLTime with days under SCP_external.csv", row.names = FALSE)

external <- scpdata

# 3C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_3Cwarmer.csv", row.names = FALSE)

H3 <- scpdata

# 5C warmer
write.csv(scpdata, "Comparing LLTime with days under SCP_5Cwarmer.csv", row.names = FALSE)

H5 <- scpdata


#### Long form data used in summarizing
external.summary <- Summarize(SCP_Bean.Leaf_Mean_External ~ LLT_Bean.Leaf_Mean_External, data = external)
external.summary

H3.summary <- Summarize(SCP_Bean.Leaf_Mean_3Cwarmer ~ LLT_Bean.Leaf_Mean_3Cwarmer, data = H3)
H3.summary

H5.summary <- Summarize(SCP_Bean.Leaf_Mean_5Cwarmer ~ LLT_Bean.Leaf_Mean_5Cwarmer, data = H5)
H5.summary




############################################################################################################

# Reading in dataframes - for worst case if starting from here just to make plots
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
external <- read.csv("Comparing LLTime with days under SCP_external.csv", header = TRUE)
H3 <- read.csv("Comparing LLTime with days under SCP_3Cwarmer.csv", header = TRUE)
H5 <- read.csv("Comparing LLTime with days under SCP_5Cwarmer.csv", header = TRUE)

library(viridis)

### Updated plotting - 1st each treatment without legends ###
external_graph <- ggplot() +
  geom_density(data=external, aes(x=SCP_Bean.Leaf_Mean_External, fill = as.factor(LLT_Bean.Leaf_Mean_External)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  # theme(legend.text = element_text(size=14)) +
  # theme(legend.title = element_blank()) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_External.jpg", plot=external_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H3_graph <- ggplot() +
  geom_density(data=H3, aes(x=SCP_Bean.Leaf_Mean_3Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_3Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_3Cwarmer.jpg", plot=H3_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LLT and SCP comparison_Bean Leaf_5Cwarmer.jpg", plot=H5_graph, device = "jpeg",
       width=7, height=5, dpi=600)



###################################
####                           ####
####      best case scenario   ####
###################################

# Reading in dataframes - for best case
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
setwd("01_Analysis/Spatial Predictions/LLT50")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Data/Comparing LLT with days under SCP/Bean Leaf Beetle")
external <- read.csv("Comparing LLTime with days under SCP_external - best case.csv", header = TRUE)
H3 <- read.csv("Comparing LLTime with days under SCP_3Cwarmer - best case.csv", header = TRUE)
H5 <- read.csv("Comparing LLTime with days under SCP_5Cwarmer - best case.csv", header = TRUE)

# Based on the density plots define the order in which the treatments should be plotted so that they're 
# most clear
external$LLT_Bean.Leaf_Mean_External <- factor(external$LLT_Bean.Leaf_Mean_External,
                                               ordered = TRUE,
                                               levels = c("1", "2", "0"))

H3$LLT_Bean.Leaf_Mean_3Cwarmer <- factor(H3$LLT_Bean.Leaf_Mean_3Cwarmer,
                                         ordered = TRUE,
                                         levels = c("1", "2", "0"))

H5$LLT_Bean.Leaf_Mean_5Cwarmer <- factor(H5$LLT_Bean.Leaf_Mean_5Cwarmer,
                                         ordered = TRUE,
                                         levels = c("1", "2", "0"))

external_graph <- ggplot() +
  geom_density(data=external, aes(x=SCP_Bean.Leaf_Mean_External, fill = as.factor(LLT_Bean.Leaf_Mean_External)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_External - best case.jpg", plot=external_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H3_graph <- ggplot() +
  geom_density(data=H3, aes(x=SCP_Bean.Leaf_Mean_3Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_3Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_3Cwarmer - best case.jpg", plot=H3_graph, device = "jpeg",
       width=7, height=5, dpi=600)


H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.175),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
# theme(legend.text = element_text(size=14)) +
# theme(legend.title = element_blank()) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure S12")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP")
ggsave("LLT and SCP comparison_Bean Leaf_5Cwarmer - best case.jpg", plot=H5_graph, device = "jpeg",
       width=7, height=5, dpi=600)


###################################
####                           ####
####        LEGEND             ####
###################################

H5_graph <- ggplot() +
  geom_density(data=H5, aes(x=SCP_Bean.Leaf_Mean_5Cwarmer, fill = as.factor(LLT_Bean.Leaf_Mean_5Cwarmer)),
               alpha = 0.5, position = "identity", color = "grey3") +
  scale_x_continuous(name="\nDays under supercooling point", limits = c(0, 80),
                     breaks = c(0, 20, 40, 60, 80),
                     labels = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name="Density\n", limits = c(0, 0.108),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     labels = c("0.000", "0.025", "0.050", "0.075", "0.100")) +
  scale_fill_manual(name='', values = c('0' = mako(100)[80],
                                        '1' = inferno(100)[90],
                                        '2' = inferno(100)[50]),
                    labels = c(" Below mortality range", " Within mortality range", " Above mortality range")) +
  # theme(aspect.ratio=1) + #aspect ratio expressed as y/x
  # theme(legend.position = c(0.85, 0.60)) +
  # theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm")) +
  theme(legend.text = element_text(size=14)) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 5")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Comparing LLT and SCP/Worst Case")
ggsave("LEGEND.jpg", plot=H5_graph, device = "jpeg",
       width=9, height=5, dpi=600)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
