<<<<<<< HEAD
#################################################################################
#########                                                               #########
#########             Figures for extent of vulnerability               #########
#########                                                               #########
#################################################################################

# Author: Kimberly Thompson

# This code takes calculations of the number of square kms where the temperature
# was below the supercooling point of each insect (in both the best and worst 
# case scenarios) and creates figures showing how the extent of vulnerability 
# (i.e., exposure to sub-lethal temperatures) changes throughout the winter 
# season, given the conditions we observed.

# Jpegs created used to make figures:
# Figure 3
# Figure S10


# clean workspace to improve efficiency: #
rm(list = ls() ) 
gc() #releases memory

library(ggplot2) 
library(viridis)
library(scales)


#############################################
####                                     ####
####          Data Loading               ####
####                                     ####
#############################################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")

best <- list.files(getwd(), pattern = "Best")
worst <- list.files(getwd(), pattern = "Worst")


# Best case scenarios for each insect
for (i in 1:length(best)) {
  
  # Set working directory
  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  
  # Read in the file
  test <- read.csv(best[i], header = TRUE)
  
  # ID/day index column for x-axis
  test$ID <- seq(1, 121, by = 1)
  
  # Reorder the factor Type to adjust which gets plotted first, then others in front of that
  test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))

  # Make the plot
  # I could not get geom_area to stop summing the values across types
  test.plot <- ggplot() +
    geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
              alpha = 0.9) +
    geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                               fill = as.factor(Type)),
              alpha = 0.5) +
    # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
    #           alpha = 0.7) +
    scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                       limits = c(0, 1000000),
                       labels = scales::comma) +
    scale_x_continuous(limits = c(1, 121), name="Date",
                       breaks=c(1, 32, 63, 91, 121),
                       labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
    scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                          'H3' = viridis(100)[50],
                                          'H5' = viridis(100)[85]),
                      labels = c(" Current conditions", " +3°C", " +5°C")) +
    theme_bw() +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  
  setwd("01_Analysis/Figure S10")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Best_", strsplit(best[i], "_")[[1]][1], ".jpg", sep = ""),
               plot = test.plot, device = "jpeg",
                width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


# Worst case scenarios for each insect
for (i in 1:length(worst)) {
  
  # Set working directory
  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  
  # Read in the file
  test <- read.csv(worst[i], header = TRUE)
  
  # ID/day index column for x-axis
  test$ID <- seq(1, 121, by = 1)
  
  # Reorder the factor Type to adjust which gets plotted first, then others in front of that
  test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))
  
  # if(i == 1 | i == 2 | i == 5 | i == 6 | i == 7) { # bean leaf or bumblebee or woolly (butterflies don't matter)
  # test$Type <- factor(test$Type, ordered = TRUE, levels = c("external", "H5", "H3"))
  # } else {
  #   if(i ==3 | i == 4) { # diamondback or hoverfly
  #     test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))
  #   }
  # }
  
  # Make the plot
  # I could not get geom_area to stop summing the values across types
  test.plot <- ggplot() +
    geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
              alpha = 0.9) +
    geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                                 fill = as.factor(Type)),
                alpha = 0.5) +
    # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
    #           alpha = 0.7) +
    scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                       limits = c(0, 1400000),
                       breaks = c(0, 400000, 800000, 1200000),
                       labels = scales::comma) +
    scale_x_continuous(limits = c(1, 121), name="Date",
                       breaks=c(1, 32, 63, 91, 121),
                       labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
    scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                          'H3' = viridis(100)[50],
                                          'H5' = viridis(100)[85]),
                      labels = c(" Current conditions", " +3°C", " +5°C")) +
    theme_bw() +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  
  setwd("01_Analysis/Figure 3")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Worst_", strsplit(worst[i], "_")[[1]][1], ".jpg", sep = ""),
         plot = test.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


#####################
### Legend        ###
#####################

# One plot with the legend so it can be laid out well for the paper
test.plot <- ggplot() +
  geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
            alpha = 0.9) +
  geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                               fill = as.factor(Type)),
              alpha = 0.5) +
  # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
  #           alpha = 0.7) +
  scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                     limits = c(0, 1400000),
                     breaks = c(0, 400000, 800000, 1200000),
                     labels = scales::comma) +
  scale_x_continuous(limits = c(1, 121), name="Date",
                     breaks=c(1, 32, 63, 91, 121),
                     labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
  scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                         'H3' = viridis(100)[50],
                                         'H5' = viridis(100)[85]),
                     labels = c(" Current conditions", " +3°C", " +5°C")) +
  scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                        'H3' = viridis(100)[50],
                                        'H5' = viridis(100)[85]),
                    labels = c(" Current conditions", " +3°C", " +5°C")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm")) +
  theme(legend.text = element_text(size=18)) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 3")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
# setwd("C:/Users/ff69upeb/Desktop/Test plots")
ggsave("Legend.jpg",
       plot = test.plot, device = "jpeg",
       width = 7, height = 5, dpi = 600)





=======
#################################################################################
#########                                                               #########
#########             Figures for extent of vulnerability               #########
#########                                                               #########
#################################################################################

# Author: Kimberly Thompson

# This code takes calculations of the number of square kms where the temperature
# was below the supercooling point of each insect (in both the best and worst 
# case scenarios) and creates figures showing how the extent of vulnerability 
# (i.e., exposure to sub-lethal temperatures) changes throughout the winter 
# season, given the conditions we observed.

# Jpegs created used to make figures:
# Figure 3
# Figure S10


# clean workspace to improve efficiency: #
rm(list = ls() ) 
gc() #releases memory

library(ggplot2) 
library(viridis)
library(scales)


#############################################
####                                     ####
####          Data Loading               ####
####                                     ####
#############################################

setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")

best <- list.files(getwd(), pattern = "Best")
worst <- list.files(getwd(), pattern = "Worst")


# Best case scenarios for each insect
for (i in 1:length(best)) {
  
  # Set working directory
  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  
  # Read in the file
  test <- read.csv(best[i], header = TRUE)
  
  # ID/day index column for x-axis
  test$ID <- seq(1, 121, by = 1)
  
  # Reorder the factor Type to adjust which gets plotted first, then others in front of that
  test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))

  # Make the plot
  # I could not get geom_area to stop summing the values across types
  test.plot <- ggplot() +
    geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
              alpha = 0.9) +
    geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                               fill = as.factor(Type)),
              alpha = 0.5) +
    # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
    #           alpha = 0.7) +
    scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                       limits = c(0, 1000000),
                       labels = scales::comma) +
    scale_x_continuous(limits = c(1, 121), name="Date",
                       breaks=c(1, 32, 63, 91, 121),
                       labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
    scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                          'H3' = viridis(100)[50],
                                          'H5' = viridis(100)[85]),
                      labels = c(" Current conditions", " +3°C", " +5°C")) +
    theme_bw() +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  
  setwd("01_Analysis/Figure S10")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Best_", strsplit(best[i], "_")[[1]][1], ".jpg", sep = ""),
               plot = test.plot, device = "jpeg",
                width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


# Worst case scenarios for each insect
for (i in 1:length(worst)) {
  
  # Set working directory
  setwd("01_Analysis/Spatial Predictions/Species Vulnerability/Extent")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/Spatial Predictions/Species Sensitivities/Y - Extent Analysis")
  
  # Read in the file
  test <- read.csv(worst[i], header = TRUE)
  
  # ID/day index column for x-axis
  test$ID <- seq(1, 121, by = 1)
  
  # Reorder the factor Type to adjust which gets plotted first, then others in front of that
  test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))
  
  # if(i == 1 | i == 2 | i == 5 | i == 6 | i == 7) { # bean leaf or bumblebee or woolly (butterflies don't matter)
  # test$Type <- factor(test$Type, ordered = TRUE, levels = c("external", "H5", "H3"))
  # } else {
  #   if(i ==3 | i == 4) { # diamondback or hoverfly
  #     test$Type <- factor(test$Type, ordered = TRUE, levels = c("H5", "external", "H3"))
  #   }
  # }
  
  # Make the plot
  # I could not get geom_area to stop summing the values across types
  test.plot <- ggplot() +
    geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
              alpha = 0.9) +
    geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                                 fill = as.factor(Type)),
                alpha = 0.5) +
    # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
    #           alpha = 0.7) +
    scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                       limits = c(0, 1400000),
                       breaks = c(0, 400000, 800000, 1200000),
                       labels = scales::comma) +
    scale_x_continuous(limits = c(1, 121), name="Date",
                       breaks=c(1, 32, 63, 91, 121),
                       labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
    scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                          'H3' = viridis(100)[50],
                                          'H5' = viridis(100)[85]),
                      labels = c(" Current conditions", " +3°C", " +5°C")) +
    theme_bw() +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"))
  
  setwd("01_Analysis/Figure 3")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Worst_", strsplit(worst[i], "_")[[1]][1], ".jpg", sep = ""),
         plot = test.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


#####################
### Legend        ###
#####################

# One plot with the legend so it can be laid out well for the paper
test.plot <- ggplot() +
  geom_line(data = test, aes(x = ID, y = value, color = as.factor(Type)),
            alpha = 0.9) +
  geom_ribbon(data = test, aes(x = ID, y = value, ymin = 0, ymax = value,
                               fill = as.factor(Type)),
              alpha = 0.5) +
  # geom_area(data = test, aes(x = ID, y = value, fill = as.factor(Type)),
  #           alpha = 0.7) +
  scale_y_continuous(name = expression("Vulnerability (km"^2*")"),
                     limits = c(0, 1400000),
                     breaks = c(0, 400000, 800000, 1200000),
                     labels = scales::comma) +
  scale_x_continuous(limits = c(1, 121), name="Date",
                     breaks=c(1, 32, 63, 91, 121),
                     labels=c("01-Dec", "01-Jan", "01-Feb", "01-Mar", "31-Mar")) +
  scale_color_manual(name='', values = c('external' = viridis(100)[10],
                                         'H3' = viridis(100)[50],
                                         'H5' = viridis(100)[85]),
                     labels = c(" Current conditions", " +3°C", " +5°C")) +
  scale_fill_manual(name='', values = c('external' = viridis(100)[10],
                                        'H3' = viridis(100)[50],
                                        'H5' = viridis(100)[85]),
                    labels = c(" Current conditions", " +3°C", " +5°C")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="black")) + 
  theme(plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm")) +
  theme(legend.text = element_text(size=18)) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

setwd("01_Analysis/Figure 3")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Extent of Vulnerability")
# setwd("C:/Users/ff69upeb/Desktop/Test plots")
ggsave("Legend.jpg",
       plot = test.plot, device = "jpeg",
       width = 7, height = 5, dpi = 600)





>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
