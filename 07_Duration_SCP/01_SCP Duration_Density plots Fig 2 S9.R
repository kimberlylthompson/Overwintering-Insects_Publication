<<<<<<< HEAD
#########################################################################
###########                                              ################
###########         Density plots showing number of      ################
###########       days under the supercooling point      ################
###########              for each species                ################
#########################################################################

# Author: Kimberly Thompson

# This code creates density plots with overlapping histograms for each 
# warming scenario to depict the days under supercooling point for each
# species.

# 1 plot for each of 7 species, but the two butterfly species didn't have 
# any days under their supercooling points.

# Figures produced:
# Figure 2


# Clear workspace and load required packages
rm(list = ls() ) 
gc() #releases memory


library(ggplot2)
library(viridis)


#####################################
###                               ###
###         Data Loading          ###
###                               ###
#####################################

setwd("01_Analysis/Summary_Results")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")

# Best and Worst Case Scenario summaries
best.case <- read.csv("Best Case SCP Summary.csv", header = TRUE)
worst.case <- read.csv("Worst Case SCP Summary.csv", header = TRUE)

# List the files for best and worst case for each bug
best <- list.files(getwd(), pattern = "Bestcase")
worst <- list.files(getwd(), pattern = "Worstcase")


#####################################
###                               ###
###          Plotting             ###
###                               ###
#####################################

# Worst case
for (i in 1:length(worst)) {
  
  # Read in the data
  
  setwd("01_Analysis/Summary_Results")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  tmp.df <- read.csv(worst[i], header = TRUE)
  
  # Rename Treatment values to aid in plotting
  tmp.df$Treatment2 <- NA
  
  # For tmp.df
  for (j in 1:length(unique(tmp.df$Treatment))) {
    
    full.name <- unique(tmp.df$Treatment)
    split.name <- strsplit(unique(tmp.df$Treatment)[j], "_")[[1]]
    
    if(split.name[length(split.name) - 1] == "Mean") { # This corresponds to external
      tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "External"
    } else {
      if(split.name[length(split.name) - 1] == "3C") {
        tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H3"
      } else {
        if(split.name[length(split.name) - 1] == "5C") {
          tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H5"
        }
      }
    }
  }
  
  # Filter the summary dataframe according to the particular bug
  # (summary and full dfs go in the same order)
  tmp.summary <- worst.case[c((i*3-2), (i*3-1), (i*3)), ]
  
  # Rename Treatment values to aid in plotting for tmp.summary
  # These are easier because they are in order
  tmp.summary$Treatment2 <- c("External", "H3", "H5")
  
  # Make the density plot
  tmp.plot <- ggplot() +
    geom_density(data = tmp.df,
                 aes(x=Duration, fill = as.factor(Treatment2)),
                 alpha=0.7, position = "identity", color = "grey3") +
    geom_vline(data = tmp.summary,
               aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
               size = 2, alpha = 0.8) +
    scale_y_continuous(name = "Density", limits = c(0, 0.15)) +
    scale_x_continuous(limits = c(0, 121), name="Number of Days",
                       breaks=c(0, 30, 60, 90, 121)) +
    scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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
  
  
  setwd("01_Analysis/Figure 2")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Worst_SCPdist_", strsplit(tmp.df$Treatment[1], "_")[[1]][2], ".jpg", sep = ""),
         plot = tmp.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


# Best case
for (i in 1:length(best)) {
  
  # Read in the data
  setwd("01_Analysis/Summary_Results")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  tmp.df <- read.csv(best[i], header = TRUE)
  
  # Rename Treatment values to aid in plotting
  tmp.df$Treatment2 <- NA
  
  # For tmp.df
  for (j in 1:length(unique(tmp.df$Treatment))) {
    
    full.name <- unique(tmp.df$Treatment)
    split.name <- strsplit(unique(tmp.df$Treatment)[j], "_")[[1]]
    
    if(split.name[length(split.name) - 1] == "Mean") { # This corresponds to external
      tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "External"
    } else {
      if(split.name[length(split.name) - 1] == "3C") {
        tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H3"
      } else {
        if(split.name[length(split.name) - 1] == "5C") {
          tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H5"
        }
      }
    }
  }
  
  # Filter the summary dataframe according to the particular bug
  # (summary and full dfs go in the same order)
  tmp.summary <- best.case[c((i*3-2), (i*3-1), (i*3)), ]
  
  # Rename Treatment values to aid in plotting for tmp.summary
  # These are easier because they are in order
  tmp.summary$Treatment2 <- c("External", "H3", "H5")
  
  # Make the density plot
  tmp.plot <- ggplot() +
    geom_density(data = tmp.df,
                 aes(x=Duration, fill = as.factor(Treatment2)),
                 alpha=0.7, position = "identity", color = "grey3") +
    geom_vline(data = tmp.summary,
               aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
               size = 2, alpha = 0.8) +
    scale_y_continuous(name = "Density", limits = c(0, 0.20)) +
    scale_x_continuous(limits = c(0, 121), name="Number of Days",
                       breaks=c(0, 30, 60, 90, 121)) +
    scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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
  
  setwd("01_Analysis/Figure S9")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Best_SCPdist_", strsplit(tmp.df$Treatment[1], "_")[[1]][2], ".jpg", sep = ""),
         plot = tmp.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}



#################################
####     Legend             #####
#################################

# One plot with the legend so it can be laid out well for the paper
tmp.plot <- ggplot() +
  geom_density(data = tmp.df,
               aes(x=Duration, fill = as.factor(Treatment2)),
               alpha=0.7, position = "identity", color = "grey3") +
  geom_vline(data = tmp.summary,
             aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
             size = 2, alpha = 0.8) +
  scale_y_continuous(name = "Density", limits = c(0, 0.20)) +
  scale_x_continuous(limits = c(0, 121), name="Number of Days",
                     breaks=c(0, 30, 60, 90, 121)) +
  scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                         'H3' = viridis(100)[50],
                                         'H5' = viridis(100)[85]),
                     labels = c(" Current conditions", " +3°C", " +5°C")) +
  scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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

setwd("01_Analysis/Figure 2")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
# setwd("C:/Users/ff69upeb/Desktop/Test plots")
ggsave("Legend.jpg",
       plot = tmp.plot, device = "jpeg",
       width = 7, height = 5, dpi = 600)

=======
#########################################################################
###########                                              ################
###########         Density plots showing number of      ################
###########       days under the supercooling point      ################
###########              for each species                ################
#########################################################################

# Author: Kimberly Thompson

# This code creates density plots with overlapping histograms for each 
# warming scenario to depict the days under supercooling point for each
# species.

# 1 plot for each of 7 species, but the two butterfly species didn't have 
# any days under their supercooling points.

# Figures produced:
# Figure 2


# Clear workspace and load required packages
rm(list = ls() ) 
gc() #releases memory


library(ggplot2)
library(viridis)


#####################################
###                               ###
###         Data Loading          ###
###                               ###
#####################################

setwd("01_Analysis/Summary_Results")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")

# Best and Worst Case Scenario summaries
best.case <- read.csv("Best Case SCP Summary.csv", header = TRUE)
worst.case <- read.csv("Worst Case SCP Summary.csv", header = TRUE)

# List the files for best and worst case for each bug
best <- list.files(getwd(), pattern = "Bestcase")
worst <- list.files(getwd(), pattern = "Worstcase")


#####################################
###                               ###
###          Plotting             ###
###                               ###
#####################################

# Worst case
for (i in 1:length(worst)) {
  
  # Read in the data
  
  setwd("01_Analysis/Summary_Results")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  tmp.df <- read.csv(worst[i], header = TRUE)
  
  # Rename Treatment values to aid in plotting
  tmp.df$Treatment2 <- NA
  
  # For tmp.df
  for (j in 1:length(unique(tmp.df$Treatment))) {
    
    full.name <- unique(tmp.df$Treatment)
    split.name <- strsplit(unique(tmp.df$Treatment)[j], "_")[[1]]
    
    if(split.name[length(split.name) - 1] == "Mean") { # This corresponds to external
      tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "External"
    } else {
      if(split.name[length(split.name) - 1] == "3C") {
        tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H3"
      } else {
        if(split.name[length(split.name) - 1] == "5C") {
          tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H5"
        }
      }
    }
  }
  
  # Filter the summary dataframe according to the particular bug
  # (summary and full dfs go in the same order)
  tmp.summary <- worst.case[c((i*3-2), (i*3-1), (i*3)), ]
  
  # Rename Treatment values to aid in plotting for tmp.summary
  # These are easier because they are in order
  tmp.summary$Treatment2 <- c("External", "H3", "H5")
  
  # Make the density plot
  tmp.plot <- ggplot() +
    geom_density(data = tmp.df,
                 aes(x=Duration, fill = as.factor(Treatment2)),
                 alpha=0.7, position = "identity", color = "grey3") +
    geom_vline(data = tmp.summary,
               aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
               size = 2, alpha = 0.8) +
    scale_y_continuous(name = "Density", limits = c(0, 0.15)) +
    scale_x_continuous(limits = c(0, 121), name="Number of Days",
                       breaks=c(0, 30, 60, 90, 121)) +
    scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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
  
  
  setwd("01_Analysis/Figure 2")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Worst_SCPdist_", strsplit(tmp.df$Treatment[1], "_")[[1]][2], ".jpg", sep = ""),
         plot = tmp.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}


# Best case
for (i in 1:length(best)) {
  
  # Read in the data
  setwd("01_Analysis/Summary_Results")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Results/SCP Distribution summary stats")
  tmp.df <- read.csv(best[i], header = TRUE)
  
  # Rename Treatment values to aid in plotting
  tmp.df$Treatment2 <- NA
  
  # For tmp.df
  for (j in 1:length(unique(tmp.df$Treatment))) {
    
    full.name <- unique(tmp.df$Treatment)
    split.name <- strsplit(unique(tmp.df$Treatment)[j], "_")[[1]]
    
    if(split.name[length(split.name) - 1] == "Mean") { # This corresponds to external
      tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "External"
    } else {
      if(split.name[length(split.name) - 1] == "3C") {
        tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H3"
      } else {
        if(split.name[length(split.name) - 1] == "5C") {
          tmp.df$Treatment2[tmp.df$Treatment == full.name[j]] <- "H5"
        }
      }
    }
  }
  
  # Filter the summary dataframe according to the particular bug
  # (summary and full dfs go in the same order)
  tmp.summary <- best.case[c((i*3-2), (i*3-1), (i*3)), ]
  
  # Rename Treatment values to aid in plotting for tmp.summary
  # These are easier because they are in order
  tmp.summary$Treatment2 <- c("External", "H3", "H5")
  
  # Make the density plot
  tmp.plot <- ggplot() +
    geom_density(data = tmp.df,
                 aes(x=Duration, fill = as.factor(Treatment2)),
                 alpha=0.7, position = "identity", color = "grey3") +
    geom_vline(data = tmp.summary,
               aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
               size = 2, alpha = 0.8) +
    scale_y_continuous(name = "Density", limits = c(0, 0.20)) +
    scale_x_continuous(limits = c(0, 121), name="Number of Days",
                       breaks=c(0, 30, 60, 90, 121)) +
    scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                           'H3' = viridis(100)[50],
                                           'H5' = viridis(100)[85]),
                       labels = c(" Current conditions", " +3°C", " +5°C")) +
    scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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
  
  setwd("01_Analysis/Figure S9")
  # setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
  # setwd("C:/Users/ff69upeb/Desktop/Test plots")
  ggsave(paste("Best_SCPdist_", strsplit(tmp.df$Treatment[1], "_")[[1]][2], ".jpg", sep = ""),
         plot = tmp.plot, device = "jpeg",
         width = 7, height = 5, dpi = 600)
  
  print(i)
  
}



#################################
####     Legend             #####
#################################

# One plot with the legend so it can be laid out well for the paper
tmp.plot <- ggplot() +
  geom_density(data = tmp.df,
               aes(x=Duration, fill = as.factor(Treatment2)),
               alpha=0.7, position = "identity", color = "grey3") +
  geom_vline(data = tmp.summary,
             aes(xintercept = mean, color = as.factor(Treatment2)), linetype="dashed",
             size = 2, alpha = 0.8) +
  scale_y_continuous(name = "Density", limits = c(0, 0.20)) +
  scale_x_continuous(limits = c(0, 121), name="Number of Days",
                     breaks=c(0, 30, 60, 90, 121)) +
  scale_color_manual(name='', values = c('External' = viridis(100)[10],
                                         'H3' = viridis(100)[50],
                                         'H5' = viridis(100)[85]),
                     labels = c(" Current conditions", " +3°C", " +5°C")) +
  scale_fill_manual(name='', values = c('External' = viridis(100)[10],
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

setwd("01_Analysis/Figure 2")
# setwd("G:/My Drive/Ch 4 Bumblebees/Analysis/Graphs/Density Plots")
# setwd("C:/Users/ff69upeb/Desktop/Test plots")
ggsave("Legend.jpg",
       plot = tmp.plot, device = "jpeg",
       width = 7, height = 5, dpi = 600)

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
