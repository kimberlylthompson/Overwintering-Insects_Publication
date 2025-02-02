<<<<<<< HEAD
#################################################################
############   Determining the best fit line from    ############
############    extracted data on LLTime 50 of       ############
###########      freeze tolerant bean leaf beetles   ############
##################################################################

# Author: Kimberly Thompson

# This script finds the best fit line describing the number of days 
# until 50% mortality in a sample of bean leaf beetles.

# Best fit at 50% as well as upper and lower confidence interval values
# extracted.

# Figures produced
# S11


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(ggplot2)

# Data on the number of hours and percent mortality at constant temperatures of 
# 0C were extracted from Lam and Pedigo 2000 using xyscan.

# Read in the dataframe
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Extracted LLT Data")
llt <- read.csv("Data from Lam and Pedigo LLT at 0C.csv", header = TRUE)

# Convert hours into days
llt$Days <- llt$Hours/24

# Create a quadratic term
llt$Days2 <- llt$Days^2

# Create the lower confidence interval values
llt$Percent.Mortality_low <- llt$Percent.Mortality - llt$CI

# Create the upper confidence interval values
llt$Percent.Mortality_upper <- llt$Percent.Mortality + llt$CI


# Make the linear model
quadratic.model <- lm(Percent.Mortality ~ Days + Days2, data=llt)
summary(quadratic.model)

quadratic.model_low <- lm(Percent.Mortality_low ~ Days + Days2, data=llt)
summary(quadratic.model_low)

quadratic.model_upper <- lm(Percent.Mortality_upper ~ Days + Days2, data=llt)
summary(quadratic.model_upper)



##################################################################
### Predicting
##########################

# Make a list Day values based on range of observed values
day.values <- seq(0, 100, by=1)


#### Regular
# Predict mortality using the model and the list of day values
predicted.mortality <- predict(quadratic.model, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new <- data.frame(day.values, predicted.mortality)


#### Lower
# Predict mortality using the model and the list of day values
predicted.mortality_low <- predict(quadratic.model_low, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new_low <- data.frame(day.values, predicted.mortality_low)

#### Upper
# Predict mortality using the model and the list of day values
predicted.mortality_upper <- predict(quadratic.model_upper, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new_upper <- data.frame(day.values, predicted.mortality_upper)


# Make a plot of the points, and fitted lines
test <- ggplot() +
  geom_point(aes(x=Days, y=Percent.Mortality), data = llt) +
  geom_line(aes(x=day.values, y=predicted.mortality), data=new) +
  geom_line(aes(x=day.values, y=predicted.mortality_low), data=new_low,
            linetype = "dashed") +
  geom_line(aes(x=day.values, y=predicted.mortality_upper), data=new_upper,
            linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Days", limits=c(0, 101),
                     breaks=c(0, 25, 50, 75, 100)) +
  scale_y_continuous(name="Percent Mortality", limits=c(0, 100),
                     breaks=c(0, 20, 40, 60, 80, 100))


setwd("01_Analysis/Insect_Plotting")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Bean Leaf Beetle/Extracted Data")
ggsave("Bean Leaf Beetle Mortality at 0C.jpg", plot=test, device = "jpg",
       width=7, height=5, dpi=300)
  



# To find the roots of the quadratic equation (i.e. back-calcuate the number of days for a given percent mortality
# response, in this case 50% for original model, and the upper and lower models to get the CI)
# How to do this is at https://stackoverflow.com/questions/1476585/how-to-use-predict-lm-in-r-to-reverse-the-regression

# in model output 1 is the intercept, 2 is the linear termp, and 3 is the quadratic term

# Original Model
a <- quadratic.model$coefficients[[3]]
b <- quadratic.model$coefficients[[2]]
c <- quadratic.model$coefficients[[1]]
y <- 50

# Original Model
root1<-sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2<--sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# Lower Model
a <- quadratic.model_low$coefficients[[3]]
b <- quadratic.model_low$coefficients[[2]]
c <- quadratic.model_low$coefficients[[1]]
y <- 50

# Original Model
root1_low <- sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2_low <- -sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# Upper Model
a <- quadratic.model_upper$coefficients[[3]]
b <- quadratic.model_upper$coefficients[[2]]
c <- quadratic.model_upper$coefficients[[1]]
y <- 50

# Original Model
root1_upper <- sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2_upper <- -sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# It's the root 2's that are what I need

llt.values <- c(root2_upper, root2, root2_low)

# [1] 28.87259 34.61265 41.32808

=======
#################################################################
############   Determining the best fit line from    ############
############    extracted data on LLTime 50 of       ############
###########      freeze tolerant bean leaf beetles   ############
##################################################################

# Author: Kimberly Thompson

# This script finds the best fit line describing the number of days 
# until 50% mortality in a sample of bean leaf beetles.

# Best fit at 50% as well as upper and lower confidence interval values
# extracted.

# Figures produced
# S11


# Clear workspace
rm(list = ls() ) 
gc() #releases memory

library(ggplot2)

# Data on the number of hours and percent mortality at constant temperatures of 
# 0C were extracted from Lam and Pedigo 2000 using xyscan.

# Read in the dataframe
setwd("00_Data")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Data/Extracted LLT Data")
llt <- read.csv("Data from Lam and Pedigo LLT at 0C.csv", header = TRUE)

# Convert hours into days
llt$Days <- llt$Hours/24

# Create a quadratic term
llt$Days2 <- llt$Days^2

# Create the lower confidence interval values
llt$Percent.Mortality_low <- llt$Percent.Mortality - llt$CI

# Create the upper confidence interval values
llt$Percent.Mortality_upper <- llt$Percent.Mortality + llt$CI


# Make the linear model
quadratic.model <- lm(Percent.Mortality ~ Days + Days2, data=llt)
summary(quadratic.model)

quadratic.model_low <- lm(Percent.Mortality_low ~ Days + Days2, data=llt)
summary(quadratic.model_low)

quadratic.model_upper <- lm(Percent.Mortality_upper ~ Days + Days2, data=llt)
summary(quadratic.model_upper)



##################################################################
### Predicting
##########################

# Make a list Day values based on range of observed values
day.values <- seq(0, 100, by=1)


#### Regular
# Predict mortality using the model and the list of day values
predicted.mortality <- predict(quadratic.model, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new <- data.frame(day.values, predicted.mortality)


#### Lower
# Predict mortality using the model and the list of day values
predicted.mortality_low <- predict(quadratic.model_low, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new_low <- data.frame(day.values, predicted.mortality_low)

#### Upper
# Predict mortality using the model and the list of day values
predicted.mortality_upper <- predict(quadratic.model_upper, list(Days=day.values, Days2=day.values^2))

# Put those predicted values in a dataframe
new_upper <- data.frame(day.values, predicted.mortality_upper)


# Make a plot of the points, and fitted lines
test <- ggplot() +
  geom_point(aes(x=Days, y=Percent.Mortality), data = llt) +
  geom_line(aes(x=day.values, y=predicted.mortality), data=new) +
  geom_line(aes(x=day.values, y=predicted.mortality_low), data=new_low,
            linetype = "dashed") +
  geom_line(aes(x=day.values, y=predicted.mortality_upper), data=new_upper,
            linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Days", limits=c(0, 101),
                     breaks=c(0, 25, 50, 75, 100)) +
  scale_y_continuous(name="Percent Mortality", limits=c(0, 100),
                     breaks=c(0, 20, 40, 60, 80, 100))


setwd("01_Analysis/Insect_Plotting")
# setwd("L:/LabMemberFolders/KimberlyThompson/Ch 4 Bumblebees/Analysis/Graphs/Species Sensitivities/Bean Leaf Beetle/Extracted Data")
ggsave("Bean Leaf Beetle Mortality at 0C.jpg", plot=test, device = "jpg",
       width=7, height=5, dpi=300)
  



# To find the roots of the quadratic equation (i.e. back-calcuate the number of days for a given percent mortality
# response, in this case 50% for original model, and the upper and lower models to get the CI)
# How to do this is at https://stackoverflow.com/questions/1476585/how-to-use-predict-lm-in-r-to-reverse-the-regression

# in model output 1 is the intercept, 2 is the linear termp, and 3 is the quadratic term

# Original Model
a <- quadratic.model$coefficients[[3]]
b <- quadratic.model$coefficients[[2]]
c <- quadratic.model$coefficients[[1]]
y <- 50

# Original Model
root1<-sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2<--sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# Lower Model
a <- quadratic.model_low$coefficients[[3]]
b <- quadratic.model_low$coefficients[[2]]
c <- quadratic.model_low$coefficients[[1]]
y <- 50

# Original Model
root1_low <- sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2_low <- -sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# Upper Model
a <- quadratic.model_upper$coefficients[[3]]
b <- quadratic.model_upper$coefficients[[2]]
c <- quadratic.model_upper$coefficients[[1]]
y <- 50

# Original Model
root1_upper <- sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
root2_upper <- -sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))


# It's the root 2's that are what I need

llt.values <- c(root2_upper, root2, root2_low)

# [1] 28.87259 34.61265 41.32808

>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
