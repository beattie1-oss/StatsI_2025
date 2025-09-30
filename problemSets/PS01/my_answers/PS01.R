#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
install.packages("tidyverse")
library(tidyverse)
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

##setting working directions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Question 1: Education
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Step 1 finding point average mean
sample_mean <- mean(y)
sample_mean
#finding key point estimate to construct interval around
#Step 2 Finding standard deviation and standard error
n <- length(y)
sample_sd <- sd(y)
sample_sd
standard_error_y <- (sample_sd / sqrt(n))
standard_error_y
#number observations, and variation stats  determined

# Find critical t value (probability 90 and df = n-1)
df <- n - 1
t90 <- qt(.90, 24)
t90

# Find upper and lower limits of CI, mean +/- margin of error
lower_90 <- sample_mean - (t90 * standard_error_y)
upper_90 <- sample_mean + (t90 * standard_error_y)
confint_90 <- c(lower_90, upper_90)
confint_90

## 1.2 Hypothesis test whether her school average IQ higher than population average ##
# Null hypothesis sample y mean is less than or equal to population mean 100
# Alternative hypothesis sample mean is greater than population average of 100
# Create test-statistic
pop_mean <- 100
ts <- (sample_mean - pop_mean) / standard_error_y
ts

# P value
pt(ts, df, lower.tail = FALSE)

#####################
# Question 2: Political economy
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
## 2.0 Explore and Import Data into R 
#colnames(expenditure) <- c("state", "exp_housing_pc", "income_pc", "n_financially_insecure", "n_urban", "region")
# naming columns something more relevant
dim(expenditure)
summary(expenditure)



## 2.1 PLot relationships between Y, X1, X2, X3
# Y and X1 
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "State expenditure on shelters/housing and personal income per capita",
    x = "Personal income per capita ($)",
    y = "Expenditure on housing assistance per capita ($)"
  )

# Y and X2
ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "State expenditure on shelters/housing and number of 'financially insecure' residents  ",
    x = " Number of residents per 100,000 that are ”financially insecure” in state",
    y = "Expenditure on housing assistance per capita ($)"
  )

#Y and X3
# Y and X2
ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "State expenditure on shelters/housing and number of urban residents ",
    x = " Number of people per thousand residing in urban areas in state",
    y = "Expenditure on housing assistance per capita ($)"
  )

# X1, X2, X3
ggplot(expenditure, aes(x = X1, y = X2)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(expenditure, aes(x = X1, y = X3)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(expenditure, aes(x = X2, y = X3)) +
  geom_point() +
  geom_smooth(method = lm)

## 2.2 Plot relationship Y and Region
expenditure$Region_name <- factor(expenditure$Region,
                             levels = c(1, 2, 3, 4),
                             labels = c("Northeast", "North Central", "South", "West"))
# Create new factor variable into Categories from encoding in data frame

ggplot(expenditure, aes(x = Region_name, y = Y)) +
  geom_boxplot()
mean_by_region <- aggregate(expenditure$Y,
                            by = list(Region = expenditure$Region_name),
                            FUN = mean,
                            )
mean_by_region


## 2.3 Plot relationship Y and X1, then including region
ggplot(expenditure)
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "State expenditure on shelters/housing and personal income per capita",
    x = "Personal income per capita ($)",
    y = "Expenditure on housing assistance per capita ($)"
  )

#Including symbols and colours
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(mapping = aes(color = Region_name, shape = Region_name)) +
  geom_smooth(method = lm) +
  labs(
    title = "State expenditure on shelters/housing and personal income per capita",
    x = "Personal income per capita ($)",
    y = "Expenditure on housing assistance per capita ($)"
  ) +

  