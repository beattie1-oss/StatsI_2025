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


#####################
# Question 1: Education
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
## 1.1 90% CI for average student IQ ##
# Determine average (mean) and variance (with standard deviation) student IQ in sample
n <- length(y)
df <- n - 1
sample_mean <- mean(y)
sample_sd <- sd(y)
standard_error_y <- (sample_sd / sqrt(n))

# Find critical t value (probability 90 and df = n-1)
t90 <- qt(.90, 24)
# Find upper and lower limits of CI, mean +/- margin of error
lower_90 <- sample_mean - (t90 * standard_error)
upper_90 <- sample_mean + (t90 * standard_error)
confint_90 <- c(lower_90, upper_90)
confint_90




## 1.2 Hypothesis test whether her school average IQ higher than population average ##
# Null hypothesis sample y mean is less than or equal to population mean 100
# Altenative hypothesis sample mean is greater than population average of 100
pop_mean <- 100
# Create test-statistic 
# T-test given small sample size
ts <- (sample_mean - pop_mean) / standard_error_y
ts

# P value
pt(ts, df, lower.tail = FALSE)

#####################
# Question 2: Political economy
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
## 2.0 Explore and Import Data into R
colnames(expenditure) <- c("state", "exp_housing_pc", "income_pc", "n_financially_insecure", "n_urban", "region")
# naming columns something more relevant
dim(expenditure)
summary(expenditure)


## 2.1 PLot relationships between Y, X1, X2, X3


## 2.2 Plot relationship Y and Region

## 2.3 Plot relationship Y and X1, then including region