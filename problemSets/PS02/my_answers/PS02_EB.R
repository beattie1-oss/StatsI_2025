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
lapply(c("ggplot2", "stargazer", "GGally", "tidyverse", "ggpubr", "gridExtra", "tidyr", "plyr", "broom"),  pkgTest)
##setting working directions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Question 1: Political Science
#####################

##1.0: Load data ##

## 1.a Chi-square test statistic by hand ##

## 1. b P-value of Chi-square ##

## 1. c Standardized residuals into table

## 1. d Interpret resuls






############################
# Question 2: Economics
#####################

## 2.0 Load and Explore data ##
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)


## 2. a Hypothesis ##

## 2. b Bivariate regression ##

## 2. c Interpret COefficient estimate ##
