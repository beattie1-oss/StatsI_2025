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
lapply(c("ggplot2", "stargazer", "GGally", "tidyverse",
         "ggpubr", "gridExtra", "tidyr", "plyr", "broom", "janitor"),  pkgTest)
##setting working directions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Question 1: Political Science
#####################

## 1.a Chi-square test statistic by hand ##

data <- matrix(c(14,7,6,7,7,1), nrow = 2)
colnames(data) = c("Not_Stopped","Bribe_Requested","Stopped_Warned")
rownames(data) = c("Upper Class", "Lower Class")
fO <- as.table(data) #frequency observed table
fO_t <- rbind(fO, Total = colSums(fO))  #add column total
fO_t <- cbind(fO_t, Total = rowSums(fO_t))   #add row total 
print(fO_t) # table of observed frequencies with totals


row_totals <- rowSums(fO)
col_totals <- colSums(fO)
grand_total <- sum(row_totals)
fe <- outer(row_totals, col_totals, "*")/ grand_total
fe <- round(fe, 2)
fe

res <- (fO - fe)^2 / (fe) #squared diff btw obs and exp values div by exp 
chi_square <- sum(res) # sum across all frequencies
chi_square

## 1. b P-value of Chi-square ##

df <- (nrow(fO)-1) * (ncol(fO)-1) 
p_value <- pchisq(chi_square, 2 , lower.tail = FALSE )
p_value

chisq.test(fO,fe)

## 1. c Standardized residuals into table
 #find adjusted resisual 

z <- (fO - fe) / sqrt(fe * (1-(row_totals/grand_total))*(1-(col_totals/grand_total)))
round_z <- round(z,2)
print(round_z)




############################ 75
# Question 2: Economics
#####################

## 2.0 Load and Explore data ##
GP <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)

glimpse(GP)
summary(GP)
str(GP)
plot(GP)

## 2. a Hypothesis ##

summary(GP$water)
plot(density(GP$water))

## 2. b Bivariate regression ##

reg1 <- lm(data= GP, water~reserved)
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("PS02_reg1output.tex", reg1)

pdf("PS02_Reg_Plot.pdf")
plot(GP$reserved, 
     GP$water,
     xaxt = "n",
     xlab="Reserved Policy",
     ylab="Number water facilities")
  abline(reg1, col = "red") # Add regression line
  axis(1, at = c(0,1))
dev.off()

## 2. c Interpret COefficient estimate ##

confint(reg1, level = 0.95)