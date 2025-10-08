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
# Question 1: Education
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)


#Finding confidence intervals ## 

mean_y <- mean(y) #Find Point Estimate
mean_y
se_y <- sd(y) / sqrt(length(y)) #Standard error, where n found by length of y
se_y 
n <- length(y) #Sample size
n

tscore_90 <- qt(0.05, 24, lower.tail = FALSE) #Find t-score
    # where p is ((1-.90)/2) = 0.05, df = n - 1 = 24
tscore_90

lower_90_t <- mean_y - (tscore_90 * se_y) # Lower limit of CI: Mean - Margin Error
upper_90_t <- mean_y + (tscore_90 * se_y)  # Upper limit of CI: Mean + Margin Error
confint_90 <- c(lower_90_t, upper_90_t) #Final Interval
round(confint_90, 2)

## 1.2 Hypothesis test whether her school average IQ higher than population average ##

test_stat <- (mean_y - 100)/ se_y # sample mean minus population mean divide by standard error
test_stat
p <- pt(test_stat, 24, lower.tail = FALSE) #prob of t-dis of getting test stat, df = n-1 = 24, lower tail false as looking at right side
p


############################
# Question 2: Political economy
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

## 2.0 Exploring Data ##

expenditure
dim(expenditure) 
stargazer(expenditure)

## 2.1 Plot relationships Y, X1, X2, X3 ##

pdf("2.1 Relationships between housing expenditure variables.pdf")
ggpairs(expenditure, 
        columns = c(3,4,5,2), 
        mapping = aes(alpha = 0.7),
        title = "Relationships between housing expenditure variables")
dev.off()

 ## 2.2 Plot relationship Y and Region ##

expenditure$Region_name <- factor(expenditure$Region,
                            labels = c("Northeast", "North Central", "South", "West")) 
# Create new factor variable into Categories from encoding in data frame

pdf("2.2 State Housing Expenditure by Region Boxplot.pdf")
ggplot(expenditure, aes(x = Region_name, y = Y, fill = Region_name)) + #fill colour by region category
  stat_boxplot(geom ='errorbar') + #to add whiskers
  geom_boxplot() +
  labs(x = "Region", y = "Per capita expenditure on housing assistance in state") +
dev.off()

#Finding regional averages 
mean_by_region <- aggregate(expenditure$Y,
                            by = list(Region = expenditure$Region_name),
                            FUN = mean,
                            ) #find summary stats for subset Y and region with function mean
mean_by_region #Print region averages

## 2.3 Plot relationship Y and X1, then including region ##
plot1 <- ggplot(expenditure, aes(x = X1, y = Y)) + #using only X1
  geom_point() +
  labs(x = "Personal income pc",
       y = "Expenditure on housing assistance pc", 
       title = "Relationship betweeen Y and X1")
plot2 <- ggplot(expenditure, aes(x = X1, y =Y)) +
  geom_point(mapping = aes(color = Region_name, shape = Region_name)) + #distinguish region by mapping points differently
  scale_shape_manual(values=c(15, 19, 6, 17)) + #specify different looking shapes 
  labs(x = "Personal income pc", 
       y = "Expenditure on housing assistance pc", 
       title = "Relationship betweeen Y and X1 by Region") +
  theme(legend.position = c(.1,.95), #legend location
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) 
plotcom <-  grid.arrange(plot1, plot2, ncol = 2)
ggsave(filename="2.3 Combined plot.pdf", plot= plotcom , width=10, height=6, units="in") #combining together
dev.off()


      
