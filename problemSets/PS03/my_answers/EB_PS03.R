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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

## Q1 ## 

plot(density(inc.sub$voteshare))
plot(density(inc.sub$difflog))
plot(density(inc.sub$presvote))

# 1.1
reg1 <- lm(data= inc.sub, voteshare~difflog) #bivariate regression
summary(reg1)
stargazer(reg1, 
          type = "latex",
          title = "Incumbent/Challenger Difference in Campaign Spending
                  on Incumbent's Voteshare",
          covariate.labels = "Difference Log Spending", #label explanatory vars
          dep.var.labels = "Voteshare") #label predicted var

#1.2
pdf("PS03_reg1plot.pdf")
ggplot(inc.sub, aes(x = difflog, y = voteshare)) + #scatter plot using date
  geom_point(size = 0.7) + #point size smaller to avoid overlapping/crowding
  geom_smooth(method=lm) + #smooth method adds linear regression line 
  labs(x = "Difference Log Spending", y = "Voteshare") + #label axis
  theme_light() + #change theme
  theme(axis.title = element_text(size = 15, family = "mono")) #change font axis
dev.off()

#1.3
reg1_residuals <- reg1$residuals # save as new object res from reg1

## Q2 ##
#2.1
reg2 <- lm(data= inc.sub, presvote~difflog) 
summary(reg2)
stargazer(reg2, #stargazer used for presenting model in table
          type = "latex", #output code for use in latex
          title = "Incumbent/Challenger Difference in Campaign Spending
            on the Presidential Candidate Voteshare of the Incumbent Party ",
          covariate.labels = "Difference Log",
          dep.var.labels = "Presidential Candidate Voteshare")

#2.2
pdf("PS03_reg2plot.pdf")
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(size = 0.7) + 
  geom_smooth(method=lm) +
  labs(x = "Difference Log", y = "Presidential Candidate Voteshare") +
  theme_light() +
  theme(axis.title = element_text(size = 15, family = "mono"))
dev.off()

#2.3
reg2_residuals <- reg2$residuals

# Q3 ## 
#3.1
reg3 <- lm(data= inc.sub, voteshare~presvote) 
summary(reg3)
stargazer(reg3, 
          type = "latex",
          title = "Presidential Candidate Voteshare of the Incumbent Party
                    on the Incumbent's Voteshare",
          covariate.labels = "Presidential Candidate Voteshare",
          dep.var.labels = "Voteshare")

#3.2
pdf("PS03_reg3plot.pdf")
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(size = 0.7) + 
  geom_smooth(method=lm) +
  labs(x = "Presidential Candidate Voteshare", y = "Voteshare") +
  theme_light() +
  theme(axis.title = element_text(size = 15, family = "mono"))
dev.off()

#Q4
#4.1
df.residuals <- data.frame(reg1_residuals, reg2_residuals) #create new df
reg4 <- lm(data = df.residuals, reg1_residuals ~ reg2_residuals) 
summary(reg4)
stargazer(reg4, 
          type = "latex",
          title = "Regression 1 Residuals on Regression 2 Residuals",
          covariate.labels = "Regression 2 Residuals",
          dep.var.labels = "Regression 1 Residuals")
#4.2
pdf("PS03_reg4plot.pdf")
ggplot(df.residuals, aes(x = reg2_residuals, y = reg1_residuals)) +
  geom_point(size = 0.7) + 
  geom_smooth(method=lm) +
  labs(x = "Regression 2 Residuals ", y = "Regression 1 Residuals") +
  theme_light() +
  theme(axis.title = element_text(size = 15, family = "mono"))
dev.off()

## Q5
#5.1
reg5 <- lm(data = inc.sub, voteshare ~ difflog + presvote) #multivariate
summary(reg5)
stargazer(reg5, 
        type = "latex",
        title = "Incumbent/Challenger Difference in Campaign Spending and 
                the Presidential Candidate Voteshare of the Incumbent Party
                on the Incumbent's Voteshare",
        covariate.labels = c("Difference Log", "Presidential Candidate Voteshare"),
        dep.var.labels = "Voteshare")
