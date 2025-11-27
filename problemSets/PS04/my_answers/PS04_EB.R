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

#Loading Packages and Running Prestige Dataset
lapply(c("car","stargazer", "ggplot2"),  pkgTest)
library(car)
data(Prestige)
help(Prestige)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Q1 a) new variable 'professional'
Prestige$type #see that prof = professional, plus some missing values
Prestige$prof <- ifelse(Prestige$type =="prof", 1, 0)
              #new var, if 'prof' in type then 1 else 0 for blue/white collar workers

# Q1 b) 
model1 <- lm(prestige ~ income + prof + income:prof, data = Prestige) 
stargazer(model1,
          type = "latex", #output code for use in latex
          title = "Impact of Income level and being a Professional on Job Prestige",
          covariate.labels = c("Income", "Professional", "Income*Professional"),
          dep.var.labels = "Prestige")

Prestige$prof <- factor(Prestige$prof, #create dummy as factor 
                        levels = c(0,1), 
                        labels = c("Non-Professional", "Professional"))
pdf("PS04_reg1plot.pdf")
ggplot(data = Prestige, aes(x = income, y = prestige, colour = prof)) + 
  geom_point(size = 1) +
  scale_x_continuous(limits = c(0, NA)) +
  geom_abline(intercept = 58.923, slope = 0.001, colour = "#00BFC4", size = 0.75) + #add pred line for professionals
  geom_abline(intercept = 21.142, slope = 0.003, colour = "#F8766D", size = 0.75) + #add pred line for non-professionals
  labs(x = "Income", y = "Prestige", colour = "Job type") +
  theme_light() +
  theme(panel.grid = element_blank(), #remove grad
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.75),
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(colour = "grey", fill = "white"))
dev.off()


#Q2a) Hypothesis Test assigned to lawn signs
ts_1 <- 0.042 / 0.016
ts_1 
p_1 <- 2*pt(abs(ts_1), 128, lower.tail = FALSE)
p_1


#Q2b) Hypothesis test adjacent to lawn signs
ts_2 <- 0.042 / 0.013
ts_2
p_2 <- 2*pt(abs(ts_2), 128, lower.tail = FALSE)
p_2



#d) Evaluate model fit f-test
F.test <- (0.094/2) / ((1-0.094)/(131-2-1))
F.test
p_f <- pf(F.test, 2, 128, lower.tail = FALSE)
p_f


