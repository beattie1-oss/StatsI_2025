# Applied Statistical Analysis I      
# Tutorial 2: Hypothesis testing, experiments, difference in means                       

# Get working directory
getwd()

# Set working directory 
setwd("/Users/ellen/Documents/GitHub/StatsI_2025/tutorials/Tuesday/Week 3")
getwd()

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# Load data 
ds <- read.csv("fictional_data.csv")

# Selection of variables
# Education: University level education in years
# Income: Monthly net income
# Capital: Whether the person lives in capital or not

### (a.) Descriptive analysis ----------

# First step, look at data
View(ds)
head(ds)
str(ds) #structure


# Step by step
mean(ds$income)
var(ds$income)
sd(ds$income)
sd(ds$income)/sqrt(length(ds$income)) #Standard error

# Get summary statistics for entire dataset


# Some quick visualizations, to look at distribution
hist(ds$income,
     main = "monthly income",
     xlab = "Euro")
plot(density(ds$income,
             main = "monthly income",
             xlab = "Euro"))



# Which kind of inferences can we make with regards to the population,
# based on the sample data?

mean(ds$income) # sample mean estimate for population mean

# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

# (b.) Confidence intervals --------
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?


# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 

# The **approximate** solution 
# Lower bound, 95 confidence level

upper_95 = mean(ds$income) + (1.96*sd(ds$income)/sqrt(length(ds$income)))
lower_95 = mean(ds$income) - (1.96*sd(ds$income)/sqrt(length(ds$income)))

# Print
lower_95
mean(ds$income)
upper_95
#95% mean population falls between upper and lower bounds

# The **precise** solution, using normal distribution
# Lower bound, 95 confidence level

lower_95_n <- qnorm(0.025, 
                    mean = mean(ds$income), 
                    sd = (sd(ds$income)/sqrt(length(ds$income))))

# Upper bound, 95 confidence level
upper_95_n <- qnorm(0.975,
                    mean = mean(ds$income),
                    sd = (sd(ds$incom)/sqrt(length(ds$income))))

# Step by step


# Print
lower_95_n
mean(ds$income)
upper_95_n
# How to calculate 99% confidence intervals?
# When to use normal distribution and when to use t distribution?

# The **precise** solution, using t distribution


# Step by step 


# Print


# Update Histogram 


# Is there a relationship between education and income?

# Scatter plot 


# Improve visualization and save


# Boxplot


# (c.) Significance test for a mean ------

# What is the average monthly income in Ireland
# According to a quick Google search, it is 3034
# How does our sample compare to the population, 
# being the working population in Ireland?


# We also found a much easier way to calculate the confidence intervals (!)


# Let's double check


# (d.) Significance test for a difference in means ------

# On average, do people earn differently in the capital
# compared to people who do not reside in the capital?

# Calculate means for subgroups


# Step by step


# t-test


# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# t-test


