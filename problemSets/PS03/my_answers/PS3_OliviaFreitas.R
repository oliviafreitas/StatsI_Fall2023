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
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("knitr")
library(knitr)


# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")




##Start setting the working directory
setwd("C:/Users/User/Documents/GitHub/StatsI_Fall2023/problemSets/PS03/template")
getwd()

##Research questions
##1. how the difference in campaign spending between incumbentand challenger affects the incumbent's vote share?


##This analyse will be about a dataset called incumbents subset.csv
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

##Check out the head and tail.
head(inc.sub)
tail(inc.sub)

##We will select only the columns "voteshare" and "difflog" to create a dataframe with the relevant variables
selected_data  <- inc.sub[, c("voteshare", "difflog")]

# Simple linear regression
model <- lm(voteshare ~ difflog, data = selected_data)

## Summary of the regression model
summary(model)

summary_inc.sub <- summary(inc.sub)
write.table(summary_inc.sub, file = "summary_inc.sub.txt")


## Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$voteshare)
abline(model, col = "red")


## Save the residuals of the model in a separate object.

residuals <- resid(model)

## Summary of the regression model
summary(model)

## Residuals
head(residuals)



#2  We will select only the variables "presvote" and "difflog"
selected_data <- inc.sub [, c("presvote", "difflog")]
## Linear regression model
model_presvote <- lm(presvote ~ difflog, data = inc.sub)
## Summary 
summary(model_presvote)


##  Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$presvote)
# Regression 
abline(model_presvote, col = "red")


## Save the residuals of the model in a separate object.

# Residuals 
residuals_presvote <- resid(model_presvote)

# Summary of the regression model
summary(model_presvote)

# Residuals
head(residuals_presvote)


# 3 We will select the columns 'voteshare' and 'presvote'
selected_data <- inc.sub[, c("voteshare", "presvote")]

# Linear regression model
model_voteshare <- lm(voteshare ~ presvote, data = selected_data)

# Summary of the regression model
summary(model_voteshare)


# Scatterplot 
plot(selected_data$presvote, selected_data$voteshare)

# Regression
abline(model_voteshare, col = "red")



# 4 The residuals from Question 1 are stored in residuals and residuals from Question 2 are stored in residuals_presvote
residuals_model <- lm(residuals ~ residuals_presvote)

# Summary of the regression model
summary(residuals_model)

# Scatterplot with regression line
plot(residuals_presvote, residuals)
abline(residuals_model, col = "red")

#5 We will select the columns 'voteshare', 'difflog', and 'presvote'
selected_data <- inc.sub[, c("voteshare", "difflog", "presvote")]

# Linear regression model
model_combined <- lm(voteshare ~ difflog + presvote, data = selected_data)

# Summary of the regression model
summary(model_combined)


