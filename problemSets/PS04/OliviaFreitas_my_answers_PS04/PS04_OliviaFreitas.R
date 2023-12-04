# Question 1


# Start setting the working directory
setwd("C:/Users/User/Documents/GitHub/StatsI_Fall2023/problemSets/PS04/template")

# Install and load the car library
install.packages("car")
library(car)

# Load the Prestige dataset
data(Prestige)

# Display the help documentation for Prestige dataset
help(Prestige)

# a) 

# Create a new variable "professional" by recoding the "type" variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# View the updated dataset
head(Prestige)


# b) 

# Now we will run a linear model
model <- lm(prestige ~ income * professional, data = Prestige)

# Now we can display the summary of the model
summary(model)


# f)

# We will extract coefficients from the model
coefficients <- coef(model)

# Then extract the coefficient for income and its interaction term
beta_income <- coefficients["income"]
beta_interaction <- coefficients["income:professional"]

# Now we can calculate the change in y associated with a $1,000 increase in income for professional occupations
change_in_y <- beta_income + beta_interaction * 1000

# Finally, print the result
cat("The change in prestige associated with a $1,000 increase in income for professional occupations is:", round(change_in_y, 2), "\n")


# g)

# To calculate it, we will use the coefficient for the professional variable and its interaction term with income from the linear model

# First off all, extract coefficients from the model
coefficients <- coef(model)

# Now we will extract the coefficient for the professional variable and its interaction term with income
beta_professional <- coefficients["professional"]
beta_interaction <- coefficients["income:professional"]

# Then set the income value to $6,000
income_value <- 6000

# Now we calculate the change in y associated with changing occupation to professional at $6,000 income
change_in_y <- beta_professional + beta_interaction * income_value

# Print the result
cat("The change in prestige associated with changing occupation to professional at $6,000 income is:", round(change_in_y, 2), "\n")



# Question 2

# a)

# We will conduct hypothesis tests on the coefficients of the variables indicating the presence of the signs and we will use the t-statistics and p-values in the repression.


# The regression results
assigned_signs_coef <- 0.042
assigned_signs_se <- 0.016

adjacent_signs_coef <- 0.042
adjacent_signs_se <- 0.013

# Degrees of freedom
df <- 131 - 3  # 3 coefficients: assigned_signs, adjacent_signs, constant

# The critical value for a two-tailed test at alpha = 0.05
critical_value <- qt(1 - 0.05 / 2, df)

# T-statistics
assigned_signs_t <- assigned_signs_coef / assigned_signs_se
adjacent_signs_t <- adjacent_signs_coef / adjacent_signs_se

# P-values
assigned_signs_p_value <- 2 * pt(-abs(assigned_signs_t), df)
adjacent_signs_p_value <- 2 * pt(-abs(adjacent_signs_t), df)

# Print results
cat("Assigned Signs:\n")
cat("  Coefficient:", assigned_signs_coef, "\n")
cat("  Standard Error:", assigned_signs_se, "\n")
cat("  t-Statistic:", assigned_signs_t, "\n")
cat("  P-Value:", assigned_signs_p_value, "\n")
cat("  Reject Null Hypothesis:", assigned_signs_p_value < 0.05, "\n")

cat("\nAdjacent Signs:\n")
cat("  Coefficient:", adjacent_signs_coef, "\n")
cat("  Standard Error:", adjacent_signs_se, "\n")
cat("  t-Statistic:", adjacent_signs_t, "\n")
cat("  P-Value:", adjacent_signs_p_value, "\n")
cat("  Reject Null Hypothesis:", adjacent_signs_p_value < 0.05, "\n")


# b)

# Similar to the exercise A, we will conduct a hypothesis test on the coefficient of the variable indicating adjacent signs and we will use the t-statistic and p-value reporter in the regresssion. 


# The values from regression results
adjacent_signs_coef <- 0.042
adjacent_signs_se <- 0.013

# Degrees of freedom
df <- 131 - 3  # 3 coefficients: assigned_signs, adjacent_signs, constant

# Critical value for a two-tailed test at alpha = 0.05
critical_value <- qt(1 - 0.05 / 2, df)

# T-statistic
adjacent_signs_t <- adjacent_signs_coef / adjacent_signs_se

# P-value
adjacent_signs_p_value <- 2 * pt(-abs(adjacent_signs_t), df)

# Print results
cat("Adjacent Signs:\n")
cat("  Coefficient:", adjacent_signs_coef, "\n")
cat("  Standard Error:", adjacent_signs_se, "\n")
cat("  t-Statistic:", adjacent_signs_t, "\n")
cat("  P-Value:", adjacent_signs_p_value, "\n")
cat("  Reject Null Hypothesis:", adjacent_signs_p_value < 0.05, "\n")

