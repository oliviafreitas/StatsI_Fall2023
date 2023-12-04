## Question 1: Political Science

## (a)

# We will first create a data frame with our data
data <- data.frame(
  Class = c("Upper class", "Lower class"),
  `Not Stopped` = c(14, 7),
  `Bribe requested` = c(6, 7),
  `Stopped/given warning` = c(7, 1)
)
# Now we will calculate row and column totals
row_totals <- rowSums(data[, -1])
col_totals <- colSums(data[, -1])

# We will calculate then the grand total
grand_total <- sum(row_totals)

# We will also calculate the expected frequencies for each cell
expected_values <- outer(row_totals, col_totals) / grand_total

# Now it is time to calculate the χ² value for each cell
chi_squared <- sum((data[, -1] - expected_values)^2 / expected_values)

# In this step, we will need to check the degrees of freedom
df <- (nrow(data) - 1) * (ncol(data) - 1)

# Finally, we will print the Chi-squared statistic
cat("Chi-squared statistic:", chi_squared, "\n")


## (b)

# Let's calculate the p-value and run the chi-squared test 
p_value <- 1 - pchisq(chi_squared, df)  

# Then print the p-value
cat("Chi-squared p-value:", p_value, "\n")


## With our p-value 0.2849151 result we can not reject the null hypothesis because 
##it suggests that there is no significant relationship between the social class of 
##drives and the solicitation of a bribe


## (c)

# We need now to calculate the expected frequencies
expected_values <- outer(row_totals, col_totals) / grand_total


standardized_residuals <- (data[, -1] - expected_values) / sqrt(expected_values)

# We need to print the standardized residuals table
standardized_residuals_table <- cbind(data[, 1], standardized_residuals)
colnames(standardized_residuals_table) <- c("Class", "Not Stopped", "Bribe requested", 
"Stopped/given warning")

standardized_residuals_table

##(d)
# Standardized residuals detect significant deviations from expected values, helping identify cells that lead to associations and their practical significance.


##Question 2: Economics

##(a)
# Null: The reservation policy for women has no effect on the number of new or repaired drinking water facilities in the villages.
# H1: The reservation policy for women has an effect on the number of new or repaired drinking water facilities in the villages. 

#Now I will load the data into R
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")


##(b)
# Now we will run a bivariate regression 
model <- lm(water ~ reserved, data = data)

# We will now print the summary of the regression
summary(model)

##(c)

# The positive coefficient of "9.252" indicates that villages with policies reserving positions for women tend to have more new or repaired drinking water facilities compared to villages without this policy. The data demonstrate a statistically significant positive relationship between the policy of reserving positions for women as village council heads and the number of new or repaired drinking water facilities in the villages.

##