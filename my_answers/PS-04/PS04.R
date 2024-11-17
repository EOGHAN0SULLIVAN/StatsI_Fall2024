## Question 1
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
setwd("C:/Users/diarmuid/Documents/StatsI_Fall2024/my_answers/PS-04")
getwd()

#Creating a data frame from the data
df <- data.frame(Prestige)

#Creating a binary 0/1 variable called professional from existing strings, ignoring missing data fields
df$professional <- ifelse(is.na(df$type), NA, ifelse(df$type == "prof", 1, 0))

#Printing the data frame with the new variable called professional
print(df)

#Run the linear model using the lm function on the data frame previously updated above
model <- lm(prestige ~ professional + df$income, data = df)
summary(model)

#Calculating the change in the average prestige associated with a $1000 increase in income
income_coefficient <- 1.371e-03
change_in_income <- 1000
change_in_prestige <- income_coefficient * change_in_income

#Printing the result
print(change_in_prestige)
 
#Calculating the change in the average prestige associated with an income of $6000
prof_coefficient <- 22.76
income_value <- 6000
change_in_prestige_professional <- prof_coefficient * income_value

#Printing the result
print(change_in_prestige_professional)

## Question 2
#Given coefficient and standard error and plugging into R
coef_assigned <- 0.042
se_assigned <- 0.016

#Calculation of T-value 
t_value <- coef_assigned / se_assigned
print(t_value)

#Calculation of p-value using the given degrees of freedom
degrees_f <- 30 - 2
p_value <- 2 * pt(-abs(t_value), degrees_f)
print(p_value)

#Coefficient and Standard Error for Precinct Adjacent to Lawn Signs
coef_adjacent <- 0.042
se_adjacent <- 0.013

#Calculating the t-value
t_value_adjacent <- coef_adjacent / se_adjacent
print(t_value_adjacent)

#Degrees of freedom
deg_free <- 30 - 2

# Calculate the p-value (two-tailed)
p_value_adjacent <- 2 * pt(-abs(t_value_adjacent), deg_free)
print(p_value_adjacent)
