#Input of data for the problem and the creation of a vector
y <- c (105,69,86,100,82,111,104,110,87,108,87,90,94,113,112,98,
        80,97,95,111,114,89,95,126,98)

#Inspecting the data
summary(y)
str(y)
n <- length(y)
n

#Calculation of the sample mean based on the data
sample_mean <- mean(y)
sample_mean

#Calculation of the standard deviation based on the data
sample_sd <- sd(y)
sample_sd

#Calculating the error
error <- qnorm(0.9)*sample_sd/sqrt(n)
error

#Lower and Upper 90 calculation for confidence interval of 90%
lower_90 <- sample_mean - (error)
lower_90
upper_90 <- sample_mean + (error)
upper_90

#Calculation of the confidence interval for the average student IQ of the school
confint90 <- c(upper_90, lower_90)
confint90
