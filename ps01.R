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

#Calculating the sum of the squared errors
for(i in y){
  demeanedSum <- y - sample_mean
} 

demeanedSum_sample <- y - sample_mean
sum(demeanedSum_sample)

z90 <- qnorm((1.64)/2)
z90
## z90 <- error - ((1.64)/2)
## c(demeanedSum - qnorm(0.95) * error,sample_mean + qnorm(0.95) * error) 

#Lower and Upper 90 calculation for confidence interval of 90%
lower_90 <- sample_mean - (z90*(sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90*(sample_sd/sqrt(n)))

#Calculation of the confidence interval for the average student IQ of the school
confint90 <- c(upper_90, lower_90)
confint90 

############
#Assuming simple random sampling in the data for the problem of a discrete type (values of 50-150) with a n = 25
#Statement of Null and Alternative Hypotheses
x <- rnorm(50:150)
y <- 0.05
#Calculation of test statistic
t.test(x, mu = sample_mean)
#Reject the null hypothesis

###########
#Load the data
View(expenditure)
head(expenditure)
#Plot the data
plot(expenditure$V2,expenditure$V6)
