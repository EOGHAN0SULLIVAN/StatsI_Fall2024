#Set working directory
setwd("C:/Users/diarmuid/Documents/StatsI_Fall2024/my_answers/PS-03")
getwd()

incumbts <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
View(incumbts)
install.packages("ggplot2")
library(ggplot2)

#Determining spending - question 5
#Defining objects for the incumbent, challenger and difference between these in spending
Candidate <- incumbts[, 5]
President <- incumbts[, 6]
Difference <- incumbts[, 7]
Voteshare <- incumbts[, 9]

#Calculating if the difflog variable is the same as the difference between incspend and chalspend 
DifferntDifference <- President - Candidate
are_equal <- all.equal(Difference, DifferntDifference)
print(are_equal == TRUE)

#Question 1 - Does campaign spending differences affect voteshare? 
#Determining whether the dependent variable follows a normal distribution by using a histogram
hist(incumbts$difflog)

#Performing the regression analysis
incumbent.voteshare.lm <- lm(incumbts$voteshare ~ incumbts$difflog, data = incumbts)
summary(incumbent.voteshare.lm)

#Review of the scatter plot to see if the distribution of data points can be described with a straight line
plot(incumbts$voteshare ~ incumbts$difflog, data = incumbts)

#Fitting a linear model using an object named lm_model
lm_model <- lm(incumbts$voteshare ~ incumbts$difflog, data = incumbts)

#Drawing the regression line to add to the scatter plot 
abline(lm_model,col="red")

#Saving the residuals of the regression model in a separate object
question1_residuals <- incumbent.voteshare.lm$residuals
question1_residuals

#Calculating the Beta0 - intercept and the BetaX - slope
intercept <- round(incumbent.voteshare.lm$coefficients[1],3)
slope <- round(incumbent.voteshare.lm$coefficients[2],3)

#Writing the prediction equation as y-hat = Intercept + Slope * Variable
cat(intercept, "+", slope, "* difflog")

#Question 2 - Do campaign spending differences affect presvote? 
#Performing the regression analysis
incumbent.presvote.lm <- lm(incumbts$presvote ~ incumbts$difflog, data = incumbts)
summary(incumbent.presvote.lm)

#Review of the scatter plot to see if the distribution of data points can be described with a straight line
plot(incumbts$presvote ~ incumbts$difflog, data = incumbts)

#Fitting a linear model using an object named lm_model
lm_model <- lm(incumbts$presvote ~ incumbts$difflog, data = incumbts)

#Drawing the regression line to add to the scatter plot 
abline(lm_model,col="red")

#Saving the residuals of the regression model in a separate object
question2_residuals <- incumbent.presvote.lm$residuals
question2_residuals

#Calculating the Beta0 - intercept and the BetaX - slope
intercept <- round(incumbent.presvote.lm$coefficients[1],3)
slope <- round(incumbent.presvote.lm$coefficients[2],3)

#Writing the prediction equation as y-hat = Intercept + Slope * Variable
cat(intercept, "+", slope, "* difflog")

#Question 3 - How does the incumbent President's party vote share affect his/her electoral success? 

#Determining whether the dependent variable follows a normal distribution by using a histogram
hist(incumbts$presvote)

#Performing the regression analysis
incumbent.voteshare.lm <- lm(incumbts$voteshare ~ incumbts$presvote, data = incumbts)
summary(incumbent.voteshare.lm)

#Review of the scatter plot to see if the distribution of data points can be described with a straight line
plot(incumbts$voteshare ~ incumbts$presvote, data = incumbts)

#Fitting a linear model using an object named lm_model
lm_model <- lm(incumbts$voteshare ~ incumbts$presvote, data = incumbts)

#Drawing the regression line to add to the scatter plot 
abline(lm_model,col="red")

#Saving the residuals of the regression model in a separate object
#question3_residuals <- incumbent.presvote.lm$residuals
#question3_residuals

#Calculating the Beta0 - intercept and the BetaX - slope
intercept <- round(incumbent.voteshare.lm$coefficients[1],3)
slope <- round(incumbent.voteshare.lm$coefficients[2],3)

#Writing the prediction equation as y-hat = Intercept + Slope * Variable
cat(intercept, "+", slope, "* presvote")
