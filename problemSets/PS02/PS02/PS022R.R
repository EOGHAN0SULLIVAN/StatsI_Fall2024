data1 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
chi_test <- chisq.test(table(data1$female, data1$reserved))
chi_test
chi_test$statistic
chi_test$p.value
chi_test$expected
