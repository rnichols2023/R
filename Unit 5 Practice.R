library(tidyverse)
library(ggplot2)
data = read.csv("incentive_pay.csv")
head(data)
tail(data)
plot(data$Base_Wage, data$Productivity, main = "Scatterplot Example 1")
plot(data$Pay_Rate_Per_Item, data$Productivity, main = "Scatterplot Example 2")
ggplot(data, aes(x = data$Productivity, data$Base_Wage)) + geom_point() + geom_smooth(method = lm)
ggplot(data, aes(x = data$Productivity, data$Pay_Rate_Per_Item)) + geom_point() + geom_smooth(method = lm)
cor.test(data$Base_Wage, data$Productivity)
cor.test(data$Pay_Rate_Per_Item, data$Productivity)
model1 <- lm(data$Productivity ~ data$Base_Wage, data = data)
summary(model1)
x = -1.7794 + (17.505 * 3.1006)
print(x)
model2 <- lm(data$Productivity ~ data$Pay_Rate_Per_Item, data = data)
summary(model2)
y = 12.320 + (139.754 * 0.18)
print(y)
model3 <- lm(data$Productivity ~ data$Base_Wage + data$Pay_Rate_Per_Item, data = data)
summary(model3)
coefficients(model3)
confint(model3)
model_residuals <- model3$residuals
print(model_residuals)
hist(model_residuals)
plot(model_residuals)
intercept <- -39.461856
beta_base <- 3.250525
beta_rate <- 145.509156
estimate_base <- 17.505
estimate_rate <- 0.18
prediction <- intercept + (beta_base * estimate_base) + (beta_rate * estimate_rate)
print(prediction)
model4 <- lm(data$Productivity ~ data$Base_Wage * data$Pay_Rate_Per_Item, data = data)
summary(model4)

