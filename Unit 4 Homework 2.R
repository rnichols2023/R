data <- read.csv("smartphone_survey.csv")
head(data)
print(data)
contingency_table <- table(data$AgeGroup, data$PreferredOS)
print(contingency_table)
library(MASS)
chisq.test(data$AgeGroup, data$PreferredOS)
chisq.test(contingency_table)
chisq.test(data)
library(ggplot2)
ggplot(data) +
  geom_bar(aes(x = data$AgeGroup, fill = data$PreferredOS))
