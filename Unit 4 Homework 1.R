library(pwr)
library(tidyverse)
library(ggplot2)
data <- read.csv("weight_loss_data.csv")
head(data)
print(data)
class(data$ParticipantID)
class(data$Group)
class(data$WeightLoss)
aggregate_data <- aggregate(WeightLoss ~ Group, data, mean)
print(aggregate_data)
ggplot(data = data) +
  geom_boxplot(aes(Group, WeightLoss))
oneway.test(WeightLoss ~ Group,
            data = data,
            var.equal = TRUE
            )
anova_data <- aov(WeightLoss ~ Group, data = data)
print(anova_data)
summary(anova_data)
effect <- lsr::etaSquared(anova_data)
print(effect)
weightloss_power <- pwr.anova.test(n = 30, k = 3, f = effect, sig.level = 0.05, power = NULL)
print(weightloss_power)

