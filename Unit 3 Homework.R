install.packages("tidyverse")
install.packages("effectsize")
library(tidyverse)
library(moments)
library(effectsize)
treatment_scores <- c(85, 92, 78, 88, 95, 83, 90, 87, 91, 84, 82, 89, 93, 79, 81, 86, 94, 77, 80, 96, 85, 92, 88, 90, 83)
control_scores <- c(80, 85, 76, 82, 90, 78, 87, 83, 89, 79, 77, 84, 88, 75, 81, 86, 91, 74, 79, 93, 80, 87, 82, 85, 78)
mean_t <- mean(treatment_scores)
mean_c <- mean(control_scores)
median(treatment_scores)
median(control_scores)
range(treatment_scores)
range(control_scores)
sd(treatment_scores)
sd(control_scores)
mean_t - mean_c
mean_t / mean_c
pooled_sd <- sd_pooled(treatment_scores, control_scores)
print(pooled_sd)
t_test <- t.test(treatment_scores, control_scores)
print(t_test)
(mean_t - mean_c) / pooled_sd
install.packages("pwr")
library(pwr)
alpha <- .05
power <- .8
effect_size <- 0.738116
sample_size <- 25 
sample_size_1 <- pwr.t.test(d = effect_size, sig.level = alpha, power = power, type =  "two.sample", alternative = "two.sided")
print(sample_size_1)
boxplot(treatment_scores, control_scores)
result <- t.test(treatment_scores, control_scores, conf.level = 0.95)
print(result)
result_2 <- t.test(treatment_scores, control_scores)
print(result_2)
exam_scores <- data.frame(study_method_a = treatment_scores, study_method_b = control_scores)
head(exam_scores)
plot(exam_scores,)
control_scores - treatment_scores
result_3 <- t.test(treatment_scores, control_scores)
confidence_interval <- result_3$conf.int
print(confidence_interval)
sd(confidence_interval)
mean_t - mean_c
3.96 / 5.3650100962937
cohens_d(treatment_scores, control_scores)
0