data <- read.csv('dataset.csv')
library(ggplot2)

#a
ggplot(data, aes(x = Hand.Dominance, y = Reaction.Time..ms. , fill = Hand.Dominance)) +
  geom_boxplot() +
  labs(title = "Reaction Time by Handedness",
       x = "Handedness",
       y = "Reaction Time (ms)") +
  theme_minimal()

#Answer:
#The boxplot and descriptive statistics indicate that, on average, there is a little difference in the reaction times of left- and right-handed people, with the latter having a slightly faster reaction time. On the other hand, right-handed people vary more in their reaction times.
#An official hypothesis test, such as an independent t-test, could be performed to ascertain whether this difference is statistically significant. However, based on hand dominance, we see a slight variation in reaction times from the descriptive and visual studies.

#b
lefthanded <- data$Reaction.Time..ms.[data$Hand.Dominance == "Left"]
righthanded <- data$Reaction.Time..ms.[data$Hand.Dominance == "Right"]

#Variance for left and right
var_left <- var(lefthanded)
var_right <- var(righthanded)

f_test <- var.test(lefthanded, righthanded, conf.level = 0.97)
print(f_test)

#c
equal_variances <- f_test$p.value > 0.03

#t-test
t_test <- t.test(var_left, var_left, var.equal = TRUE, conf.level = 0.97)
t_test

if(t_test$p.value <= 0.03) {
  print("Reject the null hypothesis <- There is a significant difference in the mean reaction times between left-handed and right-handed individuals.")
} else {
  print("Do not reject the null hypothesis <- There is no significant difference in the mean reaction times between left-handed and right-handed individuals.")
}