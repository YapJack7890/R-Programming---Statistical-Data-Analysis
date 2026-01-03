#1)
library(readxl)

df <- read_excel("Dataset 1.xlsx")
#print(head(df))

# Filter the data frame
filtered_df <- df[df$Age >= 6 & df$Age <= 80, ]
#print(filtered_df)

mean_value <- mean(filtered_df$'Reaction Time (ms)')
print(mean_value)

#2)
quantile_value <- quantile(filtered_df$'Reaction Time (ms)', probs = 0.7)
print(quantile_value)

#3)
hist(filtered_df$'Reaction Time (ms)', xlab = "Reaction Time (ms)", main = "Histogram of Reaction Time (ms)")
mode_value <- as.numeric(names(which.max(table(filtered_df$'Reaction Time (ms)'))))
median_value <- median(filtered_df$'Reaction Time (ms)')
abline(v = median_value, col = "red", lwd = 2)
text(median_value, par("usr")[4] - 0.05 * diff(par("usr")[3:4]), "Median", col = "red", pos = 4)

abline(v = mean_value, col="green", lwd = 2)
text(mean_value, par("usr")[4] - 0.1 * diff(par("usr")[3:4]), "Mean", col = "green", pos = 4)

abline(v = mode_value, col="blue", lwd = 2)
text(mode_value, par("usr")[4] - 0.15 * diff(par("usr")[3:4]), "Mode", col = "blue", pos = 4)

density_estimate <- density(filtered_df$'Reaction Time (ms)')
plot(density_estimate, 
     xlab = "Reaction Time (ms)", 
     main = "Density Plot of Reaction Time (ms)")
abline(v = median_value, col = "red", lwd = 2)
text(median_value, par("usr")[4] - 0.05 * diff(par("usr")[3:4]), "Median", col = "red", pos = 4)

abline(v = mean_value, col="green", lwd = 2)
text(mean_value, par("usr")[4] - 0.1 * diff(par("usr")[3:4]), "Mean", col = "green", pos = 4)

boxplot(filtered_df$'Reaction Time (ms)', xlab = "Boxplot for Reaction Time(ms)", ylab = "Reaction Time(ms)")

qqnorm(filtered_df$'Reaction Time (ms)', ylab = "Reaction Time(ms)", main = "Q-Q plot of Reaction Time(ms)")
qqline(filtered_df$'Reaction Time (ms)')

#4)
#Influence of age on the reaction time
a.correlation <- cor(filtered_df$Age, filtered_df$'Reaction Time (ms)')
print(a.correlation)

plot(filtered_df$Age, filtered_df$`Reaction Time (ms)`,
     main = "Scatter Plot of Age vs Reaction Time",
     xlab = "Age",
     ylab = "Reaction Time (ms)",
     pch = 16)

a_lm_model <- lm(`Reaction Time (ms)` ~ Age, data = filtered_df)

# Add regression line
abline(a_lm_model, col = "red")

par(mfrow = c(1,2)) 
boxplot(filtered_df$Age, main="Age") 
boxplot(filtered_df$`Reaction Time (ms)`, main="Reaction Time (ms)") 

#Influence of memory score on the reaction time
par(mfrow = c(1,1)) 
m.correlation <- cor(filtered_df$'Memory Score', filtered_df$'Reaction Time (ms)')
print(m.correlation)

plot(filtered_df$'Memory Score', filtered_df$`Reaction Time (ms)`,
     main = "Scatter Plot of Memory Score vs Reaction Time",
     xlab = "Memory Score",
     ylab = "Reaction Time (ms)",
     pch = 16)

m_lm_model <- lm(`Reaction Time (ms)` ~ `Memory Score`, data = filtered_df)

# Add regression line
abline(m_lm_model, col = "red")

par(mfrow = c(1,2)) 
boxplot(filtered_df$`Memory Score`, main="Memory Score")
boxplot(filtered_df$`Reaction Time (ms)`, main="Reaction Time (ms)") 
 

#Influence of attention score on the reaction time
par(mfrow = c(1,1)) 
at.correlation <- cor(filtered_df$'Attention Score', filtered_df$'Reaction Time (ms)')
print(at.correlation)

plot(filtered_df$'Attention Score', filtered_df$`Reaction Time (ms)`,
     main = "Scatter Plot of Attention Score vs Reaction Time",
     xlab = "Attention Score",
     ylab = "Reaction Time (ms)",
     pch = 16)

lm_model <- lm(`Reaction Time (ms)` ~ `Attention Score`, data = filtered_df)

# Add regression line
abline(lm_model, col = "red")

par(mfrow = c(1,2)) 
boxplot(filtered_df$`Attention Score`, main="Attention Score")
boxplot(filtered_df$`Reaction Time (ms)`, main="Reaction Time (ms)") 

