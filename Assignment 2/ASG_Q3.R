#Linear regression model for respond time
# y-axis = reaction time(dep var)
# Q3a
#import xlsx file
library(readxl)
data <- read_excel("dataset.xlsx")
head(data)

#rename col names
colnames(data) <- c("Age", "Gender", "Hand_Dominance", "Physical_Activity_Frequency", "Reaction_Time")

# Convert categorical variables to factors
data$Gender <- as.factor(data$Gender)
data$Hand_Dominance <- as.factor(data$Hand_Dominance)
data$Physical_Activity_Frequency <- as.factor(data$Physical_Activity_Frequency)

#Fit linear regression model
model <- lm(Reaction_Time ~ Age + Gender + Hand_Dominance + Physical_Activity_Frequency, data = data)
#summarize the model
summary(model)

#Check the Model, Histogram
model_residuals = model$residuals
#Plot the result
hist(model_residuals)
# it is normally dist

#Use qqplot check
qqnorm(model_residuals)
qqline(model_residuals)

# Q3b
# Calc reaction time change for an additional 10year in Age
age_effect <- coef(model)["Age"] *10
print(paste("Change in reaction time for an additional 10 years in age is:", 
            age_effect, "milliseconds"))

# Q3c Anova test
#R Code Generated ANOVA approach
anova_result <- anova(model)
print(anova_result)

#Hand Written ANOVA approach
#predicted values
predicted_values <- predict(model)
#SSR
SSR <- sum((predicted_values - mean(data$Reaction_Time))^2)
#SSE
SSE <- sum((data$Reaction_Time - predicted_values)^2)
#SST
SST <- SSE + SSR

# Total number of observations
n <- nrow(data)

#df(R)
df_R <- length(coefficients(model)) - 1
df_R
#df(E)
df_E <- n - (DF_regression + 1)
df_E

#MSR
MSR <- SSR / df_R
#MSE
MSE <- SSE / df_E

#F
F_statistic <- MSR / MSE
#p-value
f_value <- qf(0.05, DF_regression, DF_residuals, lower.tail = FALSE)

#ANOVA table
ANOVA_table <- data.frame(
  Source = c("Regression", "Residuals", "Total"),
  Sum_Sq = c(SSR, SSE, SST),
  DF = c(DF_regression, DF_residuals, n - 1),
  Mean_Sq = c(MSR, MSE, NA), 
  Test_Statistic = c(F_statistic, NA, NA),  
  p_value = c(f_value, NA, NA)  
)
#Print ANOVA table
print(ANOVA_table)

# Q3d Generate diagnostic plots
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

