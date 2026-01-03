#1)
#a)
library(readxl)

df <- read_excel("dataset.xlsx")

head(df)

n <- 100
mean_rt <- mean(df$`Reaction Time (ms)`)
sd_rt <- sd(df$`Reaction Time (ms)`)
alpha_ <- 0.05
t_value <- qt(alpha_/2, df=(n-1), lower.tail=FALSE)
moe <- t_value*(sd_rt/sqrt(n))
CI <- c(mean_rt-moe, mean_rt+moe)
CI

#c) 
alpha_ <- 0.1
moe <- 0.05
z_value <- qnorm(1-(0.1/2))
sample_required <- (sd_rt*z_value/moe)^2
sample_required <- round(sample_required)
sample_required
