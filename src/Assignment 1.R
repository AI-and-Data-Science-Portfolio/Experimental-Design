# Experiment One

# Import data
data1 = read.csv("Assignment 1 Data 1.csv")
data1$Block = as.factor(data1$Block)
data1$Length = as.factor(data1$Length)

# Create plots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

for(block_num in 1:6) {
  block_data = data1[data1$Block == block_num, ]
  
  block_data = block_data[order(block_data$Length), ]

  plot(block_data$Time, 
       type = "o", 
       col = "blue", 
       lwd = 2,
       pch = 16,
       xaxt = "n",
       xlab = "Length", 
       ylab = "Time (sec)",
       main = paste("Block", block_num),
       ylim = range(data1$Time))  
  axis(1, at = 1:nrow(block_data), labels = block_data$Length)
  grid()}

# ANOVA
mod1 = aov( Time ~ Block + Length, data = data1 )
summary(mod1)

# ANOVA without blocking
mod1b = aov( Time ~ Length, data = data1 )
summary(mod1b)

# Deleted Studentised Residuals vs Predicted values
lmod1 = lm( Time ~ Length, data = data1 )
library(olsrr)
ols_plot_resid_stud_fit(lmod1)

# Breusch-Pagan test for homoscedasticity
library(lmtest)
bptest(lmod1)

# Deleted Standardised Residuals vs Theoretical Quantiles
dev.off()
library(car)
qqPlot(rstudent(lmod1), xlab="Theoretical Quantiles", ylab="Deletion Studentised Residuals",main = "Deletion Q-Q plot")

# Shapiro-Wilk test for normality
shapiro.test(lmod1$residuals)

# Check for linear and quadratic trend
data1$Length = as.numeric(factor(data1$Length, 
                                      levels = c("A", "B", "C", "D"),
                                      labels = c(9, 8, 7, 6)))

linear_model = lm(Time ~ Length, data = data1)
summary(linear_model)

quadratic_model = lm(Time ~ Length + I(Length^2), data = data1)
summary(quadratic_model)

##################################################################################################
# Experiment Two

# Import data
data2 = read.csv("Assignment 1 Data 2.csv")
data2$Length = as.factor(data2$Length)

# Main effects plots
par(mfrow = c(1, 2))
means_length = tapply(data2$Time, 
                            data2$Length, mean)
plot(means_length, type = "b", pch = 16, lwd = 2, col = "blue",
     xlab = "Wing Length", ylab = "Time (Seconds)",
     main = "Main Effect: Wing Length", xaxt = "n",
     ylim = c(1.2, max(means_length) * 1.1))
axis(1, at = 1:4, labels = c("A", "B", "C", "D"))

means_paper = tapply(data2$Time, 
                     data2$Paper, mean)
plot(means_paper, type = "b", pch = 16, lwd = 2, col = "red",
     xlab = "Paper", ylab = "Time (Seconds)",
     main = "Main Effect: Paper", xaxt = "n", 
     ylim = c(1.2, max(means_paper) * 1.1))
axis(1, at = 1:2, labels = c("H", "L"))

# Interaction plot
interaction.plot(x.factor=data2$Paper,
                 trace.factor = data2$Length,
                 response=data2$Time,
                 main = "Interaction of Paper with Length",
                 xlab = "Paper", 
                 ylab = "Time (Seconds)",
                 trace.label = "Length",
                 col = 1:4,
                 lwd = 2,
                 legend = TRUE,
                 bty = "l")

# ANOVA
mod2 = aov( Time ~  Paper * Length, data = data2 )
summary(mod2)

# Deleted Studentised Residuals vs Predicted values
lmod2 = lm( Time ~ Paper * Length, data = data2 )
library(olsrr)
ols_plot_resid_stud_fit(lmod2)

# Test for homoscedasticity
library(lmtest)
bptest(lmod2)

# Deleted Standardised Residuals vs Theoretical Quantiles
dev.off()
library(car)
qqPlot(rstudent(lmod2), xlab="Theoretical Quantiles", 
       ylab="Deletion standardized residuals",
       main = "Deletion Q-Q plot")

# Shapiro-Wilk test for normality
shapiro.test(lmod2$residuals)

# Table of LSMs
library(emmeans)
lsmeans_table = emmeans(mod2, ~ Paper * Length)
library(dplyr)
lsmeans_ordered <- lsmeans_table %>%
  as.data.frame() %>%
  arrange(emmean)
lsmeans_ordered

# With influential points removed
data2b = data2[-c(9,18),]

bptest(lmod2b)

shapiro.test(lmod2b$residuals)

