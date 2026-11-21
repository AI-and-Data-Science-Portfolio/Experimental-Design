# Question One

# Import data
data1 = read.csv("Assignment 2 Data 1.csv")

# Create model
lmod1 = lm(Time ~ (RLength + RWidth + Paper + BLength + FLength)^2, data=data1)
summary = summary(lmod1)
round(summary$coefficients, 2)

# Create main effects and interaction plots
library(FrF2)
dev.new(width = 12, height = 6)
par(cex.lab = 0.65)
par(mar = c(4, 5, 3, 5) + 0.1)
MEPlot(lmod1, main = "Main effects plot for Flight Time")
title(xlab = "Factor Levels", font.lab = 1, line = 2.3)

dev.new(width = 12, height = 6)
par(cex.lab = 1)
par(mar = c(4, 5, 3, 3) + 0.1)
IAPlot(lmod1, main = "Interaction effects for Flight Time")

# Half normal plot
library(FrF2)
halfnormal(lmod1, main = "Half Normal Plot", 
           pch = 16, col = "darkred", ylim = c(0, max(data1)))
n = length(coef(lmod1))  
reference_value <- qnorm(1 - 0.05 / (2 * n)) 
print(reference_value)
abline(h = reference_value, col = "blue", lty = 2) 

# Lenth PSE
library(unrepx)
pse = PSE(coef(lmod1)[-1], method = "Lenth")
round(pse, 3)

# Statistical significance using Lenth PSE
df = round(n/3)
alpha = 0.05
t = qt(1 - alpha/2, df)
critical_value = pse * t
round(critical_value, 2)

# Pareto plot
library(pid)
paretoPlot(lmod1)

##################################################################################################
# Question Two

# Import data
data2 = read.csv("Assignment 2 Data 2.csv")
data2$A = as.factor(data2$A)
data2$B = as.factor(data2$B)
data2$WholePlot = as.factor(data2$WholePlot)
data2$Rep = as.factor(data2$Rep)

summary(data2$Time)

# Main effects plots
dev.off()
par(mfrow = c(1, 2))
means_A = tapply(data2$Time, 
                      data2$A, mean)
plot(means_A, type = "b", pch = 16, lwd = 2, col = "blue",
     xlab = "Temperature", ylab = "Time (secs)",
     main = "Main Effect: Temperature", xaxt = "n",
     ylim = c(0, max(means_A) * 1.2),
     xlim = c(0.75, max(means_A) * .011))
axis(1, at = 1:2, labels = c("Cold", "Hot"))

means_B = tapply(data2$Time, 
                     data2$B, mean)
plot(means_B, type = "b", pch = 16, lwd = 2, col = "red",
     xlab = "Stirring", ylab = "Time (secs)",
     main = "Main Effect: Stirring", xaxt = "n",
     ylim = c(0, max(means_A) * 1.2),
     xlim = c(0.75, max(means_A) * .011))
axis(1, at = 1:2, labels = c("No Stirring", "Stirring"))
axis_ticks = seq(0, ceiling(max(means_B) * 1.2), by = 1)

# Interaction plot
dev.off()
interaction.plot(x.factor=data2$B,
                 trace.factor = data2$A,
                 response=data2$Time,
                 main = "Interaction of Stirring with Temperature",
                 xlab = "Stirring", 
                 ylab = "Time (secs)",
                 trace.label = "Temperature",
                 col = 1:2,
                 lwd = 2,
                 legend = TRUE,
                 bty = "l")

# ANOVA
mod2 = aov(Time ~ A * B + Error(WholePlot), data=data2)
summary(mod2)

# Deleted Standardised Residuals vs Theoretical Quantiles
dev.off()
library(olsrr)

ols_plot_resid_stud_fit(mod2$WholePlot)

ols_plot_resid_stud_fit(mod2$Within)

# Deleted Standardised Residuals vs Theoretical Quantiles
dev.off()
library(car)
qqPlot(rstudent(mod2$WholePlot), xlab="Theoretical Quantiles", 
       ylab="Deletion standardized residuals",
       main = "Deletion Q-Q plot (Whole Plots)")

qqPlot(rstudent(mod2$Within), xlab="Theoretical Quantiles", 
       ylab="Deletion standardized residuals",
       main = "Deletion Q-Q plot (Subplots")







