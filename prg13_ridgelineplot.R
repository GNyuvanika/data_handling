# Data
city <- c(rep("City1", 3), rep("City2", 3))
temperature <- c(20, 21, 19, 22, 23, 24)

# Plot with lines for each city
matplot(matrix(temperature, ncol = 2), type = "l", col = c("blue", "red"), lwd = 2, xlab = "Observation", ylab = "Temperature", main = "Temperature by City")

# Add legend
legend("topleft", legend = c("City1", "City2"), col = c("blue", "red"), lty = 1, lwd = 2)
