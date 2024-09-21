# Data
month <- c("Jan", "Feb", "Mar")
sales_A <- c(100, 150, 130)
sales_B <- c(120, 140, 160)

# Plot for Product A and B
matplot(1:3, cbind(sales_A, sales_B), type = "l", lty = 1, col = c("blue", "red"), xlab = "Month", ylab = "Sales", xaxt = "n", main = "Spaghetti Plot")
axis(1, at = 1:3, labels = month)

# Add legend
legend("topleft", legend = c("Product A", "Product B"), col = c("blue", "red"), lty = 1)
