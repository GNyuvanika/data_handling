# Month and Cumulative Sales data
month <- c("Jan", "Feb", "Mar", "Apr", "May")
sales <- c(100, 200, 300, 400, 500)

# Create the step plot
plot(1:5, sales, type = "s", xaxt = "n", xlab = "Month", ylab = "Cumulative Sales", main = "Cumulative Sales Step Plot", col = "blue")

# Set the x-axis labels to months
axis(1, at = 1:5, labels = month)

# Add points for clarity
points(1:5, sales, pch = 16, col = "blue")
