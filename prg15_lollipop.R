# Data
region <- c("North", "South", "East", "West")
sales <- c(150, 200, 180, 210)

# Create the lollipop plot
plot(sales, 1:4, pch = 16, xlim = c(0, max(sales) + 10), xlab = "Sales", ylab = "Region", yaxt = "n", main = "Sales by Region")

# Add the lines (sticks of the lollipops)
segments(0, 1:4, sales, 1:4, col = "blue", lwd = 2)

# Add region labels
axis(2, at = 1:4, labels = region)
