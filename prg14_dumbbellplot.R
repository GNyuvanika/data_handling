# Data
product <- c("A", "B", "C", "D", "E")
before <- c(200, 300, 400, 500, 600)
after <- c(250, 350, 450, 550, 650)

# Create the plot
plot(before, 1:5, xlim = range(c(before, after)), pch = 16, col = "red", xlab = "Sales", ylab = "Product", yaxt = "n", main = "Sales Before and After Campaign")
points(after, 1:5, pch = 16, col = "green")

# Add lines connecting before and after
segments(before, 1:5, after, 1:5, col = "blue", lwd = 2)

# Add product labels
axis(2, at = 1:5, labels = product)
