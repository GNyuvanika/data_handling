# Year and Revenue data
year <- c(2015, 2016, 2017, 2018, 2019)
revenue <- c(30, 35, 40, 45, 50)

# Create the area plot
plot(year, revenue, type = "n", xlab = "Year", ylab = "Revenue (in million)", main = "Revenue Over the Years")
polygon(c(year, rev(year)), c(revenue, rep(0, length(revenue))), col = "lightblue", border = "blue")

# Add the points for clarity
points(year, revenue, pch = 16, col = "blue")


