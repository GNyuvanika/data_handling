# Load necessary libraries
library(ggplot2)
library(plotly)

# Weather data
weather <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Temperature = c(10, 12, 8, 15, 14),
  Humidity = c(75, 70, 80, 65, 72),
  Wind_Speed = c(15, 12, 18, 20, 16)
)

# 3D Scatter Plot
plot_ly(weather, x = ~Humidity, y = ~Wind_Speed, z = ~Temperature, type = "scatter3d", mode = "markers")

# 3D Surface Plot
plot_ly(weather, x = ~Humidity, y = ~Wind_Speed, z = ~Temperature, type = "mesh3d")
