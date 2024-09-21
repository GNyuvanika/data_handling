# Data setup
location_data <- data.frame(
  Location = c("A", "B", "C", "D", "E"),
  Temperature = c(15, 20, 18, 12, 17),
  Humidity = c(65, 70, 68, 60, 72),
  CO2_Levels = c(400, 450, 430, 420, 380)
)

# Reshape data for plotting
data_matrix <- as.matrix(location_data[, 2:4])
barplot(t(data_matrix), beside = TRUE, col = c("skyblue", "orange", "green"), 
        names.arg = location_data$Location, main = "Grouped Bar Chart", 
        xlab = "Location", ylab = "Value")
legend("topright", legend = colnames(data_matrix), fill = c("skyblue", "orange", "green"))
