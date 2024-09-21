library(ggplot2)
library(reshape2)

# Data with NA replaced by 15
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  City_A = c(5, 15, 16, 17, 18),
  City_B = c(10, 6, 7, 8, 9),
  City_C = c(15, 11, 12, 13, 14)
)

# Melt and plot
melted_data <- melt(data, id.vars="Month")
ggplot(melted_data, aes(x=Month, y=variable, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  labs(title="Heatmap of City Data", x="Month", y="City")
