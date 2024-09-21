product_data <- data.frame(Price = c(50, 70, 60, 45, 55), 
                           Rating = c(4.2, 3.8, 4.0, 4.5, 3.9), 
                           Age_Group = c("25-35", "35-45", "18-25", "45-55", "25-35"))

plot(product_data$Price, product_data$Rating, pch = as.numeric(factor(product_data$Age_Group)), 
     col = as.numeric(factor(product_data$Age_Group)), xlab = "Price ($)", ylab = "Rating", 
     main = "Price vs Rating")

legend("bottomright", legend = unique(product_data$Age_Group), pch = 1:4, col = 1:4, title = "Age Group")
