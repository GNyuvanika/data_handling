# Stock data
stock_data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Stock_Price = c(100, 102, 98, 105, 108),
  Volume_Traded = c(2.5, 3.0, 2.2, 2.8, 3.5)
)

# Bar chart for Stock Price
barplot(stock_data$Stock_Price, names.arg = stock_data$Date, col = "skyblue", 
        main = "Stock Price Over Time", xlab = "Date", ylab = "Stock Price ($)")

# Bar chart for Volume Traded
barplot(stock_data$Volume_Traded, names.arg = stock_data$Date, col = "orange", 
        main = "Volume Traded Over Time", xlab = "Date", ylab = "Volume Traded (millions)")
