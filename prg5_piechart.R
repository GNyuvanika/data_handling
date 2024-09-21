# Data
companies <- c("A", "B", "C", "D")
market_share <- c(20, 30, 25, 25)

# Pie chart
pie(market_share, 
    labels=companies, 
    main="Market Share of Companies", 
    col=c("lightblue", "lightgreen", "lightpink", "lightyellow"))
