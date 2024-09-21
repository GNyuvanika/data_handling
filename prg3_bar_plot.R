# Data
products <- c("A", "B", "C", "D", "E")
sales <- c(300, 450, 500, 350, 400)

# Bar plot
barplot(sales, 
        names.arg=products, 
        main="Sales of Products",
        xlab="Product", 
        ylab="Sales",
        col="blue", 
        border="black")
