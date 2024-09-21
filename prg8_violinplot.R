install.packages('vioplot')
# Load the vioplot package
library(vioplot)

# Data
group <- c("X", "X", "X", "X", "Y", "Y", "Y", "Y")
score <- c(80, 85, 78, 92, 88, 76, 80, 84)

# Violin plot
vioplot(score[group == "X"], score[group == "Y"], 
        names = c("X", "Y"), 
        col = c("lightblue", "lightgreen"),
        main = "Violin Plot of Scores by Group", 
        xlab = "Group", 
        ylab = "Scores")
