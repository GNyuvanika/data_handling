student_data <- data.frame(Student = c("A", "B", "C", "D", "E"), 
                           Math_Score = c(85, 72, 90, 78, 88), 
                           Science_Score = c(78, 95, 92, 98, 85), 
                           Attendance = c(93, 85, 80, 75, 82))
matplot(t(student_data[, -1]), type = "o", pch = 1, col = 1:3, xaxt = "n", 
        xlab = "Student", ylab = "Score / Attendance", main = "Performance Trends")
axis(1, 1:5, student_data$Student)