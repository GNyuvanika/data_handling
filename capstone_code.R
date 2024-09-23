# Load necessary libraries
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
data <- tibble(
  Student_ID = 1:1000,
  Name = paste0("Student", 1:1000),
  Gender = sample(c("Male", "Female"), 1000, replace = TRUE),
  Grade = sample(9:12, 1000, replace = TRUE),
  Subject = sample(c("Math", "Science", "English", "History"), 1000, replace = TRUE),
  Score = round(rnorm(1000, mean = 75, sd = 10)),
  School = sample(c("School A", "School B", "School C", "School D"), 1000, replace = TRUE),
  School_Type = sample(c("Public", "Private"), 1000, replace = TRUE),
  Teacher_Experience = sample(1:15, 1000, replace = TRUE),
  School_Resources = sample(1:10, 1000, replace = TRUE),
  Attendance_Rate = round(runif(1000, 70, 100), 2)
)

# Save dataset to CSV
write.csv(data, "educational_performance_data.csv", row.names = FALSE)

# Load dataset
edu_data <- read.csv("educational_performance_data.csv")

# Check for missing values
edu_data <- edu_data %>%
  drop_na()

# Data transformation (if needed)
edu_data <- edu_data %>%
  mutate(
    Score_Category = case_when(
      Score >= 90 ~ "Excellent",
      Score >= 75 ~ "Good",
      Score >= 60 ~ "Average",
      TRUE ~ "Poor"
    )
  )

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Educational Performance Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("school", "Select School:", choices = unique(edu_data$School), selected = "School A"),
      selectInput("subject", "Select Subject:", choices = unique(edu_data$Subject), selected = "Math"),
      selectInput("grade", "Select Grade:", choices = unique(edu_data$Grade), selected = 9),
      selectInput("gender", "Select Gender:", choices = c("All", "Male", "Female"), selected = "All"),
      sliderInput("scoreRange", "Score Range:", min = 0, max = 100, value = c(60, 100)),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", dataTableOutput("summaryTable")),
        tabPanel("Score Distribution", plotlyOutput("scoreDistPlot")),
        tabPanel("Gender Analysis", plotlyOutput("genderPlot")),
        tabPanel("School Resources", plotlyOutput("resourcesPlot")),
        tabPanel("Attendance vs Score", plotlyOutput("attendancePlot")),
        tabPanel("Teacher Experience", plotlyOutput("experiencePlot"))
      )
    )
  )
)

# Define server logic for the dashboard
server <- function(input, output) {
  # Reactive filtered data based on user input
  filtered_data <- reactive({
    data <- edu_data %>%
      filter(
        School == input$school,
        Subject == input$subject,
        Grade == input$grade,
        Score >= input$scoreRange[1],
        Score <= input$scoreRange[2]
      )
    
    if (input$gender != "All") {
      data <- data %>%
        filter(Gender == input$gender)
    }
    
    return(data)
  })
  
  # Summary table
  output$summaryTable <- renderDataTable({
    filtered_data() %>%
      group_by(School, Subject, Grade, Gender) %>%
      summarise(
        Average_Score = mean(Score),
        Max_Score = max(Score),
        Min_Score = min(Score),
        Average_Attendance = mean(Attendance_Rate)
      )
  })
  
  # Score distribution plot
  output$scoreDistPlot <- renderPlotly({
    plot <- filtered_data() %>%
      ggplot(aes(x = Score)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = "Score Distribution", x = "Score", y = "Count")
    ggplotly(plot)
  })
  
  # Gender analysis plot
  output$genderPlot <- renderPlotly({
    plot <- filtered_data() %>%
      ggplot(aes(x = Gender, fill = Gender)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count")
    ggplotly(plot)
  })
  
  # School resources plot
  output$resourcesPlot <- renderPlotly({
    plot <- filtered_data() %>%
      ggplot(aes(x = School_Resources, y = Score, color = Gender)) +
      geom_point() +
      theme_minimal() +
      labs(title = "School Resources vs Score", x = "School Resources", y = "Score")
    ggplotly(plot)
  })
  
  # Attendance vs Score plot
  output$attendancePlot <- renderPlotly({
    plot <- filtered_data() %>%
      ggplot(aes(x = Attendance_Rate, y = Score, color = Gender)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Attendance Rate vs Score", x = "Attendance Rate", y = "Score")
    ggplotly(plot)
  })
  
  # Teacher experience plot
  output$experiencePlot <- renderPlotly({
    plot <- filtered_data() %>%
      ggplot(aes(x = Teacher_Experience, y = Score, color = Gender)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Teacher Experience vs Score", x = "Teacher Experience (Years)", y = "Score")
    ggplotly(plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
