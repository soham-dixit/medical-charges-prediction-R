# Load required packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(caret)
library(MLmetrics)

setwd("D:/Degree/SEM4/DS CP")

# Load the saved linear regression model
loaded_model_list <- readRDS("ensemble_model.rds")

loaded_model1 <- loaded_model_list$model1
loaded_model2 <- loaded_model_list$model2

# Define the user interface
ui <- fluidPage(
  
  # App title
  titlePanel("Medical Cost Prediction - Ensemble Learning"),
  
  # Sidebar panel with input widgets
  sidebarPanel(
    # Age input field
    numericInput(inputId = "age", label = "Age:", min = 0, max = 100, value = 30),
    
    # BMI input field
    numericInput(inputId = "bmi", label = "BMI:", min = 10, max = 50, value = 25),
    
    # Children input field
    numericInput(inputId = "children", label = "Children:", min = 0, max = 10, value = 0),
    
    selectInput(inputId = "smoker", label = "Smoker:", choices = c("yes", "no"), selected = "no"),
    
    selectInput(inputId = "gender", label = "Gender:", choices = c("male", "female"), selected = "male"),
    
    selectInput(inputId = "region", label = "region:", choices = c("southwest", "southeast", "northwest", "northeast"), selected = "northeast")
  ),
  
  # Main panel with output widgets
  mainPanel(
    # Predicted charges display
    h3("Predicted Charges:"),
    verbatimTextOutput(outputId = "prediction"),
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Create a reactive function to make predictions based on user inputs
  predicted_charges <- reactive({
    new_observation <- data.frame(age = input$age, bmi = input$bmi, children = input$children, smoker = input$smoker, gender=input$gender, region=input$region)
    lm_pred = predict(model1, new_observation)
    svm_pred = predict(model2, new_observation)
    ensemble_pred <- (0.5 * lm_pred) + (0.5 * svm_pred)
  })
  
  # Display the predicted charges
  output$prediction <- renderPrint({
    paste0(round(predicted_charges(), 2))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
