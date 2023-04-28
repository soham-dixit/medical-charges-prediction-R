# Load required packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(caret)
library(MLmetrics)
library(kernlab)

#setwd("D:/Degree/SEM4/DS CP")

# Load the saved linear regression model
model <- readRDS("medical_charges_model.rds")

# Load the saved ensemble model
loaded_model_list <- readRDS("ensemble_model.rds")
model1 <- loaded_model_list$model1
model2 <- loaded_model_list$model2

# Define the user interface
ui <- fluidPage(
  
  # App title
  titlePanel("Medical Cost Prediction"),
  
  # Define the tabset with two tabs
  tabsetPanel(
    tabPanel("Linear Regression", 
             # Sidebar panel with input widgets for Linear Regression
             br(),
             sidebarPanel(
               # Age input field
               numericInput(inputId = "age", label = "Age:", min = 0, max = 100, value = 30),
               
               # BMI input field
               numericInput(inputId = "bmi", label = "BMI:", min = 10, max = 50, value = 25),
               
               # Children input field
               numericInput(inputId = "children", label = "Children:", min = 0, max = 10, value = 0),
               
               # Smoker input field
               selectInput(inputId = "smoker", label = "Smoker:", choices = c("yes", "no"), selected = "no")
             ),
             
             # Main panel with output widgets for Linear Regression
             mainPanel(
               # Predicted charges display
               h3("Predicted Charges:"),
               verbatimTextOutput(outputId = "prediction_linear"),
             ),
    ),
    tabPanel("Ensemble Learning",
             # Sidebar panel with input widgets for Ensemble Learning
             br(),
             sidebarPanel(
               # Age input field
               numericInput(inputId = "age", label = "Age:", min = 0, max = 100, value = 30),
               
               # BMI input field
               numericInput(inputId = "bmi", label = "BMI:", min = 10, max = 50, value = 25),
               
               # Children input field
               numericInput(inputId = "children", label = "Children:", min = 0, max = 10, value = 0),
               
               # Smoker input field
               selectInput(inputId = "smoker", label = "Smoker:", choices = c("yes", "no"), selected = "no"),
               
               # Gender input field
               selectInput(inputId = "gender", label = "Gender:", choices = c("male", "female"), selected = "male"),
               
               # Region input field
               selectInput(inputId = "region", label = "Region:", choices = c("southwest", "southeast", "northwest", "northeast"), selected = "northeast")
             ),
             
             # Main panel with output widgets for Ensemble Learning
             mainPanel(
               # Predicted charges display
               h3("Predicted Charges:"),
               verbatimTextOutput(outputId = "prediction_ensemble"),
             )
             
    ),
    tabPanel("Model Evaluation",
             # Main panel with output widgets for Model Evaluation
             mainPanel(
               h3("Model Evaluation"),
               tableOutput(outputId = "model_metrics")
             )
    )
    
  )
)

# Define the server logic
server <- function(input, output) {
  
  test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Create a reactive function to make predictions based on user inputs for Linear Regression
  predicted_charges_linear <- reactive({
    new_observation <- data.frame(age = input$age, bmi = input$bmi, children = input$children, smoker = input$smoker)
    predict(model, new_observation)
  })
  
  predicted_charges_ensemble <- reactive({
    new_observation <- data.frame(age = input$age, bmi = input$bmi, children = input$children, smoker = input$smoker, gender = input$gender, region = input$region)
    prediction1 <- predict(model1, new_observation)
    prediction2 <- predict(model2, new_observation)
    prediction <- (prediction1 + prediction2) / 2
    prediction
  })
  
  output$prediction_linear <- renderPrint({
    paste0(round(predicted_charges_linear(), 2))
  })
  
  output$prediction_ensemble <- renderPrint({
    paste0(round(predicted_charges_ensemble(), 2))
  })
  
  output$model_metrics <- renderTable({
    test$predicted_charges_linear <- predict(model, test)
    #test$predicted_charges_ensemble <- predict(ensemble_model, test)
    
    pred1 <- predict(model1, test)
    pred2 <- predict(model2, test)
    
    ensemble_pred <- (pred1 * 0.5) + (pred2 * 0.5)
    
    test$predicted_charges_ensemble <- ensemble_pred
    
    metrics <- data.frame(Model = c("Linear Regression", "Ensemble Learning"),
                          MAE = c(mean(abs(test$charges - test$predicted_charges_linear)), mean(abs(test$charges - test$predicted_charges_ensemble))),
                          RMSE = c(sqrt(mean((test$charges - test$predicted_charges_linear)^2)), sqrt(mean((test$charges - test$predicted_charges_ensemble)^2))))
    return(metrics)
  })
}

shinyApp(ui = ui, server = server)