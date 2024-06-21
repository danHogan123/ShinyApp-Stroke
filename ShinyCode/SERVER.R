library(shiny)
library(readr)
library(tidyverse)
library(scales)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

shinyServer(function(input, output){
  
  ## CODE FOR PAGE 2 SCATTERPLOT
  output$scatterplot <- renderPlot({
   
    if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
      
      ggplot(strokeDataSet, aes(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]])) +
        geom_point(aes(color = strokeDataSet[[input$catvar]])) + labs(x = input$xvar, y = input$yvar)
      #+ scale_y_continuous(breaks = y_breaks)
      #+ ylim(min(strokeDataSet[[input$yvar]],na.rm=T), max(strokeDataSet[[input$yvar]],na.rm=T))
    } else {
      # If one or both variables are not selected, display an empty plot
      return(plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot"))
    }
  })
  
  
  
  
    
  output$dynamic_inputs <- renderUI({
    predictor_inputs <- lapply(input$predictors, function(pred) {
      if (pred == "Gender") {
        selectInput(
          inputId = "input_Gender",
          label = "Gender:",
          choices = c("Male", "Female"),
          selected = "Male"
        )
      }
      else if (pred == "Age"){
        sliderInput(
          inputId = "input_Age",
          label = "Age",
          18, 90, 30
        )
      }
      else if (pred == "Hypertension"){
        selectInput(
          inputId = "input_Hypertension",
          label = "Hypertension",
          choices = c("Yes", "No"),
          selected = "Yes"
        )
      }
      else if (pred == "HeartDisease"){
        selectInput(
          inputId = "input_HeartDisease",
          label = "Heart Disease",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "EverMarried"){
        selectInput(
          inputId = "input_EverMarried",
          label = "Ever Married",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "WorkType"){
        selectInput(
          inputId = "input_WorkType",
          label = "Work Type",
          choices = c("Private", "Government", "Self-Employed", "Never Worked"),
          selected = "Private"
        )}
      else if (pred == "ResidenceType"){
        selectInput(
          inputId = "input_ResidenceType",
          label = "Residence Type",
          choices = c("Urban", "Rural"),
          selected = "Urban"
        )}
      else if (pred == "AvgGlucoseLevel"){
        numericInput( # Subject to change yall
          inputId = "input_AvgGlucoseLevel",
          label = "Average Glucose Level",
          min = 50, max = 300, value = 140
        )}
      else if (pred == "SmokingStatus"){
        selectInput(
          inputId = "input_SmokingStatus",
          label = "Smoking Status",
          choices = c("Never Smoked", "Smokes", "Formerly Smoked"),
          selected = "Never Smoked" 
        )}
      else if (pred == "Height and Weight"){ #Does not work right"
        list(
          tagList(
            numericInput( # Subject to change yall
              inputId = "input_height",
              label = "Height in inches",
              min = 50, max = 300, value = 140
            ),
          ),
          tagList(
            numericInput( # Subject to change yall
              inputId = "input_weight",
              label = "Weight in lbs",
              min = 50, max = 300, value = 140
            )
          )
        )
    }
      else {
        numericInput(
          inputId = paste0("input_", pred),
          label = pred,
          value = 0
        )
      }
    })
    
    # Split the list of UI elements into two equal parts
    n <- length(predictor_inputs)
    
    if (n == 1) {
      # If there is only one predictor, place it in a single column
      fluidRow(
        column(12, do.call(tagList, predictor_inputs))
      )
    } else {
    
    half <- ceiling(n / 2)
    col1 <- predictor_inputs[1:half]
    col2 <- predictor_inputs[(half + 1):n]
    
    # Arrange the elements in two columns
    fluidRow(
      column(6, do.call(tagList, col1)),
      column(6, do.call(tagList, col2))
    )
    }
    #do.call(tagList, predictor_inputs)
  })
  
  
  observeEvent(input$predict, {
    req(input$predictors)
    
    # Prepare the data for logistic regression
    predictors <- input$predictors[input$predictors != "Gender"]  # Exclude Gender from predictors for model building
    formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
    model <- glm(formula, data = strokeDataSet, family = binomial)
    
    # Gather input values for prediction
    new_data <- data.frame(matrix(ncol = length(predictors), nrow = 1))
    colnames(new_data) <- predictors
    for (pred in predictors) {
      new_data[[pred]] <- input[[paste0("input_", pred)]]
    }
    
    # Add Gender to the new_data for prediction
    new_data$Gender <- input$input_Gender
    
    # Predict stroke probability
    prediction <- predict(model, newdata = new_data, type = "response")
    
    # Display the prediction result
    output$prediction_result <- renderText({
      paste("Predicted probability of stroke:", round(prediction, 4))
    })
  })
  
  
  
  
  calculatedBMI <- reactive({
    if (input$useBMI) {
      return(input$weightInput / (input$heightInput ^ 2))
    }
    return(NULL)
  })
  
  categorizeBMI <- function(bmi) {
    if (is.na(bmi)) return(NA)
    if (bmi < 18.5) return("Underweight")
    if (bmi >= 18.5 & bmi < 24.9) return("Normal weight")
    if (bmi >= 25 & bmi < 29.9) return("Overweight")
    if (bmi >= 30) return("Obese")
    if (bmi >= 35) return ("Severely obese")
  }
  
  categorizedBMI <- reactive({
    bmi <- calculatedBMI()
    return(categorizeBMI(bmi))
  })
  
  
  regressionResult <- reactiveVal(NULL)
  
  
  
  observeEvent(input$runRegression, {
    # Define the formula dynamically
    predictors <- c()
    if (input$useAge) {
      predictors <- c(predictors, "Age")
    }
    if (input$useBMI) {
      predictors <- c(predictors,"BMI_Category")
    }
    
    if (length(predictors) == 0) {
      regressionResult("Please select at least one predictor.")
      return()
    }
    
    formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
    print(formula)
    # Fit logistic regression model
    model <- glm(formula, data = StrokeData, family = binomial)
    
    # Create newdata dataframe based on selected inputs
    newdata <- data.frame(
      Age = if (input$useAge) input$ageInput else NA,
      BMI_Category = if (input$useBMI) categorizedBMI() else NA
    )
    
    # Make a prediction for the selected age and/or bmi
    
    prediction <- predict(model, newdata = newdata, type = "response")
    
    # Store the model summary and prediction
    regressionResult(list(model_summary = summary(model), prediction = prediction))
  })
  
  output$regressionResults <- renderPrint({
    result <- regressionResult()
    if (is.null(result)) {
      "Run the regression to see the results."
    } else if (is.character(result)) {
      result
    } else {
      cat("Model Summary:\n")
      print(result$model_summary)
      cat("\nPrediction for selected values:\n")
      print(result$prediction)
    }
  })
  
  
  
  
  

  
})