library(shiny)
library(readr)
library(tidyverse)
library(scales)
library(DT)
library(ggplot2)
library(plotly)


server <- function(input, output) {
  
  # Your reactive expressions, observers, and outputs should be defined within this function
  
  calculatedBMI <- reactive({
    if (input$useBMI) {
      return(input$weightInput / (input$heightInput ^ 2))
    }
    return(NULL)
  })
  
  # FUNCTION THAT TAKES NUMERICAL BMI AND RETURNS CATEGORICAL VAR TYPE
  categorizeBMI <- function(bmi) {
    if (is.na(bmi)) return(NA)
    if (bmi < 18.5) return("Underweight")
    if (bmi >= 18.5 & bmi < 24.9) return("Normal weight")
    if (bmi >= 25 & bmi < 29.9) return("Overweight")
    if (bmi >= 30) return("Obese")
    if (bmi >= 35) return("Severely obese")
  }
  
  # FUNCTION THAT GETS THE CATEFORIZED BMI   
  categorizedBMI <- reactive({
    bmi <- calculatedBMI()
    return(categorizeBMI(bmi))
  })
  
  # DEFINED REACTIVE VALUE THAT AUTO UPDATES WHENEVER IT IS CHANGED IN OTHER LINES
  regressionResult <- reactiveVal(NULL)
  
  savedResults <- reactiveVal(list())
  
  # UPDATES THE PREDICTOR VECTOR WHENEVER THE REGRESSION MODEL IS RUN
  observeEvent(input$runRegression, {
    predictors <- c()
    if (input$useAge) {
      predictors <- c(predictors, "Age")
    }
    if (input$useBMI) {
      predictors <- c(predictors, "BMI_Category")
    }
    if (input$useSex) {
      predictors <- c(predictors, "Gender")
    }
    if (input$useHyper) {
      predictors <- c(predictors, "Hypertension")
    }
    if (input$useHeart) {
      predictors <- c(predictors, "HeartDisease")
    }
    if (input$useMar) {
      predictors <- c(predictors, "EverMarried")
    }
    if (input$useWork) {
      predictors <- c(predictors, "WorkType")
    }
    if (input$useRes) {
      predictors <- c(predictors, "ResidenceType")
    }
    if (input$useSmoke) {
      predictors <- c(predictors, "SmokingStatus")
    }
    if (input$useGluc) {
      predictors <- c(predictors, "Glucose_Category")
    }
    if (length(predictors) == 0) {
      regressionResult("Please select at least one predictor.")
      return()
    }
    
    # FORMULA THAT SAVES THE VALUES THE USER CHOSE
    formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
    print(formula)
    
    # FINAL MODEL IN USE AS RECIPE MODEL IS NOT WORKING ATM
    FinalModel <- glm(formula, data = StrokeData, family = binomial)
    
    # TAKES USERS SAVED VALUES AND STORES THEM IN A DATA FRAME
    newdata <- data.frame(
      Age = if (input$useAge) input$ageInput else NA,
      BMI_Category = if (input$useBMI) categorizedBMI() else NA,
      Gender = if (input$useSex) input$sexInput else NA,
      Hypertension = if (input$useHyper) input$hyperInput else NA,
      HeartDisease = if (input$useHeart) input$heartInput else NA,
      EverMarried = if (input$useMar) input$marInput else NA,
      WorkType = if (input$useWork) input$workInput else NA,
      ResidenceType = if (input$useRes) input$resInput else NA,
      SmokingStatus = if (input$useSmoke) input$smokeInput else NA,
      Glucose_Category = if (input$useGluc) input$glucInput else NA
    )
    
    # THE STROKE PREDICTION FOR THE USER
    StrokePrediction <- predict(FinalModel, newdata = newdata, type = "response")
    result <- list(model_summary = summary(FinalModel), prediction = StrokePrediction, predictors = predictors)
    regressionResult(result)
    # Store the model summary and prediction
    regressionResult(list(model_summary = summary(FinalModel), prediction = StrokePrediction))
    savedResults(c(savedResults(), list(result)))
  })
  
  # Output functions (these should be within the server function)
  
  output$dataTable <- renderDT({
    datatable(StrokeData %>% select(input$variables))
  })
  
  # Summary Statistics
  output$summaryStats <- renderPrint({
    summary(StrokeData[[input$statVariable]])
  })
  
  # Visualization
  output$plot <- renderPlot({
    p <- ggplot(StrokeData, aes_string(x = input$plotVariable))
    if (input$plotType == "Histogram") {
      p <- p + geom_histogram()
    } else if (input$plotType == "Bar Chart") {
      p <- p + geom_bar()
    } else if (input$plotType == "Boxplot") {
      p <- p + geom_boxplot()
    }
    print(p)
  })
  
  # # Display the regression results
  # output$regressionResults <- renderPrint({
  #   result <- regressionResult()
  #   if (is.null(result)) {
  #     "Run the regression to see the results."
  #   } else if (is.character(result)) {
  #     result
  #   } else {
  #     cat("Stroke prediction number:", round(result$prediction, 2))
  #   }
  # })
  output$regressionResults <- renderPrint({
    result <- regressionResult()
    if (is.null(result)) {
      "Run the regression to see the results."
    } else if (is.character(result)) {
      result
    } else {
      cat("Stroke prediction number:", round(result$prediction, 2), "\n")
      cat("Predictors used:", paste(result$predictors, collapse = ", "), "\n")
    }
  })
  
  output$savedResultsTable <- renderTable({
    results <- savedResults()
    if (length(results) == 0) {
      return(NULL)
    }
    
    data <- do.call(rbind, lapply(results, function(result) {
      data.frame(
        Prediction = round(result$prediction, 2),
        Predictors = paste(result$predictors, collapse = ", ")
      )
    }))
    return(data)
  })
  
  observeEvent(input$saveResult, {
    result <- regressionResult()
    if (!is.null(result) && !is.character(result)) {
      savedResults(c(savedResults(), list(result)))
    }
  })
  observeEvent(input$clearTable, {
    savedResults(list())
  })
}

