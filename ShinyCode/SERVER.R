library(shiny)
library(readr)
library(tidyverse)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

shinyServer(function(input, output){
  
  
  output$scatterplot <- renderPlot({
   
    if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
      
      return(ggplot(strokeDataSet, aes(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]])) +
               geom_point(aes(color = strokeDataSet[[input$catvar]])) + labs(x = input$xvar, y = input$yvar))
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
      else if (pred == "Heart Disease"){
        selectInput(
          inputId = "input_HeartDisease",
          label = "Heart Disease",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "Ever Married"){
        selectInput(
          inputId = "input_EverMarried",
          label = "Ever Married",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "Work Type"){
        selectInput(
          inputId = "input_WorkType",
          label = "Work Type",
          choices = c("Private", "Government", "Self-Employed", "Never Worked"),
          selected = "Private"
        )}
      else if (pred == "Residence Type"){
        selectInput(
          inputId = "input_ResidenceType",
          label = "Residence Type",
          choices = c("Urban", "Rural"),
          selected = "Urban"
        )}
      else if (pred == "Avg Glucose Level"){
        numericInput( # Subject to change yall
          inputId = "input_AvgGlucoseLevel",
          label = "Average Glucose Level",
          min = 50, max = 300, value = 140
        )}
      else if (pred == "Smoking Status"){
        selectInput(
          inputId = "input_SmokingStatus",
          label = "Smoking Status",
          choices = c("Never Smoked", "Smokes", "Formerly Smoked"),
          selected = "Never Smoked" 
        )}
      else {
        numericInput(
          inputId = paste0("input_", pred),
          label = pred,
          value = 0
        )
      }
    })
    do.call(tagList, predictor_inputs)
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
  
  

  
})