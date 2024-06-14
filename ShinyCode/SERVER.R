library(shiny)
library(readr)
library(tidyverse)
strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

shinyServer(function(input, output){
  
  output$scatterplot <- renderPlot({
    
    # Check if both xvar and yvar are selected
    if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
      
      ggplot(strokeDataSet, aes(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]])) + 
        geom_point(aes(color = strokeDataSet[[input$catvar]])) + labs(x = input$xvar, y = input$yvar)
    } else {
      # If one or both variables are not selected, display an empty plot
      plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot")
    }
    
  })
  
  #Page 3 regression 
  # Dynamically generate input fields for each selected predictor
  output$dynamic_inputs <- renderUI({
    lapply(input$predictors, function(predictor) {
      if (predictor == "Gender") {
        selectInput(
          inputId = paste0("input_", predictor),
          label = paste("Select", predictor, ":"),
          choices = c("male", "female")
        )
      }
      
      else {
        numericInput(
          inputId = paste0("input_", predictor),
          label = paste("Enter value for", predictor, ":"),
          value = 0
        )
      }
    })
  })
  
  observeEvent(input$predict, {
    # Prepare data for prediction
    new_data <- data.frame(matrix(ncol = length(input$predictors), nrow = 1))
    colnames(new_data) <- input$predictors
    
    for (predictor in input$predictors) {
      new_data[1, predictor] <- input[[paste0("input_", predictor)]]
    }
    
    if ("Gender" %in% input$predictors) {
      new_data$gender <- factor(new_data$gender, levels = levels(strokeDataSet$gender))
    }
    
    # Fit logistic regression model
    formula <- as.formula(paste("Stroke ~", paste(input$predictors, collapse = " + ")))
    model <- glm(formula, data = strokeDataSet, family = binomial)
    
    # Make prediction
    prediction <- predict(model, newdata = new_data, type = "response")
    
    # Display result
    output$prediction_result <- renderPrint({
      cat("Predicted Probability of Stroke:", round(prediction, 3))
    })
  })
  
})