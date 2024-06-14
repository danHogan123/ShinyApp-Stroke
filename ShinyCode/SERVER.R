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
        + ylim(input, max)
    } else {
      # If one or both variables are not selected, display an empty plot
      plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot")
    }
    
  })
<<<<<<< Updated upstream
=======
  output$dynamic_inputs <- renderUI({
    predictor_inputs <- lapply(input$predictors, function(pred) {
      if (pred == "Gender") {
        selectInput(
          inputId = "input_Gender",
          label = "Gender:",
          choices = c("Male", "Female"),
          selected = "Male"
        )
      } else {
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
  
  
  
>>>>>>> Stashed changes
  
})