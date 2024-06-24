library(shiny)
library(readr)
library(tidyverse)
library(scales)

# Load the stroke model
model1 <- readRDS("StrokeModel.rds")

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

shinyServer(function(input, output){
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      Age = if ("Age" %in% input$predictors) input$Age else NA,
      Gender = if ("Gender" %in% input$predictors) input$Gender else NA,
      Hypertension = if ("Hypertension" %in% input$predictors) input$Hypertension else NA,
      HeartDisease = if ("HeartDisease" %in% input$predictors) input$HeartDisease else NA,
      AvgGlucoseLevel = if ("AvgGlucoseLevel" %in% input$predictors) input$AvgGlucoseLevel else NA,
      BMI = if ("BMI" %in% input$predictors) (input$Weight / ((input$Height / 100) ^ 2)) else NA,
      EverMarried = if ("EverMarried" %in% input$predictors) input$EverMarried else NA,
      WorkType = if ("WorkType" %in% input$predictors) input$WorkType else NA,
      ResidenceType = if ("ResidenceType" %in% input$predictors) input$ResidenceType else NA,
      SmokingStatus = if ("SmokingStatus" %in% input$predictors) input$SmokingStatus else NA
    )
    
    # Ensure all required columns are present in new_data
    all_columns <- c("Age", "Gender", "Hypertension", "HeartDisease", "AvgGlucoseLevel", "BMI", "EverMarried", "WorkType", "ResidenceType", "SmokingStatus")
    for (col in all_columns) {
      if (!col %in% colnames(new_data)) {
        new_data[[col]] <- NA
      }
    }
    
    # Remove columns not selected by the user
    new_data <- new_data[, colSums(is.na(new_data)) == 0, drop = FALSE]
    
    # Check for empty data frame
    if (nrow(new_data) == 0 || ncol(new_data) == 0) {
      output$prediction <- renderText({
        "Please select at least one variable and provide its value."
      })
      return()
    }
    
    # Ensure factor levels are consistent with training data
    levels_gender <- c("Male", "Female")
    levels_hypertension <- c("0", "1")
    levels_heartdisease <- c("0", "1")
    levels_evermarried <- c("No", "Yes")
    levels_worktype <- c("Private", "Self-employed", "Govt_job")
    levels_residencetype <- c("Urban", "Rural")
    levels_smokingstatus <- c("Formerly smoked", "Never smoked", "Smokes")
    
    new_data$Gender <- factor(new_data$Gender, levels = levels_gender)
    new_data$Hypertension <- factor(new_data$Hypertension, levels = levels_hypertension)
    new_data$HeartDisease <- factor(new_data$HeartDisease, levels = levels_heartdisease)
    new_data$EverMarried <- factor(new_data$EverMarried, levels = levels_evermarried)
    new_data$WorkType <- factor(new_data$WorkType, levels = levels_worktype)
    new_data$ResidenceType <- factor(new_data$ResidenceType, levels = levels_residencetype)
    new_data$SmokingStatus <- factor(new_data$SmokingStatus, levels = levels_smokingstatus)
    
    # Categorize BMI and GlucoseLevel
    if ("BMI" %in% colnames(new_data)) {
      new_data$BMI_Category <- case_when(
        new_data$BMI < 18.5 ~ "Underweight",
        new_data$BMI < 25 ~ "Healthy Weight",
        new_data$BMI < 30 ~ "Overweight",
        new_data$BMI < 35 ~ "Obese",
        TRUE ~ "Severely Obese"
      )
      new_data <- new_data %>% select(-BMI)
    }
    if ("AvgGlucoseLevel" %in% colnames(new_data)) {
      new_data$Glucose_Category <- case_when(
        new_data$AvgGlucoseLevel < 70 ~ "Very Low",
        new_data$AvgGlucoseLevel < 100 ~ "Low",
        new_data$AvgGlucoseLevel < 126 ~ "Healthy",
        new_data$AvgGlucoseLevel < 200 ~ "High",
        TRUE ~ "Very High"
      )
      new_data <- new_data %>% select(-AvgGlucoseLevel)
    }
    
    # Predict using the stroke model
    prediction <- predict(model1, new_data, type = "prob")
    
    # Output the prediction
    output$prediction <- renderText({
      if (nrow(prediction) > 0) {
        paste("Probability of having a stroke:", round(prediction$.pred_1[1], 4))
      } else {
        "Prediction could not be made. Please check your inputs."
      }
    })
    
    # Calculate and display model coefficients
    coefs <- model1 %>% 
      tidy() %>%
      filter(grepl(paste0("^(", paste0(colnames(new_data), collapse="|"), ")"), term) & term != "(Intercept)")
    
    output$importance_text <- renderUI({
      HTML(paste("Model Coefficients:<br>",
                 paste(coefs$term, round(coefs$estimate, 4), sep = ": ", collapse = "<br>")))
    })
    
    output$importance_plot <- renderPlot({
      ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
        geom_point() +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Effect Sizes of Predictors in Logistic Regression Model",
             x = "Coefficient Estimate",
             y = "Predictors") +
        theme_minimal()
    })
    
    # Generate sentences for each coefficient
    coefficient_sentences <- coefs %>%
      mutate(
        color = ifelse(estimate > 0, "red", "green"),
        sentence = paste0("<span style='color:", color, "'>", term, ": ", round(estimate, 4), "</span>")
      ) %>%
      pull(sentence) %>%
      paste(collapse = "<br>")
    
    output$analysis <- renderUI({
      HTML(paste("Analysis of Coefficients:<br>", coefficient_sentences))
    })
    
    # Calculate and display correlation matrix
    if (length(input$predictors) > 1) {
      # Convert all factor and character columns to numeric via one-hot encoding
      StrokeData_numeric <- new_data %>%
        mutate(across(where(is.character), as.factor)) %>%  # Ensure all character columns are factors
        mutate(across(where(is.factor), as.numeric)) %>%   # Convert factors to numeric if ordinal; otherwise use dummy variables
        # For non-ordinal factors, create dummy variables
        mutate(across(where(is.factor), ~ as.numeric(.x == levels(.x)[2]))) %>%
        # Optionally, drop original non-numeric columns if they were not transformed in-line
        select(where(is.numeric))
      
      # Compute the correlation matrix using complete cases to handle any NAs
      correlation_matrix <- cor(StrokeData_numeric, use = "complete.obs")
      
      # Print the correlation matrix
      output$correlation_plot <- renderPlot({
        corrplot(correlation_matrix, method = "color")
      })
    } else {
      output$correlation_plot <- renderPlot({
        plot.new()
        text(0.5, 0.5, "Not enough variables selected for correlation matrix")
      })
    }
  })

  
  
  
  
  
})