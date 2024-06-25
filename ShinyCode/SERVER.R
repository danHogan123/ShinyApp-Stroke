
library(shiny)
library(readr)
library(tidyverse)
library(scales)
library(DT)
library(ggplot2)
library(plotly)
StrokeData

server <- function(input, output) {
  
  # Function to categorize BMI
  calculatedBMI <- reactive({
    if (input$useBMI) {
      return(((703 * input$weightInput) / (input$heightInput ^ 2)))
    }
    return(NULL)
  })
  
  categorizeBMI <- function(bmi) {
    if (is.na(bmi)) return(NA)
    if (bmi < 18.5) return("Underweight")
    if (bmi >= 18.5 & bmi < 24.9) return("Normal weight")
    if (bmi >= 25 & bmi < 29.9) return("Overweight")
    if (bmi >= 30 & bmi < 35) return("Obese")
    if (bmi >= 35) return("Severely obese")
  }
  
  categorizedBMI <- reactive({
    bmi <- calculatedBMI()
    return(categorizeBMI(bmi))
  })
  
  # Reactive values for storing results
  regressionResult <- reactiveVal(NULL)
  savedResults <- reactiveVal(list())
  
  # Event to run regression
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
    
    # Constructing the formula for the model
    formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
    print(formula)
    
    # Adjusting data types and preparing new data for prediction
    newdata <- data.frame(
      Age = if (input$useAge) as.numeric(input$ageInput) else NA,
      BMI_Category = if (input$useBMI) as.character(categorizedBMI()) else NA,
      Gender = if (input$useSex) as.character(input$sexInput) else NA,
      Hypertension = if (input$useHyper) as.numeric(input$hyperInput) else NA,
      HeartDisease = if (input$useHeart) as.numeric(input$heartInput) else NA,
      EverMarried = if (input$useMar) as.character(input$marInput) else NA,
      WorkType = if (input$useWork) as.character(input$workInput) else NA,
      ResidenceType = if (input$useRes) as.character(input$resInput) else NA,
      SmokingStatus = if (input$useSmoke) as.character(input$smokeInput) else NA,
      Glucose_Category = if (input$useGluc) as.character(input$glucInput) else NA
    )
    
    # Running the logistic regression model
    tryCatch({
      FinalModel <- glm(formula, data = StrokeData, family = binomial)
      
      # Predicting stroke probability
      StrokePrediction <- predict(FinalModel, newdata = newdata, type = "response")
      
      # Saving results
      result <- list(model_summary = summary(FinalModel), prediction = StrokePrediction, predictors = predictors)
      regressionResult(result)
      savedResults(c(savedResults(), list(result)))
    }, error = function(e) {
      regressionResult(paste("Error in model fitting:", e$message))
    })
    
  })
  
  # Rendering summary statistics
  output$summaryStats <- renderPrint({
    summary(StrokeData[[input$statVariable]])
  })
  
  # Rendering visualizations
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
  
  # Rendering regression results
  output$regressionResults <- renderPrint({
    result <- regressionResult()
    if (is.null(result)) {
      "Run the regression to see the results."
    } else if (is.character(result)) {
      result
    } else {
      cat("Stroke prediction number:", round(result$prediction, 2), "\n")
      cat("Predictors used:", paste(result$predictors, collapse = ", "), "\n")
      print(result$model_summary)
    }
  })
  
  # Rendering saved results table
  output$savedResultsTable <- renderTable({
    results <- savedResults()
    if (length(results) == 0) {
      return(NULL)
    } else {
      data.frame(
        Prediction = sapply(results, function(x) round(x$prediction, 2)),
        Predictors = sapply(results, function(x) paste(x$predictors, collapse = ", "))
      )
    }
  })
  
  # Observing event to save results
  observeEvent(input$saveResult, {
    result <- regressionResult()
    if (!is.null(result) && !is.character(result)) {
      savedResults(c(savedResults(), list(result)))
    }
  })
  
  # Observing event to clear table
  observeEvent(input$clearTable, {
    savedResults(list())
  })
}

# Running the Shiny app









                
                
                
                
                
                
          #       observeEvent(input$predict, {
          #         new_data <- data.frame(
          #           Age = if ("Age" %in% input$predictors) input$Age else NA,
          #           Gender = if ("Gender" %in% input$predictors) input$Gender else NA,
          #           Hypertension = if ("Hypertension" %in% input$predictors) input$Hypertension else NA,
          #           HeartDisease = if ("HeartDisease" %in% input$predictors) input$HeartDisease else NA,
          #           AvgGlucoseLevel = if ("AvgGlucoseLevel" %in% input$predictors) input$AvgGlucoseLevel else NA,
          #           BMI = if ("BMI" %in% input$predictors) (input$Weight / ((input$Height / 100) ^ 2)) else NA,
          #           EverMarried = if ("EverMarried" %in% input$predictors) input$EverMarried else NA,
          #           WorkType = if ("WorkType" %in% input$predictors) input$WorkType else NA,
          #           ResidenceType = if ("ResidenceType" %in% input$predictors) input$ResidenceType else NA,
          #           SmokingStatus = if ("SmokingStatus" %in% input$predictors) input$SmokingStatus else NA
          #         )
          #         
          #         # Ensure all required columns are present in new_data
          #         all_columns <- c("Age", "Gender", "Hypertension", "HeartDisease", "AvgGlucoseLevel", "BMI", "EverMarried", "WorkType", "ResidenceType", "SmokingStatus")
          #         for (col in all_columns) {
          #           if (!col %in% colnames(new_data)) {
          #             new_data[[col]] <- NA
          #           }
          #         }
          #         
          #         data <- do.call(rbind, lapply(results, function(result) {
          #           data.frame(
          #             Prediction = round(result$prediction, 2),
          #             Predictors = paste(result$predictors, collapse = ", ")
          #             # Remove columns not selected by the user
          #             new_data <- new_data[, colSums(is.na(new_data)) == 0, drop = FALSE]
          #             
          #             # Check for empty data frame
          #             if (nrow(new_data) == 0 || ncol(new_data) == 0) {
          #               output$prediction <- renderText({
          #                 "Please select at least one variable and provide its value."
          #               })
          #               return()
          #             }
          #             
          #             # Ensure factor levels are consistent with training data
          #             levels_gender <- c("Male", "Female")
          #             levels_hypertension <- c("0", "1")
          #             levels_heartdisease <- c("0", "1")
          #             levels_evermarried <- c("No", "Yes")
          #             levels_worktype <- c("Private", "Self-employed", "Govt_job")
          #             levels_residencetype <- c("Urban", "Rural")
          #             levels_smokingstatus <- c("Formerly smoked", "Never smoked", "Smokes")
          #             
          #             new_data$Gender <- factor(new_data$Gender, levels = levels_gender)
          #             new_data$Hypertension <- factor(new_data$Hypertension, levels = levels_hypertension)
          #             new_data$HeartDisease <- factor(new_data$HeartDisease, levels = levels_heartdisease)
          #             new_data$EverMarried <- factor(new_data$EverMarried, levels = levels_evermarried)
          #             new_data$WorkType <- factor(new_data$WorkType, levels = levels_worktype)
          #             new_data$ResidenceType <- factor(new_data$ResidenceType, levels = levels_residencetype)
          #             new_data$SmokingStatus <- factor(new_data$SmokingStatus, levels = levels_smokingstatus)
          #             
          #             # Categorize BMI and GlucoseLevel
          #             if ("BMI" %in% colnames(new_data)) {
          #               new_data$BMI_Category <- case_when(
          #                 new_data$BMI < 18.5 ~ "Underweight",
          #                 new_data$BMI < 25 ~ "Healthy Weight",
          #                 new_data$BMI < 30 ~ "Overweight",
          #                 new_data$BMI < 35 ~ "Obese",
          #                 TRUE ~ "Severely Obese"
          #               )
          #             }))
          #           return(data)
          # })
          #         
          #         observeEvent(input$saveResult, {
          #           result <- regressionResult()
          #           if (!is.null(result) && !is.character(result)) {
          #             savedResults(c(savedResults(), list(result)))
          #             new_data <- new_data %>% select(-BMI)
          #           }
          #           if ("AvgGlucoseLevel" %in% colnames(new_data)) {
          #             new_data$Glucose_Category <- case_when(
          #               new_data$AvgGlucoseLevel < 70 ~ "Very Low",
          #               new_data$AvgGlucoseLevel < 100 ~ "Low",
          #               new_data$AvgGlucoseLevel < 126 ~ "Healthy",
          #               new_data$AvgGlucoseLevel < 200 ~ "High",
          #               TRUE ~ "Very High"
          #             )
          #             new_data <- new_data %>% select(-AvgGlucoseLevel)
          #           }
          #           
          #           # Predict using the stroke model
          #           prediction <- predict(model1, new_data, type = "prob")
          #           
          #           # Output the prediction
          #           output$prediction <- renderText({
          #             if (nrow(prediction) > 0) {
          #               paste("Probability of having a stroke:", round(prediction$.pred_1[1], 4))
          #             } else {
          #               "Prediction could not be made. Please check your inputs."
          #             }
          #           })
                    
                    # Calculate and display model coefficients
#                     coefs <- model1 %>% 
#                       tidy() %>%
#                       filter(grepl(paste0("^(", paste0(colnames(new_data), collapse="|"), ")"), term) & term != "(Intercept)")
#                     
#                     output$importance_text <- renderUI({
#                       HTML(paste("Model Coefficients:<br>",
#                                  paste(coefs$term, round(coefs$estimate, 4), sep = ": ", collapse = "<br>")))
#                     })
#                     
#                     output$importance_plot <- renderPlot({
#                       ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
#                         geom_point() +
#                         geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
#                         labs(title = "Effect Sizes of Predictors in Logistic Regression Model",
#                              x = "Coefficient Estimate",
#                              y = "Predictors") +
#                         theme_minimal()
#                     })
#                     
#                     # Generate sentences for each coefficient
#                     coefficient_sentences <- coefs %>%
#                       mutate(
#                         color = ifelse(estimate > 0, "red", "green"),
#                         sentence = paste0("<span style='color:", color, "'>", term, ": ", round(estimate, 4), "</span>")
#                       ) %>%
#                       pull(sentence) %>%
#                       paste(collapse = "<br>")
#                     
#                     output$analysis <- renderUI({
#                       HTML(paste("Analysis of Coefficients:<br>", coefficient_sentences))
#                     })
#                     
#                     # Calculate and display correlation matrix
#                     if (length(input$predictors) > 1) {
#                       # Convert all factor and character columns to numeric via one-hot encoding
#                       StrokeData_numeric <- new_data %>%
#                         mutate(across(where(is.character), as.factor)) %>%  # Ensure all character columns are factors
#                         mutate(across(where(is.factor), as.numeric)) %>%   # Convert factors to numeric if ordinal; otherwise use dummy variables
#                         # For non-ordinal factors, create dummy variables
#                         mutate(across(where(is.factor), ~ as.numeric(.x == levels(.x)[2]))) %>%
#                         # Optionally, drop original non-numeric columns if they were not transformed in-line
#                         select(where(is.numeric))
#                       
#                       # Compute the correlation matrix using complete cases to handle any NAs
#                       correlation_matrix <- cor(StrokeData_numeric, use = "complete.obs")
#                       
#                       # Print the correlation matrix
#                       output$correlation_plot <- renderPlot({
#                         corrplot(correlation_matrix, method = "color")
#                       })
#                     } else {
#                       output$correlation_plot <- renderPlot({
#                         plot.new()
#                         text(0.5, 0.5, "Not enough variables selected for correlation matrix")
#                       })
#                     }
#                   })
#                 }
#           
#           
#           
#           
#           
#           
#           })
# 
# 
# 
# 
# #This is the end of what I copy and pasted
# # Load the stroke model
# #model1 <- readRDS("StrokeModel.R")
# 
# #strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")
# 
# shinyServer(function(input, output){
#   
#   
#   # output$scatterplot <- renderPlot({
#   #   
#   #   if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
#   #     
#   #     ggplot(strokeDataSet, aes(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]])) +
#   #       geom_point(aes(color = strokeDataSet[[input$catvar]])) + labs(x = input$xvar, y = input$yvar)
#   #     #+ scale_y_continuous(breaks = y_breaks)
#   #     #+ ylim(min(strokeDataSet[[input$yvar]],na.rm=T), max(strokeDataSet[[input$yvar]],na.rm=T))
#   #   } else {
#   #     # If one or both variables are not selected, display an empty plot
#   #     return(plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot"))
#   #   }
#   # })
#   # 
#   # output$dynamic_inputs <- renderUI({
#   #   predictor_inputs <- lapply(input$predictors, function(pred) {
#   #     if (pred == "Gender") {
#   #       selectInput(
#   #         inputId = "Gender",
#   #         label = "Gender:",
#   #         choices = c("Male", "Female"),
#   #         selected = "Male"
#   #       )
#   #     }
#   #     else if (pred == "Age"){
#   #       sliderInput(
#   #         inputId = "Age",
#   #         label = "Age",
#   #         18, 90, 30
#   #       )
#   #     }
#   #     else if (pred == "Hypertension"){
#   #       selectInput(
#   #         inputId = "Hypertension",
#   #         label = "Hypertension",
#   #         choices = c("Yes", "No"),
#   #         selected = "Yes"
#   #       )
#   #     }
#   #     else if (pred == "HeartDisease"){
#   #       selectInput(
#   #         inputId = "HeartDisease",
#   #         label = "Heart Disease",
#   #         choices = c("Yes", "No"),
#   #         selected = "Yes"
#   #       )}
#   #     else if (pred == "EverMarried"){
#   #       selectInput(
#   #         inputId = "EverMarried",
#   #         label = "Ever Married",
#   #         choices = c("Yes", "No"),
#   #         selected = "Yes"
#   #       )}
#   #     else if (pred == "WorkType"){
#   #       selectInput(
#   #         inputId = "WorkType",
#   #         label = "Work Type",
#   #         choices = c("Private", "Government", "Self-Employed", "Never Worked"),
#   #         selected = "Private"
#   #       )}
#   #     else if (pred == "ResidenceType"){
#   #       selectInput(
#   #         inputId = "ResidenceType",
#   #         label = "Residence Type",
#   #         choices = c("Urban", "Rural"),
#   #         selected = "Urban"
#   #       )}
#   #     else if (pred == "AvgGlucoseLevel"){
#   #       numericInput( # Subject to change yall
#   #         inputId = "AvgGlucoseLevel",
#   #         label = "Average Glucose Level",
#   #         min = 50, max = 300, value = 140
#   #       )}
#   #     else if (pred == "SmokingStatus"){
#   #       selectInput(
#   #         inputId = "SmokingStatus",
#   #         label = "Smoking Status",
#   #         choices = c("Never smoked", "Smokes", "Formerly smoked", "Unknown"),
#   #         selected = "Never Smoked" 
#   #       )}
#   #     else if (pred == "Height & Weight"){
#   #       list(
#   #         tagList(
#   #           numericInput(
#   #             inputId = "Height",
#   #             label = "Height in inches",
#   #             min = 50, max = 300, value = 140
#   #           ),
#   #         ),
#   #         tagList(
#   #           numericInput(
#   #             inputId = "Weight",
#   #             label = "Weight in lbs",
#   #             min = 50, max = 300, value = 140
#   #           )
#   #         )
#   #       )
#   #     }
#   #     else {
#   #       numericInput(
#   #         inputId = paste0(pred),
#   #         label = pred,
#   #         value = 0
#   #       )
#   #     }
#   #   })
#   #   
#   #   # Split the list of UI elements into two equal parts
#   #   n <- length(predictor_inputs)
#   #   
#   #   if (n == 1) {
#   #     # If there is only one predictor, place it in a single column
#   #     fluidRow(
#   #       column(12, do.call(tagList, predictor_inputs))
#   #     )
#   #   } else {
#   #     
#   #     half <- ceiling(n / 2)
#   #     col1 <- predictor_inputs[1:half]
#   #     col2 <- predictor_inputs[(half + 1):n]
#   #     
#   #     # Arrange the elements in two columns
#   #     fluidRow(
#   #       column(6, do.call(tagList, col1)),
#   #       column(6, do.call(tagList, col2))
#   #     )
#   #   }
#   # })
#   
#   # observeEvent(input$predict, {
#   #   #req(input$predictors)
#   # 
#   #   # Prepare the data for logistic regression
#   #   # formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
#   #   # model <- glm(formula, data = strokeDataSet, family = binomial)
#   #   
#   # 
#   #   # Gather input values for prediction
#   #   new_data <- data.frame(matrix(ncol = length(input$predictors), nrow = 1))
#   #   colnames(new_data) <- input$predictors
#   #   for (pred in input$predictors) {
#   #     new_data[[pred]] <- input[[paste0(pred)]]
#   #   }
#   # 
#   #   # Add Gender to the new_data for prediction
#   #   #new_data$Gender <- input$input_Gender
#   # 
#   #   # Predict stroke probability
#   #   prediction <- predict(model1, new_data, type = "prob")
#   #   #prediction <- predict(model, newdata = new_data, type = "response")
#   # 
#   #   # Display the prediction result
#   #   output$prediction_result <- renderText({
#   #     paste("Predicted probability of stroke:", round(prediction, 4))
#   #   })
#   # })
#   
#   
#   
#   
#   
#   
#   
#   
# 
# 
# 
# 
#   observeEvent(input$predict, {
#     new_data <- data.frame(
#       Age = if ("Age" %in% input$predictors) input$Age else "0",
#       Gender = if ("Gender" %in% input$predictors) input$Gender else "Female",
#       Hypertension = if ("Hypertension" %in% input$predictors) input$Hypertension else "Yes",
#       HeartDisease = if ("HeartDisease" %in% input$predictors) input$HeartDisease else "Yes",
#       AvgGlucoseLevel = if ("AvgGlucoseLevel" %in% input$predictors) input$AvgGlucoseLevel else NA,
#       BMI = if ("Height & Weight" %in% input$predictors) (input$Weight / ((input$Height / 100) ^ 2)) else NA,
#       EverMarried = if ("EverMarried" %in% input$predictors) input$EverMarried else NA,
#       WorkType = if ("WorkType" %in% input$predictors) input$WorkType else NA,
#       ResidenceType = if ("ResidenceType" %in% input$predictors) input$ResidenceType else NA,
#       SmokingStatus = if ("SmokingStatus" %in% input$predictors) input$SmokingStatus else NA
#     )
# 
#     # Ensure all required columns are present in new_data
#     all_columns <- c("Age", "Gender", "Hypertension", "HeartDisease", "AvgGlucoseLevel", "BMI", "EverMarried", "WorkType", "ResidenceType", "SmokingStatus")
#     for (col in all_columns) {
#       if (!col %in% colnames(new_data)) {
#         new_data[[col]] <- NA
#       }
#     }
# 
#     # Remove columns not selected by the user
#     new_data <- new_data[, colSums(is.na(new_data)) == 0, drop = FALSE]
# 
#     # Check for empty data frame
#     if (nrow(new_data) == 0 || ncol(new_data) == 0) {
#       output$prediction <- renderText({
#         "Please select at least one variable and provide its value."
#       })
#       return()
#     }
#     
#     # FORMULA THAT SAVES THE VALUES THE USER CHOSE
#     formula <- as.formula(paste("Stroke ~", paste(predictors, collapse = " + ")))
#     print(formula)
#     
#     # FINAL MODEL IN USE AS RECIPE MODEL IS NOT WORKING ATM
#     FinalModel <- glm(formula, data = StrokeData, family = binomial)
#     
#     # TAKES USERS SAVED VALUES AND STORES THEM IN A DATA FRAME
#     newdata <- data.frame(
#       Age = if (input$useAge) input$ageInput else NA,
#       BMI_Category = if (input$useBMI) categorizedBMI() else NA,
#       Gender = if (input$useSex) input$sexInput else NA,
#       Hypertension = if (input$useHyper) input$hyperInput else NA,
#       HeartDisease = if (input$useHeart) input$heartInput else NA,
#       EverMarried = if (input$useMar) input$marInput else NA,
#       WorkType = if (input$useWork) input$workInput else NA,
#       ResidenceType = if (input$useRes) input$resInput else NA,
#       SmokingStatus = if (input$useSmoke) input$smokeInput else NA,
#       Glucose_Category = if (input$useGluc) input$glucInput else NA
#     )
#     
#     # THE STROKE PREDICTION FOR THE USER
#     StrokePrediction <- predict(FinalModel, newdata = newdata, type = "response")
#     result <- list(model_summary = summary(FinalModel), prediction = StrokePrediction, predictors = predictors)
#     regressionResult(result)
#     # Store the model summary and prediction
#     regressionResult(list(model_summary = summary(FinalModel), prediction = StrokePrediction))
#     savedResults(c(savedResults(), list(result)))
#   })
#   
#   # Output functions (these should be within the server function)
#   
#   output$dataTable <- renderDT({
#     datatable(StrokeData %>% select(input$variables))
#   })
#   
#   # Summary Statistics
#   output$summaryStats <- renderPrint({
#     summary(StrokeData[[input$statVariable]])
#   })
#   
#   # Visualization
#   output$plot <- renderPlot({
#     p <- ggplot(StrokeData, aes_string(x = input$plotVariable))
#     if (input$plotType == "Histogram") {
#       p <- p + geom_histogram()
#     } else if (input$plotType == "Bar Chart") {
#       p <- p + geom_bar()
#     } else if (input$plotType == "Boxplot") {
#       p <- p + geom_boxplot()
#     }
#     print(p)
#   })
#   
#   # # Display the regression results
#   # output$regressionResults <- renderPrint({
#   #   result <- regressionResult()
#   #   if (is.null(result)) {
#   #     "Run the regression to see the results."
#   #   } else if (is.character(result)) {
#   #     result
#   #   } else {
#   #     cat("Stroke prediction number:", round(result$prediction, 2))
#   #   }
#   # })
#   output$regressionResults <- renderPrint({
#     result <- regressionResult()
#     if (is.null(result)) {
#       "Run the regression to see the results."
#     } else if (is.character(result)) {
#       result
#     } else {
#       cat("Stroke prediction number:", round(result$prediction, 2), "\n")
#       cat("Predictors used:", paste(result$predictors, collapse = ", "), "\n")
#     }
#   })
#   
#   output$savedResultsTable <- renderTable({
#     results <- savedResults()
#     if (length(results) == 0) {
#       return(NULL)
#     }
#     
#     data <- do.call(rbind, lapply(results, function(result) {
#       data.frame(
#         Prediction = round(result$prediction, 2),
#         Predictors = paste(result$predictors, collapse = ", ")
#       )
#     }))
#     return(data)
#   })
#   
#   observeEvent(input$saveResult, {
#     result <- regressionResult()
#     if (!is.null(result) && !is.character(result)) {
#       savedResults(c(savedResults(), list(result)))
#     }
#   })
#   observeEvent(input$clearTable, {
#     savedResults(list())
#   })
# }
# 
# 
#     # Ensure factor levels are consistent with training data
#     levels_gender <- c("Male", "Female")
#     levels_hypertension <- c("0", "1")
#     levels_heartdisease <- c("0", "1")
#     levels_evermarried <- c("No", "Yes")
#     levels_worktype <- c("Private", "Self-employed", "Govt_job")
#     levels_residencetype <- c("Urban", "Rural")
#     levels_smokingstatus <- c("formerly smoked", "never smoked", "smokes", "Unknown")
# 
#     new_data$Gender <- factor(new_data$Gender, levels = levels_gender)
#     new_data$Hypertension <- factor(new_data$Hypertension, levels = levels_hypertension)
#     new_data$HeartDisease <- factor(new_data$HeartDisease, levels = levels_heartdisease)
#     new_data$EverMarried <- factor(new_data$EverMarried, levels = levels_evermarried)
#     new_data$WorkType <- factor(new_data$WorkType, levels = levels_worktype)
#     new_data$ResidenceType <- factor(new_data$ResidenceType, levels = levels_residencetype)
#     new_data$SmokingStatus <- factor(new_data$SmokingStatus, levels = levels_smokingstatus)
# 
#     # Categorize BMI and GlucoseLevel
#     if ("BMI" %in% colnames(new_data)) {
#       new_data$BMI_Category <- case_when(
#         new_data$BMI < 18.5 ~ "Underweight",
#         new_data$BMI < 25 ~ "Healthy Weight",
#         new_data$BMI < 30 ~ "Overweight",
#         new_data$BMI < 35 ~ "Obese",
#         TRUE ~ "Severely Obese"
#       )
#       new_data <- new_data %>% select(-BMI)
#     }
#     if ("AvgGlucoseLevel" %in% colnames(new_data)) {
#       new_data$Glucose_Category <- case_when(
#         new_data$AvgGlucoseLevel < 70 ~ "Very Low",
#         new_data$AvgGlucoseLevel < 100 ~ "Low",
#         new_data$AvgGlucoseLevel < 126 ~ "Healthy",
#         new_data$AvgGlucoseLevel < 200 ~ "High",
#         TRUE ~ "Very High"
#       )
#       new_data <- new_data %>% select(-AvgGlucoseLevel)
#     }
# 
#     # Predict using the stroke model
#     prediction <- predict(model1, new_data, type = "prob")
# 
#     # Output the prediction
#     output$prediction <- renderText({
#       if (nrow(prediction) > 0) {
#         paste("Probability of having a stroke:", round(prediction$.pred_1[1], 4))
#       } else {
#         "Prediction could not be made. Please check your inputs."
#       }
#     })
# 
#     # Calculate and display model coefficients
#     coefs <- model1 %>%
#       tidy() %>%
#       filter(grepl(paste0("^(", paste0(colnames(new_data), collapse="|"), ")"), term) & term != "(Intercept)")
# 
#     output$importance_text <- renderUI({
#       HTML(paste("Model Coefficients:<br>",
#                  paste(coefs$term, round(coefs$estimate, 4), sep = ": ", collapse = "<br>")))
#     })
# 
#     output$importance_plot <- renderPlot({
#       ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
#         geom_point() +
#         geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
#         labs(title = "Effect Sizes of Predictors in Logistic Regression Model",
#              x = "Coefficient Estimate",
#              y = "Predictors") +
#         theme_minimal()
#     })
# 
#     # Generate sentences for each coefficient
#     coefficient_sentences <- coefs %>%
#       mutate(
#         color = ifelse(estimate > 0, "red", "green"),
#         sentence = paste0("<span style='color:", color, "'>", term, ": ", round(estimate, 4), "</span>")
#       ) %>%
#       pull(sentence) %>%
#       paste(collapse = "<br>")
# 
#     output$analysis <- renderUI({
#       HTML(paste("Analysis of Coefficients:<br>", coefficient_sentences))
#     })
# 
#     # Calculate and display correlation matrix
#     if (length(input$predictors) > 1) {
#       # Convert all factor and character columns to numeric via one-hot encoding
#       StrokeData_numeric <- new_data %>%
#         mutate(across(where(is.character), as.factor)) %>%  # Ensure all character columns are factors
#         mutate(across(where(is.factor), as.numeric)) %>%   # Convert factors to numeric if ordinal; otherwise use dummy variables
#         # For non-ordinal factors, create dummy variables
#         mutate(across(where(is.factor), ~ as.numeric(.x == levels(.x)[2]))) %>%
#         # Optionally, drop original non-numeric columns if they were not transformed in-line
#         select(where(is.numeric))
# 
#       # Compute the correlation matrix using complete cases to handle any NAs
#     }
#     #   correlation_matrix <- cor(StrokeData_numeric, use = "complete.obs")
#     #
#     #   # Print the correlation matrix
#     #   output$correlation_plot <- renderPlot({
#     #     corrplot(correlation_matrix, method = "color")
#     #   })
#     # } else {
#     #   output$correlation_plot <- renderPlot({
#     #     plot.new()
#     #     text(0.5, 0.5, "Not enough variables selected for correlation matrix")
#     #   })
#     # }
#     })



  
  
  