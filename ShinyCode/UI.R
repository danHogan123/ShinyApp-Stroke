
library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)
library(shinythemes)


StrokeData <- read_csv("healthcare-dataset-stroke-data.csv")

shinyUI(
  
  navbarPage(
    theme = shinytheme("cosmo"),
    title = "Stroke Data Shiny App",
    
    tabPanel("Descriptive Statistics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("statVariable", "Select Variable:", choices = names(StrokeData))
               ),
               mainPanel(
                 verbatimTextOutput("summaryStats")
               )
             )
    ),
    # tabPanel("Visualization",
    #          sidebarLayout(
    #            sidebarPanel(
    #              # Option 1: Histogram
    #              checkboxGroupInput("visualizationOptions", "Select Visualization:",
    #                                 choices = c("Histogram", "Model Coefficients")),
    #              conditionalPanel(
    #                condition = "input.visualizationOptions.includes('Histogram')",
    #                selectInput("histogramVariable", "Select Variable for Histogram:",
    #                            choices = setdiff(names(StrokeData), "Id"))
    #              ),
    #              # Option 2: Model Coefficients
    #              conditionalPanel(
    #                condition = "input.visualizationOptions.includes('Model Coefficients')",
    #                checkboxInput("showCoefficients", "Show Model Coefficients", value = FALSE)
    #              )
    #            ),
    #            mainPanel(
    #              plotOutput("plot"),
    #              plotOutput("coefficientsPlot")
    #            )
    #          )
    # ),
    
    tabPanel("Visualization",
              sidebarLayout(
                sidebarPanel(
                  checkboxInput("showBarChart", "Show Bar Chart", value = FALSE),
                  conditionalPanel(
                    condition = "input.showBarChart == true",
                    selectInput("barChartVariable", "Select Variable for Bar Chart:", choices = names(StrokeData))
                  ),
                  checkboxInput("showCoefficients", "Show Model Coefficients", value = FALSE)
                ),
                mainPanel(
                  plotOutput("plot"),
                  conditionalPanel(
                    condition = "input.showCoefficients == true",
                    plotOutput("coefficientsPlot")
                 )
               ),
              )
     ),
    
    tabPanel("Regression",
             h1("Test your probability of getting a stroke"),
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("useAge", "Include Age", value = FALSE),
                 conditionalPanel(
                   condition = "input.useAge == true",
                   sliderInput("ageInput", "Select an Age:", min = 1, max = 100, value = 50)
                 ),
                 checkboxInput("useBMI", "Include BMI", value = FALSE),
                 conditionalPanel(
                   condition = "input.useBMI == true",
                   numericInput("heightInput", "Enter Height (Inches):", value = 25, min = 10, max = 50, step = 0.1),
                   numericInput("weightInput", "Enter Weight (Pounds):", value = 70, min = 30, max = 200, step = 0.1)
                 ),
                 checkboxInput("useSex", "Include Sex", value = FALSE),
                 conditionalPanel(
                   condition = "input.useSex == true",
                   radioButtons("sexInput", "Select sex:", choices = c("Male", "Female", "Other"))
                 ),
                 checkboxInput("useHyper", "Include Hypertension", value = FALSE),
                 conditionalPanel(
                   condition = "input.useHyper == true",
                   radioButtons("hyperInput", "Do you have hypertension?:", choices = c("Yes" = "1", "No" = "0"))
                 ),
                 checkboxInput("useHeart", "Include Heart Disease", value = FALSE),
                 conditionalPanel(
                   condition = "input.useHeart == true",
                   radioButtons("heartInput", "Do you have heart Disease?:", choices = c("Yes" = "1", "No" = "0"))
                 ),
                 checkboxInput("useMar", "Include Marriage", value = FALSE),
                 conditionalPanel(
                   condition = "input.useMar == true",
                   radioButtons("marInput", "Were you ever married?:", choices = c("Yes", "No"))
                 ),
                 checkboxInput("useWork", "Include Work Type", value = FALSE),
                 conditionalPanel(
                   condition = "input.useWork == true",
                   radioButtons("workInput", "What is your work type?:", choices = c("Govt_job", "Self-employed", "Private", "Never_worked"))
                 ),
                 checkboxInput("useRes", "Include Residence", value = FALSE),
                 conditionalPanel(
                   condition = "input.useRes == true",
                   radioButtons("resInput", "Where do you live?:", choices = c("Rural", "Urban"))
                 ),
                 checkboxInput("useSmoke", "Include Smoking history", value = FALSE),
                 conditionalPanel(
                   condition = "input.useSmoke == true",
                   radioButtons("smokeInput", "What is your smoking history?:", choices = c("Formerly smoked", "Never smoked", "Smokes", "Unknown"))
                 ),
                 checkboxInput("useGluc", "Include Glucose Level", value = FALSE),
                 conditionalPanel(
                   condition = "input.useGluc == true",
                   radioButtons("glucInput", "What is your glucose level?:", choices = c("Very High", "Healthy", "High", "Low", "Very Low"))
                 ),
               ),
               mainPanel(
                 actionButton("runRegression", "Run Regression"),
                 actionButton("saveResult", "Save Result"),
                 actionButton("clearTable", "Clear Results"),
                 verbatimTextOutput("regressionResults"),
                 dataTableOutput("savedResultsTable")
               )
             )
    )
  )
)



          
