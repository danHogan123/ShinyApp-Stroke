library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

#navbarPage creates mutiple pages
shinyUI(
  
  navbarPage(
    theme = shinytheme("darkly"), #Add your theme name here
    title = "Stroke Prediction Shiny App",
    
    # Defining each tab panel creates a new page
    tabPanel("Testing page 1",
      h1("This is a header"),
      p("This is a paragraph")
    ),


    tabPanel("Variable Analysis",
      h1("hi"),
      sidebarLayout(
        sidebarPanel(
          
          # Id, title, and panel with choices using data set
          selectInput("yvar", "Y Variable:", 
            c("Select Y Variable", colnames(select(strokeDataSet, -id, - Gender)))),
          selectInput("xvar", "X Variable:", 
            c("Select X Variable", colnames(select(strokeDataSet, -id, - Gender)))),
          selectInput("catvar", "Categorial Variable:",
            c("Select a Variable", colnames(select(strokeDataSet, -Age, -BMI, -id))))
        ),
        mainPanel(
          # creates scatterplot object
          plotOutput("scatterplot")
        )
      )
    ),
    tabPanel("Regression testing",
      h1("testing regression prediction for stroke"),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = "predictors",
            label = "Select Predictors:",
            choices = colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id"))],  # Exclude the target variable
            selected = colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id"))[1]]  # Select the first predictor by default
            ),
            uiOutput("dynamic_inputs"),
            actionButton("predict", "Predict")
          ),
        mainPanel(
          verbatimTextOutput("prediction_result")
          )
          
        )
      )

  )
)

  
