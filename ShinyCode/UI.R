library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")
summar <- summary(StrokeData)
print(summar)

shinyUI(
  fluidPage(
  
  tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
),

  #navbarPage creates mutiple pages
  navbarPage(
    theme = shinytheme("darkly"), #Add your theme name here
    title = "Stroke Prediction Shiny App",
    
    # Defining each tab panel creates a new page
    # tabPanel("Introduction",
    #   h1("Variable Selection"),
    #   fluidRow(
    #     column(
    #       width = 6, 
    #       sidebarLayout(
    #         sidebarPanel(
    #           width = 7,
    #           div(class = "sidebar-panel",
    #           checkboxGroupInput(
    #             inputId = "predictors",
    #             label = "Select Predictors:",
    #             choices = c(colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id", "BMI"))], "Height & Weight"),
    #             # Exclude the target variable
    #             selected = colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id", "BMI"))[1]] 
    #             # Select the first predictor by default
    #             ),
    #           actionButton("predict", "Predict")
    #           )
    #         ),
    #         mainPanel( width = 5,
    #                   uiOutput("dynamic_inputs"), #verbatimTextOutput("prediction_result"), 
    #         ),
    #       ),
    #       
    #     ),
    #     column( width = 6,
    #       p("This is a function to create a large amount of text
    #         in an almost paragraph style fashion. This is currently
    #         a test to see how this would look like within the app
    #         once I reload it. It should be able to adapt to the 
    #         pane width and height, making it dynamic in size. 
    #         This side of the tab should be pretty filled, just
    #         like right now. We can change the size of the text
    #         within the CSS file to make sure this text completely
    #         fills the tab.")
    #     )
    #   ),
    #   
    # ),


    # tabPanel("Variable Analysis",
    #   h1("Variable Analysis"),
    #   sidebarLayout(
    #     sidebarPanel(
    #       # Id, title, and panel with choices using data set
    #       selectInput("yvar", "Y Variable:", 
    #         c("Select Y Variable", colnames(select(strokeDataSet, -id, - Gender)))),
    #       selectInput("xvar", "X Variable:", 
    #         c("Select X Variable", colnames(select(strokeDataSet, -id, - Gender)))),
    #       selectInput("catvar", "Categorial Variable:",
    #         c("Select a Variable", colnames(select(strokeDataSet, -Age, -BMI, -id))))
    #     ),
    #     mainPanel(
    #       # creates scatterplot object
    #       plotOutput("scatterplot")
    #     )
    #   )
    # ),
    tabPanel("Regression testing",
      h1("testing regression prediction for stroke"),
      sidebarLayout(
        sidebarPanel(
          checkboxInput("useAge", "Include Age", value = TRUE),
          conditionalPanel(
            condition = "input.useAge == true",
            sliderInput("ageInput", 
                        "Select an Age:", 
                        min = 1, 
                        max = 100, 
                        value = 50)
          ),
          checkboxInput("useBMI", "Include BMI", value = TRUE),
          conditionalPanel(
            condition = "input.useBMI == true",
            numericInput("heightInput", 
                         "Enter Height:", 
                         value = 25, 
                         min = 10, 
                         max = 50, 
                         step = 0.1),
            numericInput("weightInput", 
                         "Enter Weight:", 
                         value = 70, 
                         min = 30, 
                         max = 200, 
                         step = 0.1),
          ),
          checkboxInput("useSex", "Include sex", value = TRUE),
          conditionalPanel(
            condition = "input.useSex == true",
            # Additional UI elements to include when useSex checkbox is checked
            radioButtons("sexInput", "Select sex:",
                         choices = c("Male", "Female", "Other")),
            # You can add more inputs here based on your requirements
          ),
          
          
          checkboxInput("useHyper", "Include Hypertension", value = TRUE),
          conditionalPanel(
            condition = "input.useHyper == true",
            # Additional UI elements to include when useSex checkbox is checked
            radioButtons("hyperInput", "Do you have hypertension?:",
                         choices = c("yes", "no", "Other")),
            # You can add more inputs here based on your requirements
          ),
          actionButton("runRegression", "Run Regression")
          
        ),
        mainPanel(verbatimTextOutput("regressionResults"))
      )
    )
  )
  )
)

  
