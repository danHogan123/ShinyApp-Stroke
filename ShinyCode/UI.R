
library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")


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
      tabPanel("Introduction",
               h1("Variable Selection"),
               fluidRow(
                 column(
                   width = 6, 
                   sidebarLayout(
                     sidebarPanel(
                       width = 7,
                       div(class = "sidebar-panel",
                           checkboxGroupInput(
                             inputId = "predictors",
                             label = "Select Predictors:",
                             choices = c(colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id", "BMI"))], "Height & Weight"),
                             # Exclude the target variable
                             selected = colnames(strokeDataSet)[!(colnames(strokeDataSet) %in% c("Stroke", "id", "BMI"))[1]] 
                             # Select the first predictor by default
                           ),
                           actionButton("predict", "Predict")
                       )
                     ),
                     mainPanel( width = 5,
                                uiOutput("dynamic_inputs"), #verbatimTextOutput("prediction_result"), 
                     ),
                   ),
                   
                 ),
                 column( width = 6,
                         p("This is a function to create a large amount of text
            in an almost paragraph style fashion. This is currently
            a test to see how this would look like within the app
            once I reload it. It should be able to adapt to the 
            pane width and height, making it dynamic in size. 
            This side of the tab should be pretty filled, just
            like right now. We can change the size of the text
            within the CSS file to make sure this text completely
            fills the tab.")
                 )
               ),
               
      ),
      
      
      tabPanel("Variable Analysis",
               h1("Variable Analysis"),
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
      tabPanel("Analysis",
               sidebarLayout(
                 sidebarPanel(
                   h3("Model Analysis")
                 ),
                 mainPanel(
                   h3("Coefficient Analysis"),
                   htmlOutput("analysis"),
                   h3("Model Coefficients"),
                   htmlOutput("importance_text"),
                   plotOutput("importance_plot"),
                   h3("Correlation Matrix"),
                   plotOutput("correlation_plot")
                 )
               )
      )
    )
  )
)

