
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
    
<<<<<<< HEAD
    tabPanel("Visualization",
      sidebarLayout(
        
        sidebarPanel(
          selectInput("plotVariable", "Select Variable:", choices = names(StrokeData)),
            selectInput("plotType", "Select Plot Type:", choices = c("Histogram", "Bar Chart", "Boxplot"))
        ),
        
        mainPanel(
          plotOutput("plot")
        )
      )
    ),
    
    tabPanel("Regression",
             h1("Test your probability of getting a stroke"),
             fluidRow(
               column(12, 
                      div(style = "text-align: center; margin-bottom: 20px;",
                          actionButton("runRegression", "Run Regression"),
                          actionButton("saveResult", "Save Result"),
                          actionButton("clearTable", "Clear Table")
                      )
               )
             ),
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
                   numericInput("heightInput", "Enter Height:", value = 25, min = 10, max = 50, step = 0.1),
                   numericInput("weightInput", "Enter Weight:", value = 70, min = 30, max = 200, step = 0.1)
                 ),
                 checkboxInput("useSex", "Include sex", value = FALSE),
                 conditionalPanel(
                   condition = "input.useSex == true",
                   radioButtons("sexInput", "Select sex:", choices = c("Male", "Female", "Other"))
                 ),
                 checkboxInput("useHyper", "Include Hypertension", value = FALSE),
                 conditionalPanel(
                   condition = "input.useHyper == true",
                   radioButtons("hyperInput", "Do you have hypertension?:", choices = c("yes" = "1", "no" = "0"))
                 ),
                 checkboxInput("useHeart", "Include Heart Disease", value = FALSE),
                 conditionalPanel(
                   condition = "input.useHeart == true",
                   radioButtons("heartInput", "Do you have heart Disease?:", choices = c("yes" = "1", "no" = "0"))
                 ),
                 checkboxInput("useMar", "Include Marriage", value = FALSE),
                 conditionalPanel(
                   condition = "input.useMar == true",
                   radioButtons("marInput", "Were you ever married?:", choices = c("Yes", "No"))
                 ),
                 checkboxInput("useWork", "Include work type", value = FALSE),
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
                   radioButtons("smokeInput", "What is your smoking history?:", choices = c("formerly smoked", "never smoked", "smokes", "unknown"))
                 ),
                 checkboxInput("useGluc", "Include Glucose", value = FALSE),
                 conditionalPanel(
                   condition = "input.useGluc == true",
                   radioButtons("glucInput", "What is your glucose level?:", choices = c("Very High", "Healthy", "High", "Low", "Very Low"))
                 ),
                 #actionButton("runRegression", "Run Regression"),
                 #actionButton("saveResult", "Save Result"),
                 #actionButton("clearTable", "Clear Table")
=======
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
>>>>>>> b4258a94e9e4c14f6d1ad72e05efee383c5c98c3
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
                   h3("Model Analysis"),
                   textOutput("prediction")
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

