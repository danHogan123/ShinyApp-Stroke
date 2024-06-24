library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)
library(shinythemes)


shinyUI(
  #NAVBAR IS USED OVER FLUID PAGE BECAUSE IT ALLOWS THE CREATION OF MUTIPLE PAGES
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
                 actionButton("runRegression", "Run Regression"),
                 actionButton("saveResult", "Save Result")
               ),
               mainPanel(
                 verbatimTextOutput("regressionResults"),
                 h3("Saved Results"),
                 tableOutput("savedResultsTable")
               )
             )
    )
  )
)


  
