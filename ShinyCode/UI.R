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
            c("Select a Variable", colnames(select(strokeDataSet, -Age, -BMI))))
        ),
        mainPanel(
          # creates scatterplot object
          plotOutput("scatterplot")
        )
      )
    )
  )
)

  
