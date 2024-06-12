library(shiny)
library(shinythemes)

strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

#navbarPage creates mutiple pages
shinyUI(
  
  navbarPage(
    theme = shinytheme("darkly"), #Add your theme name here
    title = "Stroke Prediction Shiny App",
    
    # Defining each tab panel creates a new page
    tabPanel("Variable Analysis",
      h1("This is a header"),
      p("This is a paragraph")
    ),


    tabPanel("Testing page 2",
      h1("hi"),
      sidebarLayout(
        sidebarPanel(
          
          # Id, title, and panel with choices using data set
          selectInput("xvar", "X Variable:", choices = c("Select X Variable", colnames(strokeDataSet))),
          selectInput("yvar", "Y Variable:", choices = c("Select y Variable", colnames(strokeDataSet)))
        ),
        mainPanel(
          # creates scatterplot object
          plotOutput("scatterplot")
        )
      )
    )
  )
)

  
