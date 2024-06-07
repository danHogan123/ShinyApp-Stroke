library(shiny)
library(rio)
#test

strokeData <- import("healthcare-dataset-stroke-data.csv")

ui <- fluidPage("This is course of statistical computing with R")
server <- function(input, output) {}
shinyApp(ui = ui, server = server)