library(shiny)
library(rio)
#test
#tester

strokeData <- import("healthcare-dataset-stroke-data.csv")
strokeData

ui <- fluidPage("This is course of statistical computing with R")
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

# terror lurks in the depths of the fog