library(shiny)
library(readr)
library(tidyverse)
strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")

shinyServer(function(input, output){
  
  output$scatterplot <- renderPlot({
    
    # Check if both xvar and yvar are selected
    if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
      # Plot the scatterplot
      # plot(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]],
      #      xlab = input$xvar, ylab = input$yvar,
      #      main = paste("Scatterplot of", input$xvar, "vs", input$yvar)
      #      
      # )+ geom_point(aes(color = .data[[input$catvar]]))
      
      ggplot(strokeDataSet, aes(strokeDataSet[[input$xvar]], strokeDataSet[[input$yvar]])) + 
        geom_point(aes(color = strokeDataSet[[input$catvar]])) + labs(x = input$xvar, y = input$yvar)
    } else {
      # If one or both variables are not selected, display an empty plot
      plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot")
    }
    
  })
  
})