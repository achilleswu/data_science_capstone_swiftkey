#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Swiftkey prediction model"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Input text"),
      textInput("textInput", "", "Hey sunshine, can you follow me and make me the"),
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      helpText("Next word, be patient, it's very very slow."),
       textOutput("res"),
      helpText("Word Cloud of top possible words."),
      plotOutput("plotOutput"),
      helpText("Data frame of top possible words"),
      tableOutput("textOutput")
    )
  )
))
