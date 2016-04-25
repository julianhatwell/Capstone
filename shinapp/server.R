library(shiny)
library(tm)
library(dplyr)
load("tdm.Rdata")

# tuning parameters
lambdas <- c(0.6, 0.29, 0.99995, 0.00005)
rare <- 1

source("app_startup_interpolation.R")

shinyServer(
  function(input, output) {
    pred <- reactive({
      textType <- get(input$textType)
      predText(input$inText, textType)
      })
    
    output$pred <- renderPrint({pred()})

    output$button <- renderUI({
      word <- pred()
      assign('word', word, envir=.GlobalEnv)
      button <- actionButton(inputId = "word", label=word)
      tagList(button)
    })
    
  }
)

  
  
  