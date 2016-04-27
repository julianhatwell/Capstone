library(shiny)
library(tm)
library(dplyr)
load("tdm.Rdata")

# tuning parameters
lambdas <- c(0.6, 0.29, 0.99995, 0.00005)
rare <- v + 1

# number of predictions to return
preds <- 3

source("app_startup_interpolation.R")

shinyServer(
  function(input, output, session) {
    pred <- reactive({
      textType <- get(input$textType)
      predText(input$inText, textType)
      })
    
    output$pred <- renderPrint({pred()})

    output$button <- renderUI({
      word <- pred()
      assign('predWord', word, envir=.GlobalEnv)
      button <- actionButton(inputId = "word1", label=word)
      tagList(button)
    })
    
    observeEvent(input$word1, {
      updateTextInput(session, "inText"
                      , value = paste(input$inText
                                      , get('predWord'
                                            , envir=.GlobalEnv)))
    })
  }
)

  
  
  