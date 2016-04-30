library(shiny)
library(tm)
library(dplyr)
load("tdm_v4.Rdata")

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
      firstWord <- word$firstWord
      assign('predWord', word, envir=.GlobalEnv)
      buttons <- list()
      for (i in 1:preds) {
        buttons <- list(buttons, list(
          actionButton(inputId = paste0("word", i), label=word$words[i])
        ))
      }
      tagList(buttons)
    })
    
    observeEvent(input$word1, {
      updateTextInput(session, "inText"
                      , value = paste(input$inText
                                      , get('predWord', " "
                                            , envir=.GlobalEnv)$words[1]))
    })
    observeEvent(input$word2, {
      updateTextInput(session, "inText"
                      , value = paste(input$inText
                                      , get('predWord', " "
                                            , envir=.GlobalEnv)$words[2]))
    })
    observeEvent(input$word3, {
      updateTextInput(session, "inText"
                      , value = paste(input$inText
                                      , get('predWord', " "
                                            , envir=.GlobalEnv)$words[3]))
    })
  }
)

  
  
  