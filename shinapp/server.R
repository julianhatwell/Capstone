library(shiny)
library(tm)
library(dplyr)
load("tdm_cond.Rdata")

source("app_startup_gtd.R")
source("app_startup_plot.R")

shinyServer(
  function(input, output, session) {
    
    # number of predictions to return
    preds <- 3
    
    pred <- reactive({
      textType <- get(input$textType)
      predText(input$inText, textType, preds)
      })
    
    output$topPred <- renderText({pred()$words[1]})
    
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
    output$wordPlot <- renderPlot({
        wordProbPlot(pred()$probs, preds)
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

  
  
  