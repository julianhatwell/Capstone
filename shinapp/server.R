library(shiny)
source("app_startup_backoff_naive.R")

shinyServer(
  function(input, output) {
    pred <- reactive({
      textType <- get(input$textType)
      predText(input$inText, textType)})
    
    output$pred <- renderPrint({pred()})
    })