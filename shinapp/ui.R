library(shiny)
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Next word prediction"),
    
    sidebarPanel(
      radioButtons("textType", "Choose text type:",
                   c("General" = "all",
                     "Formal" = "txt",
                     "Tweet" = "twit")),
      textInput('inText', label = 'Enter Text:'),
      h5("Lazy Buttons"),
      uiOutput("button"),
      h3("One Top Prediction:"),
      h3(textOutput('topPred'))
      ),

    mainPanel(
      plotOutput('wordPlot')
      )
  )
)