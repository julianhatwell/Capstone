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
      uiOutput("button")
      ),

    mainPanel(
      h3('Suggested next words'),
      verbatimTextOutput("pred")
    )
  )
)