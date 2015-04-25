library(shiny)

shinyUI(
  fluidPage(
    includeCSS("style.css"),
    
    mainPanel(align="center",
        h2("Text Prediction"),
        h4("This app will predict the next word in your sentence"),
        textInput('text',label=h5("Type below to see how it works")),
        #br(),
        #submitButton('Submit'),
        h4("The prediction is..."),
        h2(textOutput('text'))
    )
  )
)  