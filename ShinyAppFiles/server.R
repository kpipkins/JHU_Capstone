library(shiny)
source('Functions.r')
source('Prediction.r')
LoadPackages()
bi.freq <- read.csv(textConnection("bi_freq.csv"))
tri.freq <- read.csv(textConnection("tri_freq.csv"))
quad.freq <- read.csv(textConnection("quad_freq.csv"))

shinyServer(function(input,output){
  prediction <- reactive({
    predict(input$text)
  })
  output$text <- renderText({
    prediction()
  })
})