library(shiny)

shinyServer(function(input, output, session) {
  
  output$TEXT = renderText({
    paste('I am', input$gender)
  })
  
  
  
})
