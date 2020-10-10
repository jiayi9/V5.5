library(shiny)


shinyUI(fluidPage(

  h4("Example"),
  
  hr(),
  
  h1('This is an example'),
  
  fluidRow(
    column(6, h1('Left')),
    column(6, h1('Right'))
    
  ),

  selectInput('gender', 'Gender', choices = c('Male', 'Female'), selected = 'Male'),
  
  textOutput('TEXT'),
  
  radioButtons('asdasd','asdasd', choices = LETTERS)
))
