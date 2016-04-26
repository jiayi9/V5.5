# Plugin
#attr_ranked_p_re()
#STATUS()
#labelx()

#attr_ranked_p_re_drive()
#STATUS_drive()
#labelx_drive()

#input$head_or_drive

#input$go


output$twoway_text = renderText({
  n = ncol(attr_ranked_p_re_drive())
  m = ceiling(n/3)
  paste0("There are ",n," attributes and ",m," clusters analyzed by ",input$head_or_drive,".")
})


output$factor1 <- renderUI({
  X = attr_ranked_p_re_drive()
  selectInput("factor_1", "Factor 1:",  choices = names(X),selected = names(X)[1])
}) 

output$factor2 <- renderUI({
  selectInput("factor_2", "Factor 2:",  choices = names(attr_ranked_p_re_drive()),selected = names(attr_ranked_p_re_drive())[2])
}) 

output$factor3 <- renderUI({
  selectInput("factor_3", "Factor:",  choices = names(attr_ranked_p_re_drive()),selected = names(attr_ranked_p_re_drive())[1])
}) 

output$interaction = renderPlot({
  withProgress(message = 'Making two way barchart', {
    
    
    if(input$head_or_drive =="DRIVE"){
      X = attr_ranked_p_re_drive()
      y = STATUS_drive()
    } else{
      X = attr_ranked_p_re()
      y = STATUS()
    }
    
    
    if(is.null(input$factor_1) & is.null(input$factor_2)){
      x1 = X[,1]
      x2 = X[,2]
      FAIL = y
      xname1 = names(X)[1]
      xname2 = names(X)[2]
    } else{
      x1 = X[,input$factor_1]
      x2 = X[,input$factor_2]
      FAIL = y
      xname1 = input$factor_1
      xname2 = input$factor_2
    }
    barplot_2(x1,x2,FAIL,xname1,xname2)
  })
})

output$custom_barplot = renderPlot({
  
  if(input$head_or_drive =="DRIVE"){
    X = attr_ranked_p_re_drive()
    y = STATUS_drive()
    labelx = labelx_drive()
  } else{
    X = attr_ranked_p_re()
    y = STATUS()
    labelx = labelx()
  }
  
  if(is.null(input$factor_3)) {
    xname = names(X)[1]
  } else {
    xname = input$factor_3
  }
  
  x = X[,xname]
  p = barplot_1(x,
                y,
                xname = xname,
                chisq_test(x,y,floor(nrow(X)*0.05)),
                labelx,
                
                NUM_CLUST_A()
                
  )
  grid::grid.newpage()
  grid::grid.draw(p)
})


output$two_way = renderUI({

    barheight = paste0(input$bar_height,"px")
    barwidth = paste0(input$bar_width,"px")
    
    twowaywidth = paste0(input$twoway_width,"px")
    twowayheight = paste0(input$twoway_height,"px")
    
  
  
  tags$div(class = "group-output",
           plotOutput("interaction",
                      height = twowayheight,
                      width = twowaywidth
                      ),
           hr(),
  
          plotOutput("custom_barplot",
                      height = barheight,
                      width = barwidth
                      )
             
          )
  
})




