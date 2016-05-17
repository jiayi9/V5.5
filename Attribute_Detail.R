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
  selectInput("factor_3", "Factor:",  choices = names(ATTR_many_levels_drive()),selected = names(ATTR_many_levels_drive())[1])
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
#  print(names(ATTR_many_levels()))
#  print(names(ATTR_many_levels_drive()))
#   print(1)
  if(input$head_or_drive =="DRIVE"){
    X = ATTR_many_levels_drive()
    y = STATUS_drive()
#    labelx = labelx_drive()
  } else{
    X = ATTR_many_levels()
    y = STATUS()
#    labelx = labelx()
  }
#   print(2)
  if(is.null(input$factor_3)) {
    xname = names(X)[1]
  } else {
    xname = input$factor_3
  }
#   print(3)
  x = as.character(X[,xname])
#   print(4)
#   print(length(x))
#   print(x)
#   print(length(y))
#   print(y)
  
  p = barplot_1_custom(x,
                y,
                xname = xname,
                chisq_test(x,y,floor(nrow(X)*0.05)),
                L=NULL,
                
                NUM_CLUST_A()
                
  )
  print(5)
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



observe({
  if(input$head_or_drive =="DRIVE"){
    X = ATTR_many_levels_drive()
    y = STATUS_drive()
  } else{
    X = ATTR_many_levels()
    y = STATUS()
  }
  n = ncol(X)
  n = min(n, 20)
  
  validate(need(n>0,"No enough qualified long attributes"))
  
  output$text_rank = renderUI({
    text_rank_output_list = lapply(1:n,function(i){
      
      name = paste("line",i,sep="")
      tags$div(class = "group-output",
               textOutput(name)
      )
    })
    do.call(tagList,text_rank_output_list)
    
  })
  
  for(j in 1:n){
    local({
      my_i = j
      
      name = paste("line",my_i,sep="")
      
      output[[name]] = renderText({
        x = as.character(X[,my_i])
        
        ListFails = function(x,y){
          D = data.frame(x,y)
          ddply(D,.(x), function(df) sum(df$y=="F"))$V1
        }
        
        QUANT = ListFails(x,y)
        LEN_1 = length(QUANT)
        LEN_2 = min(LEN_1,50)
        QUANT_2 = QUANT[1:LEN_2]
        
        c(names(X)[my_i],rep("-",25-nchar(names(X)[my_i])),QUANT_2)
      })
    })
  }
  
})


