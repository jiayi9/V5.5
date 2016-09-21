



library(rpivotTable)
output$pivot = renderRpivotTable({
  
  
  withProgress(message = 'Constructing pivot table', {
    
    if(input$pivot_level == 0) {
      D = data.frame(
                     DRIVE_SERIAL_NUM = DSN(),
                     STATUS = Y(),
                     ATTR(),
                     PARA()
                     )
    } else if(input$pivot_level == 1){
      D = data.frame(
                    DRIVE_SERIAL_NUM = DSN_drive(),
                    STATUS = Y_drive(),
                    ATTR_drive()
        )
      
    } else {
      D = data.frame(
        DRIVE_SERIAL_NUM = DSN(),
        STATUS = Y(),
        upload()
      )      
    }
    
    D = D[,sort(names(D))]
    D = data.frame(D[,c("DRIVE_SERIAL_NUM","STATUS")],
                   D[,setdiff(names(D),c("DRIVE_SERIAL_NUM","STATUS"))]
                  )
    
    rpivotTable(D)
  })
  
})