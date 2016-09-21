  upload = reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    ext = tools::file_ext(inFile)[1]
    print(ext)
    if(ext =="csv"){
      R = read.csv(inFile$datapath, header=TRUE, sep=",",stringsAsFactors=FALSE)
    } else if( ext == "rda"){
      R = readRDS(inFile$datapath)
    } else {
      stop("Upoaded file format unsupported")
    }
    R
  })

  output$info = renderText({
    req(upload())
    LEVELS = getLevels(upload())
    n = sum(LEVELS==1)
    
    m = sum(sapply(upload(),function(x) all(is.na(x))))    
    
    paste("The file contains",nrow(upload()),"rows",ncol(upload()),"cols(",
          n,"all-identical-value cols,",
          m,"all-missing-value cols)."
          
    )
  })
  
  output$upload_log = renderPrint({
    req(upload())
    print_summary(upload())    
  })
  
  output$EXCLUDE = renderUI({
    req(upload())
    X = upload()
    selectInput("exclude","Pre-Exclude",choices = c("",sort(names(X))), multiple = TRUE ,selected ="")
  })
  
  
  output$TARGET_UI = renderUI({
    req(upload())
    
    NAMES = setdiff(names(upload()),input$exclude)
     
    default_target = 
    if("STATUS" %in% NAMES) {"STATUS"} else if
    ("EFA_RESULT" %in% NAMES) {"EFA_RESULT"} else if
    ("EFA" %in% NAMES) {"EFA_RESULT"} else
    {NAMES[1]}
     
    selectInput("target","TARGET",choices = sort(NAMES),selected = default_target)
   
  })
  
  output$TARGET_UI_log = renderPrint({
    input$target
  })
  
  TARGET = reactive({
    req(upload())
    req(input$target)
      upload()[,input$target]
    
  })
  
  output$TARGET_log = renderPrint({
    req(TARGET())
    #try({table(TARGET())},silent = TRUE)
    table(TARGET())
  })
  
  
  output$TARGET_DEFINE_UI = renderUI({
      
    req(input$target)
      
      
      NAMES = unique(TARGET())
      default =  character()
      
      if("F" %in% NAMES ) {default = c(default,"F")}
      if("Fail" %in% NAMES ) {default = c(default,"Fail")}
      if("FAIL" %in% NAMES ) {default = c(default,"FAIL")}
      if("Failure" %in% NAMES ) {default = c(default,"Failure")}
      if("FAILURE" %in% NAMES ) {default = c(default,"FAILURE")}
      if(input$define_non_empty_as_fail){
        default =  setdiff(NAMES,c("",NA))
      }
      selectInput("target_define","Define Failures",choices = unique(TARGET()),multiple = TRUE,selected = default)

  })
  
  output$define_non_empty_as_fail_log = renderPrint({
    input$define_non_empty_as_fail
  })
    
  output$AGG_UI = renderUI({
    req(input$target)
    NAMES = setdiff(names(upload()),input$exclude)
    
      default =  
      if("DRIVE_SERIAL_NUM" %in% NAMES ) {"DRIVE_SERIAL_NUM"} else if
      ("Drive_Serial_Num" %in% NAMES ) {"Drive_Serial_Num"} else if
      ("drive_serial_num" %in% NAMES ) {"drive_serial_num"} else if

      ("DRIVE.SERIAL.NUM" %in% NAMES ) {"DRIVE.SERIAL.NUM"} else if
      ("Drive.Serial.Num" %in% NAMES ) {"Drive.Serial.Num"} else if
      ("drive.serial.num" %in% NAMES ) {"drive.serial.num"} else if
    
      ("SERIAL_NUM" %in% NAMES ) {"SERIAL_NUM"} else if
      ("serial_num" %in% NAMES ) {"serial_num"} else if
      ("SN" %in% NAMES ) {"SN"}
      
      if(!input$no_agg){
        selectInput("agg","Define Aggregation Column",choices = NAMES,selected = default)      
      }
      

    
  })
  
  output$agg_log = renderPrint({input$agg})
  
  output$HEAD_OR_DRIVE = renderUI({
    if(!input$no_agg){
      selectInput("head_or_drive","Analyze Attributes by",choices = c("DRIVE","HEAD"),selected = "DRIVE")
    } else {
      selectInput("head_or_drive","Analyze Attributes by",choices = c("DRIVE","HEAD"),selected = "HEAD")
      
    }

  })
  
  output$head_or_drive_log = renderPrint({input$head_or_drive})