ui <- fluidPage(
  theme = "style.css",
  div(class="small_text",
    
    navbarPage("",
               tabPanel("Upload & Define",icon = icon("upload"),
                        br(),br(),br(),
                        
                        
                        fluidRow(
                          column(7,class = "grey",
                                 


                                 h3("#1 Upload and Define"),
                                 
                                 fluidRow(
                                   column(5,

                                          fileInput('file1', '',
                                                    accept=c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv')),
                                          textOutput("info"),
                                          uiOutput("EXCLUDE"),
                                          uiOutput("TARGET_UI"),
                                          uiOutput("TARGET_DEFINE_UI"),
                                          checkboxInput("define_non_empty_as_fail",label = "Mark all non-empty values as Fails",value = FALSE)
                                          ,downloadButton('downloadAGG', 'Download aggregated Attrs',class="btn btn-default btn-xs")
                                          
                         
                                          
                                          ),
                                   column(5,
                                          br(),

                                          uiOutput("AGG_UI"),
                                          uiOutput("HEAD_OR_DRIVE"),
                                          checkboxInput("no_agg",label = "No aggregation column",value = FALSE),
                                          numericInput(inputId="max_p",label="Cut-off P value for ATTRs",value=0.1,min=0.01,max=1,step=0.01),
                                          numericInput(inputId="max_level",label="Max number of Levels for ATTRs",value=15,min=3,max=50,step=1),
                                          helpText("Refreshing page for new uploading."),
                                          
                                          fluidRow(column(7,""),column(5,
                                                                       #br(),
                                                                       actionButton("update","Process Data",class="btn btn-primary btn-sm")#icon=icon("fa fa-check")),
                                                                       
                                                                       ))
                                          
                                          

                                          
                                          ),
                                   column(1,
                                          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                          

                                          
                                         )
                                   )
                                 #narrow fluid row ends up
                                 ),
                          column(4,class = "grey",
                                 h3("#3 Analyze"),
                                 
                                 checkboxInput("show_advanced_settings","Show advanced settings",value = FALSE),
                                 fluidRow(
                                   column(6,
                                          conditionalPanel(condition = "input.show_advanced_settings ==1",
                                                           numericInput(inputId = "chisq_tol",label="Chisq Tolerance level",value=0.05,min=0.01,max=0.99,step=0.01),
                                                           numericInput("para_missing_percent","Ignore Parametrics with missing proportion >",
                                                                        value = 0.2,min = 0.01,max = 0.5,step = 0.01),
                                                           
                                                           numericInput("para_unique","Ignore Parametrics with unique values <",
                                                                        value = 20,min = 10, max = 100,step = 1),
                                                           numericInput("attr_many_levels_cutoff","Attributes with many levels Cut-off value",
                                                                        value = 30,min=20,max=100,step=1)
                                          )
                                          
                                          ),
                                   column(6,
                                          conditionalPanel(condition = "input.show_advanced_settings ==1",
                                                           numericInput("max_p_para","Cut-off P value for PARAs",
                                                                        value = 0.05,min = 0.01,max = 1, step =0.01),
                                                           numericInput("attr_clust_h",
                                                                        "Attributes clustering sensitivity value",
                                                                        value = 0.5,
                                                                        min = 0.01,max=1,step=0.01),
                                                           numericInput("para_clust_h",
                                                                        "Parametrics clustering sensitivity value",
                                                                        value = 0.5,
                                                                        min = 0.01,max=1,step=0.01),

                                                           selectInput("treatNA","How to treat Attributes Missing values",
                                                                       choices = c("Exclude in analysis","Treat as a group"))
                                            )
                                          )
                                   ),

                                 
                                 
                                 actionButton("analyze","GO!",class="btn btn-primary btn-sm")
                                 ,helpText("Find results in other tabs at the top")
                                 
                                 )
                          
                          ),
                        #wide fluid row ends up
                        h3("#2 Define Attributes"),
                        
                        fluidRow(class="grey2",
                                 
                                 column(4,class="grey2",
                                        h4("Attributes Not Used"),
                                        tableOutput("left_table"),
                                        helpText("Code 401 means chisq test cannot be performed")
                                 ),
                                 
                                 column(5,
                                        br(),
                                        br(),br(),
                                        p(""),

                                        

                                        
                                        uiOutput("CHOOSER")
                                 ),
                                 column(3,class="grey2",
                                        h4("Attributes Used"),
                                        tableOutput("right_table")
                                 )
                                 
                                 
                        )


                        #h4("upload log"),
                        #verbatimTextOutput("upload_log"),
                        #h4("exclude"),

                        #h4("TARGET_UI"),

                        #h4("TARGET_UI_log"),
                        #verbatimTextOutput("TARGET_UI_log"),
                        #h4("TARGET_log"),
                        
                        #verbatimTextOutput("TARGET_log"),
                        

                        
                        
                        #verbatimTextOutput("define_non_empty_as_fail_log"),
                        
                        #h4("agg_log"),
                        
                        #verbatimTextOutput("agg_log"),
                        #verbatimTextOutput("head_or_drive_log"),
                        
                        
               )
               #tabPanel upload&filter ends up
               
               ,tabPanel("Explore",icon = icon("cube"),
                         p(""),
                         br(),br(),br(),
                         radioButtons("pivot_level","Construct the Pivot table for:",inline = TRUE,
                                      choices = c("Head-level attributes and parametrcis data"=0,
                                                  "Drive-level attributes data"=1,
                                                  "Raw uploaded data" = 2)
                                      
                         ),
                         rpivotTableOutput("pivot")
               )
               
               
               ,navbarMenu("Attributes",icon = icon("pie-chart"),
                           tabPanel("Attributes Summary",
                                    br(),br(),br(),
                                    h4("Attributes Summary: 1-way BarPlots & Clustering"),
                                    textOutput("barplot_text"),
                                    uiOutput("hist_1"),
                                    uiOutput("hist_2_x"),
                                    fluidRow(
                                      column(2,
                                             numericInput("onewaybarheight","Chart Height",value = 450,step=10,width = 100)
                                             
                                             ),
                                      column(2,
                                             br(),
                                             checkboxInput("showbarcolor","Bar Color",value = TRUE)
                                             
                                             )
                                      ),
                                    uiOutput("ATTR_CLUST_CHART")
                                    
                                    
                                    ),
                           tabPanel("Custom Plots",
                                    br(),br(),br(),
                                    h4("2-way barplot"),
                                    fluidRow(
                                      column(2,
                                             uiOutput("factor1"),
                                             uiOutput("factor2"),
                                             uiOutput("two_way_size")
                                             ),
                                      column(10,
                                             uiOutput("TWO_WAY_BARPLOT")
                                             
                                             )
                                      ),

                                    hr(),
                                    
                                    
                                    h4("Attributes with many levels"),
                                    fluidRow(
                                      column(4,
                                             uiOutput("factor3")

                                             
                                      ),
                                      column(2,
                                             br(),
                                             checkboxInput("attr_many_levels_rank","Rank by Scores",value = FALSE)
                                             
                                             
                                      ),
                                      column(6,
                                             uiOutput("custom_barplot_size")
                                             
                                             
                                      )

                                      
                                      
                                    ),
                                    uiOutput("CUSTOM_BARPLOT"),
                                    h4("Summary"),
                                    uiOutput("text_rank")
                                    #dataTableOutput("attr_many_levels_summary")

#                                     p("ATTR_many_levels"),
#                                     verbatimTextOutput("ATTR_many_levels_log"),
#                                     p("ATTR_many_levels_drive"),
#                                     verbatimTextOutput("ATTR_many_levels_drive_log"),
#                                     p("attr_many_levels"),
#                                     verbatimTextOutput("attr_many_levels_log"),
#                                     plotOutput("two_way_barplot"),


                                    
                                    
                                    )
                           ),
#navMenu attributes ends up

                          tabPanel("Parametrics",icon = icon("area-chart"),
                                   br(),br(),br(),
                                   h4("Parametrics Summary"),
                                   p("Parametrics with pvalues < 0.05 are analyzed by default."),
                                   uiOutput("para"),
                                   hr(),
                                   h4("Rank & Scatterplot"),
                                   fluidRow(
                                     column(3,
                                            div(class="small_table",
                                                tableOutput("ranked_para_names_table")
                                                #dataTableOutput("ranked_para_names")
                                            )
                                     ),
                                     column(9,
                                            uiOutput("parametrics")
                                     )
                                     
                                   )

                                   
                                   
                                  ),
# tabPanel Parametrics ends up
                
                        navbarMenu("Interactions",icon = icon("random"),
                                   tabPanel("Interaction Analysis Summary",
                                            br(),br(),br(),br(),
                                            actionButton("interaction","Search for all Interactions",,class="btn btn-primary btn-sm"),
                                            checkboxInput("use_attr_list","Only Use pre-defined Attributes",value = TRUE),

                                            checkboxInput("inter_log","Log Y Scale",value = FALSE),
                                            textOutput("interaction_text"),
                                            uiOutput("boxplots_para")
                                            
                                            ),
                                   tabPanel("Attribute-Dominant Analysis",
                                            br(),br(),br(),
                                            
                                            
                                            
                                            fluidRow(
                                              column(3,uiOutput("dominant_ATTR")),
                                              column(3,uiOutput("second_PARA")),
                                              column(2,checkboxInput("rank_second_para","Rank by Score",value = TRUE)
                                                     ,
                                                     checkboxInput("dominant_ATTR_log","Log Scale",value = FALSE),
                                                     checkboxInput("use_attr_list_1","Only Use pre-defined Attributes",value = FALSE)
                                                     
                                                     ),
                                              column(2,numericInput("dominant_attr_width","Chart Width",value = 600,step = 10)),
                                              column(2,numericInput("dominant_attr_height","Chart Height",value = 450,step=10))
                                            ),
                                            fluidRow(
                                              column(6,uiOutput("dominant_attr_BOXPLOT"))
                                              ,
                                              column(6,plotOutput("dominant_attr_cdf"))
                                            )
                                            ,
                                            
                                            hr(),
                                            h4("Top Parametrics for the dominant Attribute (P value <0.05)"),
                                            actionButton("dominant_attr_go","Show"),
                                            fluidRow(
                                              column(6,
                                                     uiOutput("dominant_attr_boxplots")
                                              ),
                                              column(6,
                                                     uiOutput("dominant_attr_CDF")
                                              )
                                              
                                            )
                                            

                                            ),
                                   tabPanel("Parametric-Dominant Analysis",
                                            br(),br(),br(),
                                            fluidRow(
                                              column(3,uiOutput("dominant_PARA")),
                                              column(3,uiOutput("second_ATTR")),
                                              column(2,
                                                     checkboxInput("rank_second_attr","Rank by Score",value = TRUE)
                                                     ,
                                                     checkboxInput("dominant_PARA_log","Log Scale",value = FALSE),
                                                     checkboxInput("use_attr_list_2","Only Use pre-defined Attributes",value = FALSE)
                                                     
                                                     ),
                                              column(2,numericInput("dominant_para_width","Chart Width",value = 600,step = 10)),
                                              column(2,numericInput("dominant_para_height","Chart Height",value = 450,step=10))
                                            ),
                                            fluidRow(
                                              column(6,uiOutput("dominant_para_BOXPLOT"))
                                              ,
                                              column(6,plotOutput("dominant_para_cdf"))
                                              )
                                            ,
                                            hr(),
                                            h4("Top Attributes for the dominant Parametric (P value <0.05)"),
                                            actionButton("dominant_para_go","Show"),
                                            
                                            fluidRow(
                                              column(6,
                                                     uiOutput("dominant_para_boxplots")
                                              ),
                                              column(6,
                                                     uiOutput("dominant_para_CDF")
                                              )
                                              
                                            )
                                            
                                            #tableOutput("Interaction_DF")
                                            
                                            )                                   
                                   )
#navpage interaction ends up
                              ,tabPanel("Summary",icon = icon("table"),
                                        br(),br(),br(),
                                        div(class="small_table",
                                            fluidRow(
                                              column(4,
                                                        h4("Raw data summary"),
                                                        tableOutput("UPLOAD_summary")
                                                     
                                                     ),
                                              column(4,
                                                     div(class = "grey_small",
                                                         
                                                     h4("Attributes summary"),
                                                     tableOutput("ATTR_summary")
                                                     )
                                                     ),
                                              column(4,
                                                     h4("Parametrics summary"),
                                                     tableOutput("PARA_summary")
                                                     )
                                              )
                                        ))

#                               ,tabPanel("Pivot",icon = icon("cube"),
#                                         p(""),
#                                         br(),br(),br(),
#                                         radioButtons("pivot_level","Construct the Pivot table for:",inline = TRUE,
#                                                      choices = c("Head level all data (raw data)"=0,
#                                                                  "Drive level attributes data"=1)
#                                                      
#                                                      ),
#                                         rpivotTableOutput("pivot")
#                                         )

#DataView ends up
                              ,tabPanel("Report",icon = icon("dedent"),
                                        br(),br(),br(),
                                        
                                        h3("View all reports before you download!")      
                                        ,
                                        hr(),
                                        actionButton("reportSelectAll","Select / Unselect All",class="btn btn-primary btn-sm"),
                                        br(),br(),
                                        checkboxGroupInput("reportList","Report:",
                                                           choices = 
                                                             c(
                                                             "Attributes summary",
                                                             "Attributes clustering",
                                                             "Two way barplot",
                                                             "Attributes with many levels",
                                                             "Parametrics Summary",
                                                             "Parametrics clustering",
                                                             "Custom Parametric Plot",
                                                             "Interaction Summary",
                                                             "Attribute-Dominant Custom plots",
                                                             "Attribute-Dominant Rank",
                                                             "Parametric-Dominant Custom plots",
                                                             "Parametric-Dominant Rank"
                                                             ),
                                                           selected = ""
#                                                              c(
#                                                              "Attributes summary",
#                                                              "Attributes clustering",
#                                                              
#                                                              "Two way barplot",
#                                                              "Attributes with many levels",
#                                                              "Parametrics Summary",
#                                                              "Parametrics clustering",
#                                                              "Custom Parametric Plot",
#                                                              "Attribute Interaction Boxplot",
#                                                              "Parametric Interaction Boxplot"
#                                                            )
                                                           ),
                                        radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word')),#,inline=TRUE),
                                        br(),
                                        downloadButton("download","Download Report",class="btn btn-primary btn-sm")
                                        
                                        
                                        )
                          


#   
#  checkboxInput("disp_missing_as_group","Missing as a group",value = TRUE),
               
     ,windowTitle = "deepdive", position = "fixed-top"   #navPage settings          
    )
    #navpage ends up
  ),
  #div ends up
  
  
  
  
#   h4("RAW:"),
#   verbatimTextOutput("RAW_log"),
#   
#   h4("ATTR:"),
#   verbatimTextOutput("ATTR_log"),
#   h4("Y"),
#   verbatimTextOutput("Y_log"),
#   h4("DSN"),
#   verbatimTextOutput("DSN_log"),
#   h4("ATTR_drive log"),
#   verbatimTextOutput("ATTR_drive_log"),
#   h4("ATTR_drive head"),
  #tableOutput("ATTR_drive_table") 

  
#   
#   h4("DSN_drive_log"),
#   verbatimTextOutput("DSN_drive_log"),
#   h4("Y_drive_log"),
#   verbatimTextOutput("Y_drive_log"),
#   
#   
#   h4("chisq table head"),
#   verbatimTextOutput("chisq_table_head_log"),
#   
#   
#   h4("chisq table drive"),
#   verbatimTextOutput("chisq_table_drive_log"),
#   
#   h4("chisq_table"),
#   verbatimTextOutput("chisq_table_log"),
#   
#   h4("attr_names_log"),
#   verbatimTextOutput("attr_names_log"),
  
#   p("SELECTED NAMES"),
#   verbatimTextOutput("SELECTED_NAMES_log"),
#   p("UNSELECTED NAMES"),
#   verbatimTextOutput("UNSELECTED_NAMES_log"),
  

#   p("attr_y_dsn_log"),
#   verbatimTextOutput("attr_y_dsn_log"),
#   p("attr_ranked_log"),
#   verbatimTextOutput("attr_ranked_log"),
  #plotOutput("attr_clust_chart"),

  
  

  
#   p("PARA_log"),
#   verbatimTextOutput("PARA_log"),
#   p("para_log"),
#   verbatimTextOutput("para_log"),
#   p("para_p_table"),
#   verbatimTextOutput("para_p_table_log"),
#   
#   p("scatterData_log"),
#   verbatimTextOutput("scatterData_log"),
#   p("scatter_color_log"),
#   verbatimTextOutput("scatter_color_log"),



  
  p(""),
br(),br(),br(),
  div(class="small_table",
  helpText("ning.h.he@seagate.com"),
  
  helpText("jiayi.l.lu@seagate.com")
  )
)
#fluidPage