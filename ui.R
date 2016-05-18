


ui <- fluidPage(

  tags$head(tags$style(
  "
      
      .sidebar{height:1000px;background-color: #FAFAFA;
               font-family: 'Arial Narrow', Arial, sans-serif;}
      .grey{background-color: #FAFAFA; margin:10px;height:300px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;font-size:small}

      .grey2{background-color: #FAFAFA; font-size:x-small;}

      .red{background-color: #F6CECE;padding:10px;margin:10px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;}
      .darkgrey{background-color: #A4A4A4;padding:10px;margin:5px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;}

      .small_table{font-size:x-small}

      .small_data_table{font-size:xx-small}
  "
  )),
  

           navbarPage(
             "",
             tabPanel(
               "Upload & Define",icon = icon("th"),
               br(),br(),br(),

               fluidRow(

                 column(6,class="grey",
                        h3("#1 Upload and Set Criteria"),
                        fluidRow(
                          column(7,
                                 helpText("Please await when loading and processing."),
                                 fileInput('file1', '',
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv'),width = "1200px"),
                                 textOutput("info"),
                                 br(),
                                 actionButton("update","OK & Reset",class="btn btn-primary btn-sm")#,icon=icon("fa fa-check"))
                                 
                                 
                          ),
                          column(5,
                                 numericInput(inputId="maxp",label="Cut-off P value for ATTRs",value=0.1,min=0.01,max=1,step=0.01),
                                 numericInput(inputId="maxlevel",label="Max number of Levels for ATTRs",value=15,min=3,max=50,step=1) 
                                 
                          )
                        ),
                        br(),
                        p(tags$b("↓"),align = "center")
                  ),
                 
                 
                 column(5,class="grey",
                        h3("#3"),
                        selectInput("head_or_drive",label="Analyze Attributes by:",choices = c("HEAD","DRIVE"),selected = "DRIVE"),  
#                        numericInput(inputId="num_clust_A",label="Number of clusters Attr",value=5,min=2,max=50,step=1),
#                        numericInput(inputId="num_clust_P",label="Number of clusters Para",value=5,min=2,max=50,step=1),
                        actionButton("go","Ready & Analyze!",class="btn btn-primary btn-sm"),
                        helpText("Find results in tabs 'Attributes', 'Parametrics' and 'Interaction'."),
                

br(),br(),br(),br(),br(),
                        p(tags$b("↑"),align = "center")
                        
                 )
               ),
               
               
   
               

               hr(),
               h3("#2 Define Attributes"),
               #p(tags$b("→"),align = "center"),
               
               #verbatimTextOutput("L"),
               
               fluidRow(class="grey2",
                 column(4,class="grey2",
                        h4("Attributes Not Used"),
                        tableOutput("selected_attr_names_2")
                 ),

                 column(5,
                        br(),
                        h6("Add  unused attributes to → the right box for analysis"),

                        uiOutput("CHOOSER")
                        ),
                 column(3,class="grey2",
                        h4("Attributes Used"),
                        tableOutput("selected_attr_names")
                 )


               ),
helpText("Attributes with code 401 are not eligible for computing p-value and may be excluded for Drive-level attribute analysis. Check corresponding raw data for details."),
              

              br(),br(),br(),br(),              br(),br(),br(),br()

             ),
             tabPanel(
               "DataView",icon = icon("file"),
               br(),br(),br(),br(),
               div(class="small_data_table",
               dataTableOutput("rawdata")
               )
             ),
             navbarMenu(
               "Attributes",icon = icon("bar-chart"),
               tabPanel("1-way BarPlots & Clustering",
                        br(),br(),br(),
                        h4("1-way BarPlots & Clustering"),
                        hr(),
                        textOutput("barplot_text"),
                        br(),
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
                        hr(),
                        tags$b("Hierarchical Clustering Dendrogram"),
                        uiOutput("ATTR_CLUST_CHART")
                        ),
               tabPanel("Custom Barplot",
                        br(),br(),br(),
                        
                        h4("Custom BarPlot"),
                        hr(),
                        textOutput("twoway_text"),
                        br(),
                        p("Custom 2-way Barplot (Attributes Interaction)"),
                        
                        fluidRow(
                          column(2,
                                 uiOutput("factor1"),
                                 uiOutput("factor2"),
                                 numericInput("twoway_width","2-way barplot Width",value = 900,step = 10),
                                 numericInput("twoway_height","2-way barplot Height",value = 400,step=10),
                                 hr(),
                                 br(),br(),br(),br(),br(),br(),br(),
                                 p("Other Attributes Barplots"),
                                 #p("Custom 1-way Barplot"),

                          
                                 uiOutput("factor3"),
                                 numericInput("bar_width","1-way barplot Width",value = 900,step = 10),
                                 
                                 numericInput("bar_height","1-way barplot Height",value = 400,step=10)
                                 ),
                          column(10,
                                 
                                 uiOutput("two_way"),
                                 br(),
                                 h4("Summary"),
                                 
                                 uiOutput("text_rank")
                                 )
                          )
                        

          
                        

                        )
             ),
             navbarMenu(
               "Parametrics",icon = icon("line-chart"),
               #tabPanel("Ranked BoxPlots"),
               tabPanel("Summary & Clustering",
                        br(),br(),br(),
                        h4("Parametrics (Top 18) Summary & Clustering (Head Level)"),
                        hr(),
                        uiOutput("para"),
                        helpText("This Tab is prone to occasional crash due to limited system charting capability. Refresh in case of errors.")
                        
                        ),
               tabPanel("Custom ScatterPlot",
                        br(),br(),br(),
                        h4("Parametrics Ranking (P-value) & ScatterPlot  (Head Level)"),
                        hr(),
                        fluidRow(
                          column(3,
                                 div(class="small_table",
                                 tableOutput("ranked_para_names")
                                 )
                                 ),
                          column(9,
                                 uiOutput("parametrics")
                                 )

                        )
               )
             ),
            navbarMenu("Interaction",icon = icon("random"),
                       tabPanel("Interaction Summary",
                                br(),br(),br(),
                                #textOutput("L"),
                                h4("Attribute X Parametric Interaction (Head Level)"),
                                hr(),
                                fluidRow(
                                  column(1,
                                         actionButton("interaction","Run",class="btn btn-primary btn-sm")
                                         ),
                                  column(4,
                                         checkboxInput("use_attr_list","Only Use pre-defined Attributes",value = TRUE)
                                         )
                                ),
                                textOutput("interaction_text"),
                                
                                uiOutput("boxplots_para")                       
                       ),
                       tabPanel(
                         "Custom Boxplot",
                         br(),br(),br(),
                         h4("Custom Boxplot (Head Level)"),
                         hr(),
                         fluidRow(
                           column(3,
                                  uiOutput("BOX_ATTR"),
                                  uiOutput("BOX_PARA"),
                                  numericInput("box_width","Chart Width",value = 900,step = 10),
                                  numericInput("box_height","Chart Height",value = 450,step=10)
                           ),
                           column(9,
                                  uiOutput("CUSTOM_BOXPLOT")
                                  
                                  )
                           ),
                         
                         div(class = "small_table",
                             tableOutput("Interaction_DF")
                         )


                         
                       )

            ),

             windowTitle = "deepdive",
             position = "fixed-top"
           ),

  hr(),
div(class="small_table",
  helpText("ning.h.he@seagate.com"),
  helpText("jiayi.l.lu@seagate.com")
)
)

