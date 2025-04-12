#################data
# Data Sources
source("data/csv.R")
source('data/table.R')
source('data/summary.R')
source('data/data_cleansing.R')
source('data/scraping.R')
source('data/feature.R')
# tags$style(
#         type="text/css",
#         "#loadmessage {
#         position: absolute;
#         top: 50%;
#         margin-left: 500px;
#         margin-top: -100px;
#         z-index: 105;
#         width:70px;
#         margin-left:150px;
#         }"
# )
dataUI = function(id,label="data"){
  ns <- NS(id)
  tagList(
    sidebarPanel(width = 3,
     csvFileInput(ns("datafile"), "Upload data file")
    ),
    mainPanel(width = 9,
      tabsetPanel(
         # tabPanel(title = "Data Sources",
         #          p("Click",actionButton(ns("getdata"), "Go!" ,icon("paper-plane"), 
         #                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
         #                       ),"to import internal factors from SAP."),
         #          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
         #                           tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
         #                           tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
         #          
         #          dataTableOutput(ns("tab"))
         #          ),
       
        tabPanel(title= "Table",  tableUI(ns("table"))),
         tabPanel(title= "Data Understanding",
                  fluidRow(
                  column(3,div(p(style = 'color:darkblue;font-size: 25px',"Dependent Variable:"),
                               br(), br(),                
                               p(style = 'color:darkblue;font-size: 20px',"Monthly Sales Total:"),
                                                
                               verbatimTextOutput(ns("dv_summary"))
                                                  
                                                  
                                                 )),
                                                 
                  column(4,  p(style = 'color:darkblue;font-size: 25px',"Endogenous factors:"),
                         br(),
                      div(id = "mytable",style = 'margin-left:100px;margin-top:30px;text-align: justify;',
                                                      HTML('<table>
                              <tr>
                                                           <td><strong>Independent Variable </strong></td>
                                                           <td><strong>Data Source</strong></td>
                                                           </tr>
                                                           <tr>
                                                           <td>Price Per Unit </td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           <tr>
                                                           <td>Discount Percentage </td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           <tr>
                                                           <td>Marketing Expenditure</td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           <tr>
                                                           <td>Number of Retail Locations </td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           <tr>
                                                           <td>Sulphate content </td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           <tr>
                                                           <td>Market Size Index </td>
                                                           <td>SAP</td>
                                                           </tr>
                                                           </table>'))),
                  
                  column(5, p(style = 'color:darkblue;font-size: 25px',"Exogenous factors:"),
                         br(),
                         div(id = "mytable",style = 'margin-left:100px;margin-top:30px;text-align: justify;',

                         HTML('<table>
                      <tr>
                              <td><strong>Independent Variable</strong> </td>
                              <td><strong>Data Source</strong></td>
                              </tr>
                              <tr>
                              <td>Temperature </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Expected consumers income millions </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Health Consciousness Rate </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Personal Consumption Expenditures </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Population growth</td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Consumer_Price_Index_Urban </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Total Market Size Cumulative </td>
                              <td>Web Source</td>
                              </tr>
                              <tr>
                              <td>Total Market Size Monthly </td>
                              <td>Web Source</td>
                              </tr>

                              </table>')) ) )  ,
                  
                  selectInput(ns("dens_var"),"Choose Variable",choices=c(names(data1[,c(6,7,14:24,34:44)]))),
                  h4(tags$b("Density Plot")),
                  plotOutput(ns("density_plot"))
        ),
        
        #tabPanel(title= "Summary",  summary_dataUI(ns("summary_data"))),
        #tabPanel(title= "Imputation",  data_cleansingUI(ns("data_cleansing"))),
        #tabPanel(title= "Web Scraping",  scrapingUI(ns("scraping"))),
        tabPanel(title= "Imputation and Variable Standardisation",  featureUI(ns("feature")))
      )
    )
  )
  
}
data = function(input,output,session){
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  output$dv_summary = renderPrint({
    data_under = datafile()
    summary(data_under[6])
  })

  
  
  cle <- eventReactive(input$getdata,{
    shinyjs::disable("getdata")
    url <- 'http://10.6.102.222:8081/SAPConnector/ClearDemandForecastingAction'
    r <- GET(url)
    s <- content(r,"text",  encoding = "UTF-8")
    shinyjs::enable("getdata")
    return(s)
  })

  
  d <- eventReactive(input$getdata,{
    
    shinyjs::disable("getdata")
    url <- 'http://10.6.102.222:8081/SAPConnector/DemandForecastingDataAction'
    r <- GET(url)
    s <- content(r,"text",  encoding = "UTF-8")
    shinyjs::enable("getdata")
   
    return(s)
  })
  

  output$tab <- renderDataTable({
    cle()
    if(d() == 1){
      con <- dbConnect(MySQL(), user="yash", password="yash", dbname="sap", host="10.6.102.222")
      req1 <- dbSendQuery(con, "select * from DF_stock")
      rid1 <- fetch(req1,n=-1)
      rid1 = rid1[5:11]
      names(rid1) = c( "Monthly_sales_total","Sulphate_content","Market_Share","Price_Per_Unit","Advertising_costs","Number_of_Retail_Locations","Discount_Percentage")
    
      return(rid1)
    }
  }, options = list(scrollX = TRUE))
  
  
  output$density_plot <- renderPlot({
    data_under = datafile()
    dat1_col = data_under[,input$dens_var]
    m <- ggplot(data_under, aes(x= dat1_col)) +
      geom_histogram(aes(y = ..density..),fill = "#5E5A80") +
      
      geom_density() +
      xlab(input$dens_var)
    m
  })
  
  callModule(table,"table",datafile)
  #callModule(summary_data,"summary_data",datafile)
  #callModule(data_cleansing,"data_cleansing",datafile)
  #callModule(scraping,"scraping")
  callModule(feature,"feature",datafile)

  #callModule(data_visu,"data_visu",imputed)
}
