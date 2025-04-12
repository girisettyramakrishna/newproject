source("forecast/multi_variate_analysis.R")
source("forecast/sensitivity.R")
source("forecast/boruta.R")
source("forecast/demandmaximization.R")

# con <- dbConnect(MySQL(), user="yash", password="yash", dbname="sap", host="10.6.102.222")
# req1 <- dbSendQuery(con, "select * from DF_stock")
# rid1 <- fetch(req1,n=-1)
# rid1 = rid1[1:11]
# rid1 <- rid1[order(as.Date(rid1$PDATE,format = "%d/%m/%Y")),]
# rid1 <- rid1[5:11]
# names(rid1) = c("Monthly_sales_total","Sulphate_content","Market_Share","Price_Per_Unit","Advertising_costs","Number_of_Retail_Locations","Discount_Percentage")
# 
# notsap <- read.csv("Product_data.csv")
# notsap = notsap[,-c(6,16,22,23,24,35,36)]
# ns_d <- cbind(rid1,notsap)
# data1 <- ns_d[,c(8:12,1,13:21,2,22:26,3,4,5,27:36,6,7,37:44)]
# data1$Number_of_Retail_Locations=as.numeric(data1$Number_of_Retail_Locations)

data1 = read.csv("Product_data.csv")
for(i in 1:ncol(data1)){
  data1[is.na(data1[,i]), i] <- mean(data1[,i], na.rm = TRUE)
}

maximum_sc <- max(tail(data1[16], n=20))
minimum_sc <- min(tail(data1[16], n=20))
avg <- (maximum_sc - minimum_sc) / 2
maximum_sc <- maximum_sc + avg


maximum_ms <- max(tail(data1[22], n=20))
minimum_ms <- min(tail(data1[22], n=20))
avg <- (maximum_ms - minimum_ms) / 2
maximum_ms <- maximum_ms + avg


maximum_ps <- max(tail(data1[23], n=20))
minimum_ps <- min(tail(data1[23], n=20))
avg <- (maximum_ps - minimum_ps) / 2
maximum_ps <- maximum_ps + avg


maximum_ac <- max(tail(data1[24], n=20))
minimum_ac <- min(tail(data1[24], n=20))
avg <- (maximum_ac - minimum_ac) / 2
maximum_ac <- maximum_ac + avg


maximum_norl <- max(tail(data1[35], n=20))
minimum_norl <- min(tail(data1[35], n=20))
avg <- (maximum_norl - minimum_norl) / 2
maximum_norl <- maximum_norl + avg


maximum_ds <- max(tail(data1[36], n=20))
minimum_ds <- min(tail(data1[36], n=20))
avg <- (maximum_ds - minimum_ds) / 2
maximum_ds <- maximum_ds + avg


forecastUI = function(id,label="forecast"){
  ns <- NS(id)
  tagList(
    bootstrapPage(
    sidebarPanel(width = 3,
       conditionalPanel(
         condition = "input['forecast-tabs'] == 'forecast-d'", 
          selectInput(ns("type"),"Choose Location",choices = c("Country","Region","Store")),
          uiOutput(ns("select_inp_reg")),
          radioButtons(ns("rbutton"),"Select Regression Type", choices=c("Automatic","Manual"), selected ="Automatic" , inline = FALSE,width = NULL),
         conditionalPanel(
           condition = "input['forecast-rbutton'] == 'Manual'", 
                 selectInput(ns("features"),"Select Features",choices=c(            
                   names(data1[,c(11,14:22,7,23,24,34:37)])
                   ),
                    selected = c(names(data1[,c(23,36,18,11)])),multiple=TRUE)
         )
         
       ),         
            
       conditionalPanel(
         condition = "input['forecast-tabs'] == 'forecast-f'", 
         selectInput(ns("Time_boruta"),"Select Time",choices=c("Monthly","Quarterly"), selected = "Quarterly"),
         conditionalPanel(
           condition = "input['forecast-Time_boruta'] == 'Quarterly'", 
              selectInput(ns("boruta_quartly"),"Select Quarterly Period",choices=c("all_quarters","Q1","Q2","Q3","Q4"), selected = "all_quarters")
         ),
         conditionalPanel(
           condition = "input['forecast-Time_boruta'] == 'Monthly'", 
           selectInput(ns("boruta_monthly"),"Select Monthly Period",choices=c("1","2","3","4","5","6","7","8","9","10","11","12"), selected = "1")
         )
          ),  
       conditionalPanel(
         condition = "input['forecast-tabs'] == 'forecast-g'",   
            sliderInput(ns("price_sensitivity"),"Price of Product",min=5,max=50,value=5,step=0.25),
            sliderInput(ns("Discount_sensitivity"),"Discount on Product",min=1,max=35,value=5,step=0.25),
           sliderInput(ns("Advertising_costs"),"Advertising cost",min=4000000,max=9000000,value=5,step=10),
           sliderInput(ns("Sulphate_content"),"Sulphate content",min=85,max=110,value=5,step=1),
           sliderInput(ns("Market_Share"),"Market Share",min=20,max=60,value=55,step=1),
         sliderInput(ns("Number_of_Retail_Locations"),"Number of Retail Locations",min=1,max=60,value=50,step=1)
         
       ),
       conditionalPanel(
         condition = "input['forecast-tabs'] == 'forecast-h'", 
           sliderInput(ns("Sulphate_content_i"),"Sulphate content",min=minimum_sc,max=maximum_sc,value=c(104,106),step=1),
           sliderInput(ns("Price_sensitivity_i"),"Price of Product",min=minimum_ps,max=maximum_ps,value=c(5,8),step=1),
           sliderInput(ns("Advertising_costs_i"),"Marketing Expenditure",min=minimum_ac,max=maximum_ac,value=c(7808534,8908594),step=10),
           sliderInput(ns("Discount_sensitivity_i"),"Discount on Product",min=minimum_ds,max=maximum_ds,value=c(8,15),step=1)
       )
    )
    ),
    mainPanel(width = 9,
      tabsetPanel(id = ns("tabs"),
                             tabPanel(title= "Introduction", value=ns("r"), 
                                      div(style = 'margin-left:100px;margin-top:30px;text-align: justify;',
                                          p(style = 'color:darkblue;font-size: 30px',"Multivariate Analysis: "),
                                          p(style = 'color:black;font-size: 18px;',"Observing the impact of Endogenous and Exogenous factors on the dependent variable,
                                            Monthly Sales."),
                                          p(style = 'color:darkblue;font-size: 20px',"Predictive:"),
                                          p(style = 'color:black;font-size: 18px;',
                                            "Using two methods of regression:",br(),
                                            "1. Automatic - Method1: Fits the regression model by adding/dropping variables one at a time based on minimum AIC (Akaike Information Criterion) ",br(),
                                            "2. Manual - Method2: Fits the regression model based on least sum of squares error. "
                                            
                                          ),                                          p(style = 'color:darkblue;font-size: 20px',"Variable Importance:"),
                                          p(style = 'color:black;font-size: 18px;', 
                                            " Variable importance analysis helps identify the  most significant variables affecting sales. 
                                            "),
                                          p(style = 'color:darkblue;font-size: 20px',"Sensitivity Scenario Analysis:"),
                                          p(style = 'color:black;font-size: 18px;', 
                                            "Sensitivity Scenario analysis enables decision makers understand the impact of endogenous factors on sales.
                                            ")
                                          
                                          )
                                          ),
                             tabPanel(title= "Predictive", value=ns("d"), multi_variateUI(ns("multi_variate"))),
                             tabPanel(title= "Variable Importance",value=ns("f"),  borutaUI(ns("boruta"))),
                             tabPanel(title= "Sensitivity Scenario Analysis",value=ns("g"),  sensitivityUI(ns("sensitivity"))),
                             tabPanel(title= "Demand Maximization",value=ns("h"), Demand_maximizationUI(ns("Demand_maximization")))
                 
      )
    )
  )
  
}


forecast = function(input,output,session){
 
  output$select_inp_reg <- renderUI({
    ns=session$ns
    if(input$type == "Region"){
      selectInput(ns("reg_wise"),"Choose Region",choices = c(names(data1[,c(38,39)])))
    } else if(input$type == "Store"){
      selectInput(ns("store_wise"),"Choose Region",choices = c(names(data1[,c(40:44)])))
    }
  })

  sliderValues  <- reactive({ 
    data.frame(
      Name = c("Range"),
      Value = as.character(c(paste(Sulphate_content_i(), collapse=' '))), 
      stringsAsFactors=FALSE)
  })
  
  output$values <- renderTable({
    sliderValues()
  })
  
  # output$slider_inp <- renderUI({
  #   ns=session$ns
  #   if(input$feature_causal == "Expected_consumers_income_millions"){
  #     sliderInput(ns("Expected_consumers"),"Expected_consumers",min=300,max=600,value=417.6329575,step=5)
  #   } else if(input$feature_causal == "Personal_Consumption_Expenditures"){
  #     sliderInput(ns("Personal_Consumption"),"Personal_Consumption",min=8000,max=15000,value=12510.5,step=100)
  #   } else if(input$feature_causal == "Price_Per_Unit"){
  #     sliderInput(ns("price"),"Price of Product",min=5,max=50,value=5,step=0.25)
  #   } else if(input$feature_causal == "Discount_Percentage"){
  #     sliderInput(ns("Discount"),"Discount on Product",min=1,max=35,value=5,step=0.25)
  #   }
  # })
  
  timeline<-reactive({ input$timeline })
  year1<-reactive({ input$year1 })
  year2<-reactive({ input$year2 })
  year3<-reactive({ input$year3 })
  External<-reactive({ input$External })
  Internal<-reactive({ input$Internal })
  fcType<-reactive({ input$fcType })
  nWeeks<-reactive({ input$nWeeks })
  features<-reactive({ input$features })
  price<-reactive({ input$price })
  rbutton<-reactive({ input$rbutton })
  Discount<-reactive({ input$Discount })
  type<-reactive({ input$type })
  reg_wise<-reactive({ input$reg_wise })
  store_wise<-reactive({ input$store_wise })
  seasonality<-reactive({ input$seasonality })
  feature_causal<-reactive({ input$feature_causal })
  Expected_consumers<-reactive({ input$Expected_consumers })
  Personal_Consumption<-reactive({ input$Personal_Consumption })
  Time_boruta <-reactive({ input$Time_boruta })
  boruta_quartly<-reactive({ input$boruta_quartly })
  boruta_monthly<-reactive({ input$boruta_monthly })
  price_sensitivity<-reactive({ input$price_sensitivity })
  Discount_sensitivity<-reactive({ input$Discount_sensitivity })
  nnType<-reactive({ input$nnType })
  noWeeks<-reactive({ input$noWeeks })
  Advertising_costs<-reactive({ input$Advertising_costs })           
  Sulphate_content<-reactive({ input$Sulphate_content })
  Market_Share<-reactive({ input$Market_Share })
  Number_of_Retail_Locations   <-reactive({ input$Number_of_Retail_Locations })
  
  Sulphate_content_i<-reactive({ input$Sulphate_content_i })
  Price_sensitivity_i<-reactive({ input$Price_sensitivity_i })
  Advertising_costs_i<-reactive({ input$Advertising_costs_i })
  Discount_sensitivity_i<-reactive({ input$Discount_sensitivity_i })
  
  callModule(multi_variate,"multi_variate",rbutton,features,price,Discount,type,store_wise,reg_wise)
  callModule(boruta,"boruta",boruta_quartly,boruta_monthly,Time_boruta)
  callModule(sensitivity,"sensitivity",rbutton,type,store_wise,reg_wise,Discount_sensitivity,price_sensitivity,Advertising_costs,Sulphate_content,Market_Share,Number_of_Retail_Locations)
  callModule(Demand_maximization,"Demand_maximization",Sulphate_content_i,Price_sensitivity_i,Advertising_costs_i,Discount_sensitivity_i)
}