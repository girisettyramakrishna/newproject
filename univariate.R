source("forecast/univariate_eda.R")
source("forecast/neural.R")
source("forecast/analysis.R")
source("forecast/forecast_output.R")
source("forecast/Neural_output.R")
source("forecast/causal.R")
source("forecast/bivariate_eda.R")

#preprocessing data for market share analysis
#date conversion
data1[,3] <- as.Date(data1[,3], format = "%d/%m/%Y")
#north region
agg_nr <- aggregate(x = as.numeric(data1$NORTH_REGION), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_nr) <- c("Date","Sales")
agg_NorthR <- aggregate(agg_nr$Sales ~ year(Date), data = agg_nr, sum)  
colnames(agg_NorthR) <- c("Date","Sales")
agg_NorthR$Sales = round((agg_NorthR$Sales),0)

#south region
agg_sr <- aggregate(x = as.numeric(data1$SOUTH_REGION), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_sr) <- c("Date","Sales")
agg_SouthR <- aggregate(agg_sr$Sales ~ year(Date), data = agg_sr, sum)  
colnames(agg_SouthR) <- c("Date","Sales")
agg_SouthR$Sales = round((agg_SouthR$Sales),0)

#north store1
agg_ns1 <- aggregate(x = as.numeric(data1$NORTH_STORE1), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_ns1) <- c("Date","Sales")
agg_NorthS1 <- aggregate(agg_ns1$Sales ~ year(Date), data = agg_ns1, sum)  
colnames(agg_NorthS1) <- c("Date","Sales")
agg_NorthS1$Sales = round((agg_NorthS1$Sales),0)

#north store2
agg_ns2 <- aggregate(x = as.numeric(data1$NORTH_STORE2), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_ns2) <- c("Date","Sales")
agg_NorthS2 <- aggregate(agg_ns2$Sales ~ year(Date), data = agg_ns2, sum)  
colnames(agg_NorthS2) <- c("Date","Sales")
agg_NorthS2$Sales = round((agg_NorthS2$Sales),0)

#south store1
agg_ss1 <- aggregate(x = as.numeric(data1$SOUTH_STORE1), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_ss1) <- c("Date","Sales")
agg_SouthS1 <- aggregate(agg_ss1$Sales ~ year(Date), data = agg_ss1, sum)  
colnames(agg_SouthS1) <- c("Date","Sales")
agg_SouthS1$Sales = round((agg_SouthS1$Sales),0)

#south store2
agg_ss2 <- aggregate(x = as.numeric(data1$SOUTH_STORE2), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_ss2) <- c("Date","Sales")
agg_SouthS2 <- aggregate(agg_ss2$Sales ~ year(Date), data = agg_ss2, sum)  
colnames(agg_SouthS2) <- c("Date","Sales")
agg_SouthS2$Sales = round((agg_SouthS2$Sales),0)

#south store3
agg_ss3 <- aggregate(x = as.numeric(data1$SOUTH_STORE3), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_ss3) <- c("Date","Sales")
agg_SouthS3 <- aggregate(agg_ss3$Sales ~ year(Date), data = agg_ss3, sum)  
colnames(agg_SouthS3) <- c("Date","Sales")
agg_SouthS3$Sales = round((agg_SouthS3$Sales),0)

#overall
agg_y <- aggregate(x = as.numeric(data1$Monthly_sales_total), FUN = sum, by = list(Group.date = data1[,3]))
colnames(agg_y) <- c("Date","Sales")
agg_overall <- aggregate(agg_y$Sales ~ year(Date), data = agg_y, sum)  
colnames(agg_overall) <- c("Date","Sales")
agg_overall$Sales = round((agg_overall$Sales),0)



univariateUI = function(id, label = "univariate"){
  ns = NS(id)

  tagList(
    
    sidebarPanel(width = 3,
        # conditionalPanel(
        # condition = "input['univariate-tabs'] == 'univariate-a' ", 
        #        
        # # highchartOutput(ns("trend_Sales"))
        # ),
        # conditionalPanel(
        #   condition = "input['univariate-tabs'] == 'univariate-b' | input['univariate-tabs'] == 'univariate-c'", 
                     selectInput(ns("type"),"Choose Location",choices = c("Country","Region","Store")),
                     uiOutput(ns("select_inp_reg")),
        # ),
     
        
        conditionalPanel(
         condition = "input['univariate-tabs'] == 'univariate-b' ", 
             selectInput(ns("fcType"),"Select Time series Forecast Method",choices=c("ARIMA")),
             numericInput(ns("nWeeks"), "Select Number of Weeks to Forecast", 10, min = 5, max = 24),
             selectInput(ns("seasonality"),"Select Seasonality period",choices=c("Quarterly","Half-Yearly","Yearly")),
         selectInput(
           ns("exo"),
           "By Segments:",
           choices = c(colnames(data1[,c(6,38:44)])),
           selected = c(colnames(data1[,c(6,38:44)]))
         ),
         
         valueBox(textOutput(ns("perc_share")),"Percentage of Sales",width = 12 ),
         highchartOutput(ns("Market_share"))
        ),
        conditionalPanel(
          condition = "input['univariate-tabs'] == 'univariate-cd'",
          # selectInput(ns("type"),"Choose Location",choices = c("Country","Region","Store")),
          # uiOutput(ns("select_inp_reg")),
          selectInput(ns("timeline"),"Choose Time Frequency",choices = c("Yearly","Monthly"), selected = "Monthly"),

          conditionalPanel(
            condition = "input['bivariate-timeline'] == 'Yearly'",
            selectInput(ns("year1"),"From",choices = unique(data1[,c(1)]) ),
            selectInput(ns("year2"),"To",selected = 2016, unique(data1[,c(1)]))
          ),
          conditionalPanel(
            condition = "input['bivariate-timeline'] == 'Monthly'",
            selectInput(ns("year3"),"Select Year",choices = unique(data1[,c(1)]) )
          ),

          uiOutput(ns("slides")),
          uiOutput(ns("slides2")),
          selectInput(
            ns("External"),
            "External",
            choices = c(colnames(data1[,c(14,15,17:21,37)])),
            selected = c(colnames(data1[14]))
          ),
          selectInput(
            ns("Internal"),
            "Internal",
            selected = c(colnames(data1[24])),
            choices = c(colnames(data1[,c(16,22:24,35,36)]))
          )
        )
        # ,
        # conditionalPanel(
        #   condition = "input['univariate-tabs'] == 'univariate-cd'",
        #   selectInput(ns("feature_causal"),"Select Feature",choices=c(names(data1[,c(14,17,23,36)])),
        #               selected = c(names(data1[23])))
        # )
        # conditionalPanel(
        #  condition = "input['univariate-tabs'] == 'univariate-c' ", 
        #  selectInput(ns("nnType"),"Select Neural Network Method",choices=c("Perceptron")),
        #  numericInput(ns("noWeeks"), "Select Number of Weeks to Forecast", 10, min = 5, max = 24)
        # )
    ),
    mainPanel(width = 9,
      tabsetPanel(id = ns("tabs"),

                  # tabPanel(title= "EDA ", value=ns("a"),  
                  #          univariate_edaUI(ns("univariate_eda"))
                  # ),
                  tabPanel(title= "Univariate EDA and Time Trend Analysis",value=ns("b"),
                           analysisUI(ns("analysis"))
                           # tabsetPanel(id = ns("tabs_sub2"),
                                       # tabPanel(title= "Analysis", value=ns("h"),
                                                
                                       # )
                                       # tabPanel(title= "Forecast_Output",value=ns("i"),
                                       #          forecast_outputUI(ns("forecast_output"))
                                       # ) 
                                       # )
                           )
                  ,
                  tabPanel(title = "Bivariate and Causal analysis",
                           tabsetPanel(
                             tabPanel(title = "Comparitive Correlation Analysis"
                                      , value = ns("cd"),
                                      bivariate_edaUI(ns("bivariate_eda"))
                                      ),
                             tabPanel(title = "Demand Elasticity Estimation",
                                      fluidRow(
                                        column(6,
                                               selectInput(ns("feature_causal"),"Select Feature 1:",choices=c(names(data1[,c(14,17,23,36)])),
                                                           selected = c(names(data1[23])))
                                        ),
                                        column(6,
                                               selectInput(ns("feature_causal_2"),"Select Feature 2:",choices=c(names(data1[,c(14,17,23,36)])),
                                                           selected = c(names(data1[23])))
                                        )
                                      ),
                                      causalUI(ns("causal")))
                             
                           )
                           
                          

                  )
                  
                  # tabPanel(title= "Neural Network Analysis ",value=ns("c"),
                  # 
                  #          tabsetPanel(id = ns("tabs_sub3"),
                  #                      tabPanel(title= "Analysis", value=ns("d"),
                  #                               nnUI(ns("nn"))
                  #                      ),
                  #                      tabPanel(title= "Forecast_Output",value=ns("e"),
                  #                               Neural_outputUI(ns("Neural_output"))
                  #                      ) )
                  # )
      )
    )
    
  )
 
}

univariate = function(input,output, session,timeline,year1,year2,year3,type,store_wise,reg_wise){

  output$select_inp_reg <- renderUI({
    ns=session$ns
    if(input$type == "Region"){
      selectInput(ns("reg_wise"),"Choose Region",choices = c(names(data1[,c(38,39)])))
    } else if(input$type == "Store"){
      selectInput(ns("store_wise"),"Choose Region",choices = c(names(data1[,c(40:44)])))
    }
  })
  
  
  
  output$perc_share = renderText({
    perc_share = (sum(data1[,exo()])/(sum(data1[,6])))*100
    perc_share = round(perc_share,2)
    perc_share
    
  })
  
  output$trend_Sales <- renderHighchart({
    data1[,3] <- as.Date(data1[,3], format = "%d/%m/%Y")
    agg_x <- aggregate(x = as.numeric(data1[,exo()]), FUN = sum, by = list(Group.date = data1[,3]))
    colnames(agg_x) <- c("Date","Sales")
    agg_data <- aggregate(agg_x$Sales ~ year(Date), data = agg_x, sum)  
    colnames(agg_data) <- c("Date","Sales")
    agg_data$Sales = round((agg_data$Sales),0)
    xaxis_lab <- as.character(agg_data$Date)
    hc <- highchart() %>%
      hc_xAxis(categories = xaxis_lab) %>%
      hc_add_series(name = "Sales",data = agg_data$Sales) %>%
      hc_title(text = "Sales")
    hc
  })
  
  output$Market_share <- renderHighchart({
    xaxis_lab <- as.character(agg_overall$Date)
    if(data1[,exo()]==data1$NORTH_REGION | data1[,exo()]==data1$SOUTH_REGION | data1[,exo()]==data1$Monthly_sales_total){
      
      if(data1[,exo()]==data1$SOUTH_REGION){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Region", data = ((agg_NorthR$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Region", data = ((agg_SouthR$Sales/agg_overall$Sales)*100))
      }else{
      hc <- highchart() %>%
        hc_xAxis(categories = xaxis_lab) %>%
        hc_title(text = "Market Share") %>%
        hc_add_series(name = "South Region", data = ((agg_SouthR$Sales/agg_overall$Sales)*100))%>%
        hc_add_series(name = "North Region", data = ((agg_NorthR$Sales/agg_overall$Sales)*100))
      }
       hc
      
      
    } else{
      if(data1[,exo()]==data1$NORTH_STORE1){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Store2", data = ((agg_NorthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "North Store1", data = ((agg_NorthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store1", data = ((agg_SouthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store2", data = ((agg_SouthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store3", data = ((agg_SouthS3$Sales/agg_overall$Sales)*100))
      } else if(data1[,exo()]==data1$NORTH_STORE2){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Store1", data = ((agg_NorthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "North Store2", data = ((agg_NorthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store1", data = ((agg_SouthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store2", data = ((agg_SouthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store3", data = ((agg_SouthS3$Sales/agg_overall$Sales)*100))
      } else if(data1[,exo()]==data1$SOUTH_STORE1){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Store1", data = ((agg_NorthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store1", data = ((agg_SouthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "North Store2", data = ((agg_NorthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store2", data = ((agg_SouthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store3", data = ((agg_SouthS3$Sales/agg_overall$Sales)*100))
      } else if(data1[,exo()]==data1$SOUTH_STORE2){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Store1", data = ((agg_NorthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store2", data = ((agg_SouthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "North Store2", data = ((agg_NorthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store1", data = ((agg_SouthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store3", data = ((agg_SouthS3$Sales/agg_overall$Sales)*100))
      } else if(data1[,exo()]==data1$SOUTH_STORE3){
        hc <- highchart() %>%
          hc_xAxis(categories = xaxis_lab) %>%
          hc_title(text = "Market Share") %>%
          hc_add_series(name = "North Store1", data = ((agg_NorthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store3", data = ((agg_SouthS3$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "North Store2", data = ((agg_NorthS2$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store1", data = ((agg_SouthS1$Sales/agg_overall$Sales)*100))%>%
          hc_add_series(name = "South Store2", data = ((agg_SouthS2$Sales/agg_overall$Sales)*100))
        
      }
      hc
    }
  })
  
  
  
  
  
  External<-reactive({ input$External })
  Internal<-reactive({ input$Internal }) 
  fcType <- reactive({input$fcType})
  exo<-reactive({ input$exo })
  timeline<-reactive({ input$timeline })
  year1<-reactive({ input$year1 })
  year2<-reactive({ input$year2 })
  year3<-reactive({ input$year3 })
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
  feature_causal_2<-reactive({ input$feature_causal_2 })
  
  Expected_consumers<-reactive({ input$Expected_consumers })
  Personal_Consumption<-reactive({ input$Personal_Consumption })
  boruta_quartly<-reactive({ input$boruta_quartly })
  price_sensitivity<-reactive({ input$price_sensitivity })
  Discount_sensitivity<-reactive({ input$Discount_sensitivity })
  nnType<-reactive({ input$nnType })
  noWeeks<-reactive({ input$noWeeks })
  
  callModule(bivariate_eda,"bivariate_eda",timeline,year1,year2,year3,External,Internal,type,store_wise,reg_wise)
  callModule(causal,"causal",rbutton,Personal_Consumption,Expected_consumers,feature_causal,feature_causal_2,price,Discount,type,store_wise,reg_wise)
  # 
  # callModule(univariate_eda,"univariate_eda",timeline,exo,year1,year2,year3,type,store_wise,reg_wise)
  # callModule(nn,"nn",nnType,noWeeks,type,store_wise,reg_wise)
  callModule(analysis,"analysis",timeline,exo,year1,year2,year3,fcType,nWeeks,type,store_wise,reg_wise,seasonality)
  # callModule(forecast_output,"forecast_output",fcType,nWeeks,type,store_wise,reg_wise,seasonality)
  # callModule(Neural_output,"Neural_output",nnType,noWeeks,type,store_wise,reg_wise)
}