source("forecast/univariate_eda.R")
source("forecast/neural.R")
source("forecast/analysis.R")
source("forecast/forecast_output.R")
source("forecast/Neural_output.R")
univariateUI = function(id, label = "univariate"){
  ns = NS(id)
  tagList(
    
    sidebarPanel(
        conditionalPanel(
        condition = "input['univariate-tabs'] == 'univariate-a' ", 
               selectInput(
                 ns("exo"),
                 "Factors:",
                 choices = c(colnames(data1[,c(6,38:44)])),
                 selected = c(colnames(data1[,c(6,38:44)]))
               )
        ),
        conditionalPanel(
          condition = "input['univariate-tabs'] == 'univariate-b' | input['univariate-tabs'] == 'univariate-c'", 
                     selectInput(ns("type"),"Choose Location",choices = c("Country","Region","Store")),
                     uiOutput(ns("select_inp_reg"))
        ),
     
        
        conditionalPanel(
         condition = "input['univariate-tabs'] == 'univariate-b' ", 
             selectInput(ns("fcType"),"Select Time series Forecast Method",choices=c("ARIMA")),
             numericInput(ns("nWeeks"), "Select Number of Weeks to Forecast", 10, min = 5, max = 24),
                 conditionalPanel(
                   condition = "input['univariate-tabs_sub2'] == 'univariate-h' ", 
                     selectInput(ns("seasonality"),"Select Seasonality period",choices=c("Quarterly","Half-Yearly","Yearly"))
                 )
        ),
        conditionalPanel(
         condition = "input['univariate-tabs'] == 'univariate-c' ", 
         selectInput(ns("nnType"),"Select Neural Network Method",choices=c("Perceptron")),
         numericInput(ns("noWeeks"), "Select Number of Weeks to Forecast", 10, min = 5, max = 24)
        )
    ),
    mainPanel(
      tabsetPanel(id = ns("tabs"),

                  tabPanel(title= "EDA ", value=ns("a"),  
                           univariate_edaUI(ns("univariate_eda"))
                  ),
                  tabPanel(title= "Arima",value=ns("b"),
                           tabsetPanel(id = ns("tabs_sub2"),
                                       tabPanel(title= "Analysis", value=ns("h"),
                                                analysisUI(ns("analysis"))
                                       ),
                                       tabPanel(title= "Forecast_Output",value=ns("i"),
                                                forecast_outputUI(ns("forecast_output"))
                                       ) )),
                  
                  tabPanel(title= "Neural Network Analysis ",value=ns("c"),

                           tabsetPanel(id = ns("tabs_sub3"),
                                       tabPanel(title= "Analysis", value=ns("d"),
                                                nnUI(ns("nn"))
                                       ),
                                       tabPanel(title= "Forecast_Output",value=ns("e"),
                                                Neural_outputUI(ns("Neural_output"))
                                       ) )
                  )
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
  Expected_consumers<-reactive({ input$Expected_consumers })
  Personal_Consumption<-reactive({ input$Personal_Consumption })
  boruta_quartly<-reactive({ input$boruta_quartly })
  price_sensitivity<-reactive({ input$price_sensitivity })
  Discount_sensitivity<-reactive({ input$Discount_sensitivity })
  nnType<-reactive({ input$nnType })
  noWeeks<-reactive({ input$noWeeks })
  
  
  callModule(univariate_eda,"univariate_eda",timeline,exo,year1,year2,year3,type,store_wise,reg_wise)
  callModule(nn,"nn",nnType,noWeeks,type,store_wise,reg_wise)
  callModule(analysis,"analysis",fcType,nWeeks,type,store_wise,reg_wise,seasonality)
  callModule(forecast_output,"forecast_output",fcType,nWeeks,type,store_wise,reg_wise,seasonality)
  callModule(Neural_output,"Neural_output",nnType,noWeeks,type,store_wise,reg_wise)
}