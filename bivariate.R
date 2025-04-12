source("forecast/causal.R")
source("forecast/bivariate_eda.R")
source("forecast/bivariate_predect.R")

bivariateUI = function(id, label="bivariate"){
  ns<-NS(id)
  tagList(
    
    sidebarPanel(width = 3,
      conditionalPanel(
        condition = "input['bivariate-tabs'] == 'bivariate-j'",
      selectInput(ns("endo"),"Select Feature",choices=c(names(data1[,c(23,36,24,35,22,17)])),
        selected = c(names(data1[23])))
      ),
      conditionalPanel(
        condition = "input['bivariate-tabs'] == 'bivariate-u'",
        selectInput(ns("type"),"Choose Location",choices = c("Country","Region","Store")),
        uiOutput(ns("select_inp_reg")),
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
      ),
      conditionalPanel(
        condition = "input['bivariate-tabs'] == 'bivariate-i'",
      selectInput(ns("feature_causal"),"Select Feature",choices=c(names(data1[,c(14,17,23,36)])),
        selected = c(names(data1[23])))
      )
    ),
    mainPanel(width = 9,
      tabsetPanel(id = ns("tabs"),
                  tabPanel(title= "EDA ", value=ns("u"),  
                           bivariate_edaUI(ns("bivariate_eda"))
                  ),
                  tabPanel(title= "Predictive",value=ns("j"),
                           bivariate_predectUI(ns("bivariate_predect"))
                  ),
                  tabPanel(title= "Causal",value=ns("i"),
                           causalUI(ns("causal"))
                          )
      )
    )
    
    
  )
}
  
bivariate = function(input,output,session){
  
  output$select_inp_reg <- renderUI({
    ns=session$ns
    if(input$type == "Region"){
      selectInput(ns("reg_wise"),"Choose Region",choices = c(names(data1[,c(38,39)])))
    } else if(input$type == "Store"){
      selectInput(ns("store_wise"),"Choose Region",choices = c(names(data1[,c(40:44)])))
    }
  })
  
 
  External<-reactive({ input$External })
  Internal<-reactive({ input$Internal }) 
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
  endo<-reactive({ input$endo })
  
  callModule(bivariate_eda,"bivariate_eda",timeline,year1,year2,year3,External,Internal,type,store_wise,reg_wise)
  callModule(causal,"causal",rbutton,Personal_Consumption,Expected_consumers,feature_causal,price,Discount,type,store_wise,reg_wise)
  callModule(bivariate_predect,"bivariate_predect",type,store_wise,reg_wise,endo)
}