
borutaUI = function(id,label ="boruta"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    fluidRow(
      column(width = 1,""),br(),br(),br(),br(),
      column(width = 5,
             highchartOutput(ns("radar"))
      ),
      column(width = 1,""),
      column(width = 5,
             dataTableOutput(ns("boruta_table"), width = 300)
      )
    )
  )
}

boruta=function(input,output,session,boruta_quartly,boruta_monthly,Time_boruta){
  
  dataInput <- function(boruta_values){
    
    hc_boruta = highchart() %>% 
      hc_chart(polar = TRUE, type = "line") %>% 
      hc_title(text = "Variable Importance") %>% 
      hc_xAxis(categories = c(names(data1[,c(15:19,21:24,34:38)])),
               
               tickmarkPlacement = 'on',
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = 'polygon',
               lineWidth = 0,
               min = 0) %>% 
      hc_series(
        list(
          name = "Variable Importance (Mean)",
          data = as.numeric(boruta_values$meanImp) ,
          pointPlacement = 'on'
        )
      )
    return(hc_boruta)
  }
  data_function <- function(){
    month_one <- data1[,c(1,2,6,15,16,17,18,19,21,22,23,24,34:38)]
    sub_data <- subset(month_one,month_one$Month == boruta_monthly())
    boruta.train <- Boruta(Monthly_sales_total~., data = sub_data, doTrace = 2)
    final.boruta <- TentativeRoughFix(boruta.train)
    boruta.df <- attStats(final.boruta)
    data_f <- data.frame(boruta.df)
    tra <- data.frame(t(data_f))
    data_f <- data.frame(t(tra[,-c(1,2)]))
  }
  output$radar <- renderHighchart({
    
    if(Time_boruta() == "Quarterly"){
      if(boruta_quartly() == "all_quarters"){
        boruta_values <- read.csv("forecast/allq.csv")
        dataInput(boruta_values)
      } else if(boruta_quartly() %in% "Q1"){
        boruta_values = read.csv("forecast/q1.csv")
        dataInput(boruta_values)
        
      } else if(boruta_quartly() == "Q2"){
        boruta_values = read.csv("forecast/q2.csv")
        dataInput(boruta_values)
        
      } else if(boruta_quartly() == "Q3"){
        boruta_values = read.csv("forecast/q3.csv")
        dataInput(boruta_values)
        
      } else if(boruta_quartly() == "Q4"){
        boruta_values = read.csv("forecast/q4.csv")
        dataInput(boruta_values)
      }
    } else if(Time_boruta() == "Monthly"){  
      if(boruta_monthly() == "1"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "2"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "3"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "4"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "5"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "6"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "7"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "8"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "9"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "10"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "11"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      } else if(boruta_monthly() == "12"){
        boruta_values <- data_function()
        dataInput(boruta_values)
      }
      
    }
  })
  
  output$boruta_table <- renderDataTable({
    if(Time_boruta() == "Quarterly"){
      if(boruta_quartly() == "all_quarters"){
        boruta_values <- read.csv("forecast/allq.csv")
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_quartly() %in% "Q1"){
        boruta_values = read.csv("forecast/q1.csv")
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_quartly() == "Q2"){
        boruta_values = read.csv("forecast/q2.csv")
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_quartly() == "Q3"){
        boruta_values = read.csv("forecast/q3.csv")
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_quartly() == "Q4"){
        boruta_values = read.csv("forecast/q4.csv")
        c = data.frame(boruta_values[,1:2])
      }
    }else if(Time_boruta() == "Monthly"){  
      
      if(boruta_monthly() == "1"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1])
      } else if(boruta_monthly() == "2"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1])
      } else if(boruta_monthly() == "3"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "4"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "5"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "6"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "7"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "8"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "9"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "10"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1])
      } else if(boruta_monthly() == "11"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      } else if(boruta_monthly() == "12"){
        boruta_values <- data_function()
        c = data.frame(boruta_values[,1:2])
      }
      
    }
  })
}