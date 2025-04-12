nnUI <- function(id,label ="nn"){
  ns <- NS(id)
tagList(
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                   tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
  
    plotOutput(ns("nn")),
    verbatimTextOutput(ns("accuracy_nn"))
)
  
}

nn<- function(input,output,session,nnType,noWeeks,type,store_wise,reg_wise){

  data_arima = data1[,c(1,2,6,38:44)]
  data_arima$date <- as.yearmon(paste(data_arima$Year, data_arima$Month), "%Y %m")
  data_arima=data_arima[,-c(1,2)]
  
  ######################
  ##country wise########
  data_arima_country=data_arima[,c(2,1)]
  names(data_arima_country)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_country = ts((data_arima_country$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_country) <- "Monthly Sales"
  
  #######################
  ##north region wise#######
  data_arima_north=data_arima[,c(9,2)]
  names(data_arima_north)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_north = ts((data_arima_north$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_north) <- "Monthly Sales"
  
  ########################
  ##south region wise#####
  data_arima_south=data_arima[,c(9,3)]
  names(data_arima_south)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_south = ts((data_arima_south$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_south) <- "Monthly Sales"
  
  ##########################
  ##north_store1########
  data_arima_NS1 =data_arima[,c(9,4)]
  names(data_arima_NS1)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_NS1 = ts((data_arima_NS1$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_NS1) <- "Monthly Sales"
  
  ###########################
  ##north_store2#########
  data_arima_NS2=data_arima[,c(9,5)]
  names(data_arima_NS2)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_NS2 = ts((data_arima_NS2$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_NS2) <- "Monthly Sales"
  
  ###########################
  ## south_store_1##########
  data_arima_SS1=data_arima[,c(9,6)]
  names(data_arima_SS1)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS1 = ts((data_arima_SS1$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS1) <- "Monthly Sales"
  
  ##########################
  ## south_store_2##########
  data_arima_SS2=data_arima[,c(9,7)]
  names(data_arima_SS2)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS2 = ts((data_arima_SS2$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS2) <- "Monthly Sales"
  
  #########################
  ## south_store_3#########
  data_arima_SS3=data_arima[,c(9,8)]
  names(data_arima_SS3)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS3 = ts((data_arima_SS3$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS3) <- "Monthly Sales"
  
  
  #########plot for neural network ######
plot_nn = function(data_procedureA_country,weeks){
          decom =decompose(data_procedureA_country)
          sum(is.na(decom))
          random = data_procedureA_country - decom$seasonal
          r_df = data.frame(random)
          fit <- nnetar((na.omit(r_df$random)),lambda=0)
          autoplot(predict(fit,h=weeks))
}

#########accuracy for neural network ######
accuracy_nn = function(data_procedureA_country,x,weeks){
  decom =decompose(data_procedureA_country)
  sum(is.na(decom))
  random = data_procedureA_country - decom$seasonal
  r_df = data.frame(random)
  fit <- nnetar((na.omit(r_df$random)),lambda=0)
  autoplot(predict(fit,h=weeks))
  predicted <- fit$fitted + decom$seasonal
  accuracy(predicted, x, test=NULL, d=NULL, D=NULL)
  
}

# # boosted model 
# 
# # devtools::install_github("ellisp/forecastxgb-r-package/pkg")
# library(forecastxgb)
# model <- xgbar(data_procedureA_country)
# fc <- predict(model, h = 10)
# plot(fc)
# xgboost.accuracy <- accuracy(fc, data1$Monthly_sales_total, test=NULL, d=NULL, D=NULL)


####### plot for nn ######
  output$nn <- renderPlot({
    if(type() == "Country"){
      if(nnType() == "Perceptron"){
        plot_nn(data_procedureA_country,noWeeks())
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_north,noWeeks())
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_south,noWeeks())
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_NS1,noWeeks())
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_NS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_SS1,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(nnType() == "Perceptron"){
          plot_nn(data_procedureA_SS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        plot_nn(data_procedureA_SS3,noWeeks())
      }
    }
    
  })
  
  # neural network time series forecasting
  output$accuracy_nn = renderPrint({
    if(type() == "Country"){
      if(nnType() == "Perceptron"){
        accuracy_nn(data_procedureA_country,data1[,6],noWeeks())
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(nnType() == "Perceptron"){
          accuracy_nn(data_procedureA_north,data1[,38],noWeeks())
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(nnType() == "Perceptron"){
          #fit <- nnetar(data1$SOUTH_REGION,lambda=0)
          accuracy_nn(data_procedureA_south, data1[,39], noWeeks())  
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(nnType() == "Perceptron"){
          accuracy_nn(data_procedureA_NS1,data1[,40],noWeeks())
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(nnType() == "Perceptron"){
          accuracy_nn(data_procedureA_NS2,data1[,41],noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(nnType() == "Perceptron"){
          accuracy_nn(data_procedureA_SS1,data1[,42],noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(nnType() == "Perceptron"){
          accuracy_nn(data_procedureA_SS2,data1[,43],noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        accuracy_nn(data_procedureA_SS3,data1[,44],noWeeks())
      }
    }
  })
}