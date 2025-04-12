forecast_outputUI = function(id,label ="forecast_output"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    highchartOutput(ns("forecast_plot")),
    dataTableOutput(ns("pred_sales")) 
    
  )}
    
    
    
    
forecast_output=function(input,output,session,fcType,nWeeks,type,store_wise,reg_wise,seasonality){
  
  
  data_arima = data1[,c(1,2,6,38:44)]
  data_arima$date <- as.yearmon(paste(data_arima$Year, data_arima$Month), "%Y %m")
  data_arima=data_arima[,-c(1,2)]
  
  ######################
  ##country wise########
  data_arima_country= data.frame(data_arima[,c(1)])
  names(data_arima_country)=c('Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_country = ts((data_arima_country$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_country) <- c("Monthly Sales")
  
  fit_country = arima(data_procedureA_country,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  
  
  #######################
  ##north region wise#######
  data_arima_north=data_arima[,c(9,2)]
  names(data_arima_north)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_north = ts((data_arima_north$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_north) <- "Monthly Sales"
  
  fit_north = arima(data_procedureA_north,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  # # Fit MLP
  # install.packages("RSNNS")
  # library(RSNNS)
  # mlp.fit <- mlp(data1$Time_Variable, data1$Monthly_sales_total)
  # plot(mlp.fit)
  # print(mlp.fit)
  # mlp.frc <- forecast(mlp.fit,h=tst.n)
  # plot(mlp.frc)
  
  ########################
  ##south region wise#####
  data_arima_south=data_arima[,c(9,3)]
  names(data_arima_south)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_south = ts((data_arima_south$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_south) <- "Monthly Sales"
  
  fit_south = arima(data_procedureA_south,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  ##########################
  ##north_store1########
  data_arima_NS1 =data_arima[,c(9,4)]
  names(data_arima_NS1)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_NS1 = ts((data_arima_NS1$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_NS1) <- "Monthly Sales"
  
  fit_NS1 = arima(data_procedureA_NS1,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  ###########################
  ##north_store2#########
  data_arima_NS2=data_arima[,c(9,5)]
  names(data_arima_NS2)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_NS2 = ts((data_arima_NS2$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_NS2) <- "Monthly Sales"
  
  fit_NS2 = arima(data_procedureA_NS2,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  ###########################
  ## south_store_1##########
  data_arima_SS1=data_arima[,c(9,6)]
  names(data_arima_SS1)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS1 = ts((data_arima_SS1$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS1) <- "Monthly Sales"
  
  fit_SS1 = arima(data_procedureA_SS1,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  ##########################
  ## south_store_2##########
  data_arima_SS2=data_arima[,c(9,7)]
  names(data_arima_SS2)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS2 = ts((data_arima_SS2$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS2) <- "Monthly Sales"
  
  fit_SS2 = arima(data_procedureA_SS2,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  #########################
  ## south_store_3#########
  data_arima_SS3=data_arima[,c(9,8)]
  names(data_arima_SS3)=c('Date','Monthly Sales')
  
  ### CONVERSION TO TIME SERIES DATA
  data_procedureA_SS3 = ts((data_arima_SS3$`Monthly Sales`),c(2004,3),end = c(2016,12),frequency =12 )
  names(data_procedureA_SS3) <- "Monthly Sales"
  
  fit_SS3 = arima(data_procedureA_SS3,order = c(0,1,1), seasonal = list(order = c(0,1,2)))
  
  
  
  
  ########  ARIMA forecast highchart #############
  hc_arima1 = function(series2,xAxis){
    hc1=highchart() %>% 
      hc_title(text = "Monthly Sales for stores") %>% 
      hc_add_series(name = "Predicted  Sales", data = series2,
                    dataLabels = list(enabled = FALSE))%>%
      hc_xAxis(categories = xAxis)
    return(hc1)
  }
  

  
  ################ forecast plot ###########
  output$forecast_plot <- renderHighchart({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        predicted <- predict(fit_country, n.ahead =nWeeks() , prediction.interval = TRUE)
        all <- predicted$pred
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
        year[2]=2017.085
        f <- format(date_decimal(year), "%b %Y")
        all1=cbind(f,all1)
        
        hc_arima1(all1$all,all1$f)
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_north, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_south, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- predicted$pred
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          year[2]=2017.085
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        predicted <- predict(fit_SS3, n.ahead = nWeeks(), prediction.interval = TRUE)
        all <- predicted$pred
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
        year[2]=2017.085
        f <- format(date_decimal(year), "%b %Y")
        all1=cbind(f,all1)
        
        hc_arima1(all1$all,all1$f)
      }
    }
    
  })
  
  ########## PREDICTED SALES ARIMA TABLE ##########
  
  pred_sales_func = function(fit,weeks){
    a=as.data.frame(forecast.Arima(fit,h=weeks))
    a=a[,c(1,4,5)]
    a= round((a/10^6),2)
    names(a) = c('Point forecast (in millions)','Lo 95 (in millions)','Hi 95 (in millions)')
    return(a)
  }
  output$pred_sales <- renderDataTable({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        pred_sales_func(fit_country,nWeeks())
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_north,nWeeks())
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_south,nWeeks())
        }
      }
    } else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_NS1,nWeeks())
        }
      } else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_NS2,nWeeks())
        }
      } else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_SS1,nWeeks())
        }
      } else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_SS2,nWeeks())
        }
      } else if(store_wise() == "SOUTH_STORE3"){
        pred_sales_func(fit_SS3,nWeeks())
      }
    }
    
    
    
  },options=list(scrollX=TRUE))
  
  ########### ARIMA MAPE ################
  # error_metric_func = function(fit){
  #   summary_arima <- summary(fit)
  #   #describe_arima <- describe(summary_arima)
  #   #return(summary_arima)
  # }
  # 
  # 
  # output$error_metric <- renderPrint ({
  #   
  #   if(type() == "Country"){
  #     if(fcType() == "ARIMA"){
  #       
  #       error_metric_func(fit_country)
  #     }
  #   }else if(type() == "Region"){
  #     if(reg_wise() == "NORTH_REGION"){
  #       if(fcType() == "ARIMA"){
  #         
  #         error_metric_func(fit_north)
  #       }
  #       
  #     }else if(reg_wise() == "SOUTH_REGION"){
  #       if(fcType() == "ARIMA"){
  #         data_set_t1=
  #           error_metric_func(fit_south)
  #         
  #       }
  #     }
  #   }else if(type() == "Store"){
  #     if(store_wise() == "NORTH_STORE1"){
  #       if(fcType() == "ARIMA"){
  #        
  #         error_metric_func(fit_NS1)
  #       }
  #     }else if(store_wise() == "NORTH_STORE2"){
  #       if(fcType() == "ARIMA"){
  #        
  #         
  #         error_metric_func(fit_NS2)
  #       }
  #     }else if(store_wise() == "SOUTH_STORE1"){
  #       if(fcType() == "ARIMA"){
  #        
  #         
  #         error_metric_func(fit_SS1)
  #       }
  #     }else if(store_wise() == "SOUTH_STORE2"){
  #       if(fcType() == "ARIMA"){
  #         
  #         error_metric_func(fit_SS2)
  #       }
  #     }else if(store_wise() == "SOUTH_STORE3"){
  #       error_metric_func(fit_SS3)
  #       
  #     }
  #   }
  #   
  # })
  
  
  
}