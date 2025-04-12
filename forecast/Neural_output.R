Neural_outputUI = function(id,label ="Neural_output"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    highchartOutput(ns("nn_forecast")),
    dataTableOutput(ns("pred_sales_table1"))
  )}




Neural_output=function(input,output,session,nnType,noWeeks,type,store_wise,reg_wise){
  
  
  
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
  
  
  ########  ARIMA forecast highchart #############
  hc_arima1_graph = function(series2,xAxis){
    hc1=highchart() %>% 
      hc_title(text = "Monthly Sales for stores") %>% 
      hc_add_series(name = "Predicted  Sales", data = series2,
                    dataLabels = list(enabled = FALSE))%>%
      hc_xAxis(categories = xAxis)
    return(hc1)
  }
  
  plot_nn_f = function(data_procedureA_country,weeks){
    decom =decompose(data_procedureA_country)
    sum(is.na(decom))
    random = data_procedureA_country - decom$seasonal
    r_df = data.frame(random)
    fit <- nnetar((na.omit(r_df$random)),lambda=0)
   
    predicted <- predict(fit,h = noWeeks() , prediction.interval = TRUE)
    
    all <- predicted$mean
    all1 = as.data.frame(all)
    all1=cbind(index(all),all)
    all1=as.data.frame(all1)
    
    year=all1$`index(all)`
    for(i in index(year))
      if(i %in% c(2,14,26,38)){
        year[i] = year[i]+0.02
      }
    f <- format(date_decimal(year), "%b %Y")
    all1=cbind(f,all1)
    hc_arima1_graph(all1$all,all1$f)
  }
  
  for_table = function(data_procedureA_country,weeks){
    decom =decompose(data_procedureA_country)
    sum(is.na(decom))
    random = data_procedureA_country - decom$seasonal
    r_df = data.frame(random)
    fit <- nnetar((na.omit(r_df$random)),lambda=0)
    a=as.data.frame(forecast.nnetar(fit,h=noWeeks()))
    a = t(a)
    names(a) = "Point forecast (in millions)"
    return(a)
  }
  
  # pred_sales_func = function(fit,weeks){
  #   
  # }
  ################ forecast plot ###########
  output$nn_forecast <- renderHighchart({
    if(type() == "Country"){
      if(nnType() == "Perceptron"){
        plot_nn_f(data_procedureA_country,noWeeks())
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_north,noWeeks())
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_south,noWeeks())
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_NS1,noWeeks())
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_NS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_SS1,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(nnType() == "Perceptron"){
          plot_nn_f(data_procedureA_SS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        plot_nn_f(data_procedureA_SS3,noWeeks())
      }
    }
    
  })
  
  
  output$pred_sales_table1 <- renderDataTable({
    if(type() == "Country"){
      if(nnType() == "Perceptron"){
        for_table(data_procedureA_country,noWeeks())
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_north,noWeeks())
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_south,noWeeks())
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_NS1,noWeeks())
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_NS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_SS1,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(nnType() == "Perceptron"){
          for_table(data_procedureA_SS2,noWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        for_table(data_procedureA_SS3,noWeeks())
      }
    }
    
  })
}