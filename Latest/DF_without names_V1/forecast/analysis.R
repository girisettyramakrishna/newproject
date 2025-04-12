analysisUI = function(id,label ="analysis"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    fluidRow(column(6,highchartOutput(ns("reg_share"))),column(6,highchartOutput(ns("Store_share"))))
    ,br(),br(),
    
    fluidRow(column(6,plotlyOutput(ns("boxplot"))),column(6,plotOutput(ns("hist"))))
    ,br(),br(),br(),
    highchartOutput(ns("per_change")),
    
    
    highchartOutput(ns("models")),
    tableOutput(ns("error_metric")),
    fluidRow(
      column(width = 4,plotOutput(ns("decompose_seasonal"))),
      column(width = 4,plotOutput(ns("decompose_trend"))),
      column(width = 4,plotOutput(ns("decompose_random")))
    ),
    br(),br(),
    div("Seasonality",style="font-size:20px;font-style:bold;"),
    tableOutput(ns("summary_seasonal")),
    div("Trend",style="font-size:20px;font-style:bold;"),
    tableOutput(ns("summary_trend")),
    div("Randomness",style="font-size:20px;font-style:bold;"),
    tableOutput(ns("summary_random")),hr(),
    
    
    div(h3("Seasonality")),hr(),
    plotOutput(ns("dec_grap")),hr(),
    div(h3("Variance(%)")),hr(),
    div(img(id = "img_variance",src="variance.png")),  
    div(id = "box_variance",h1(textOutput(ns("variance")))),
    br(),br()
    
          )}
    
data_arima = data1[,c(1,2,6,38:44)]
data_arima$date <- as.yearmon(paste(data_arima$Year, data_arima$Month), "%Y %m")
data_arima = data_arima[order(as.Date(data_arima$date, format="%Y %m")),]
data_arima=data_arima[,-c(1,2)]

######################
##country wise########
data_arima_country= data.frame(data_arima[,c(1)])
names(data_arima_country)=c('Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_country = ts((data_arima_country$`Monthly Sales`),frequency =12 )
names(data_procedureA_country) <- c("Monthly Sales")

fit_country = arima(data_procedureA_country,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

#######################
##north region wise#######
data_arima_north=data_arima[,c(9,2)]
names(data_arima_north)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_north = ts((data_arima_north$`Monthly Sales`),frequency =12 )
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
data_procedureA_south = ts((data_arima_south$`Monthly Sales`),frequency =12 )
names(data_procedureA_south) <- "Monthly Sales"

fit_south = arima(data_procedureA_south,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

##########################
##north_store1########
data_arima_NS1 =data_arima[,c(9,4)]
names(data_arima_NS1)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_NS1 = ts((data_arima_NS1$`Monthly Sales`),frequency =12 )
names(data_procedureA_NS1) <- "Monthly Sales"

fit_NS1 = arima(data_procedureA_NS1,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

###########################
##north_store2#########
data_arima_NS2=data_arima[,c(9,5)]
names(data_arima_NS2)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_NS2 = ts((data_arima_NS2$`Monthly Sales`),frequency =12 )
names(data_procedureA_NS2) <- "Monthly Sales"

fit_NS2 = arima(data_procedureA_NS2,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

###########################
## south_store_1##########
data_arima_SS1=data_arima[,c(9,6)]
names(data_arima_SS1)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_SS1 = ts((data_arima_SS1$`Monthly Sales`),frequency =12 )
names(data_procedureA_SS1) <- "Monthly Sales"

fit_SS1 = arima(data_procedureA_SS1,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

##########################
## south_store_2##########
data_arima_SS2=data_arima[,c(9,7)]
names(data_arima_SS2)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_SS2 = ts((data_arima_SS2$`Monthly Sales`),frequency =12 )
names(data_procedureA_SS2) <- "Monthly Sales"

fit_SS2 = arima(data_procedureA_SS2,order = c(0,1,1), seasonal = list(order = c(0,1,2)))

#########################
## south_store_3#########
data_arima_SS3=data_arima[,c(9,8)]
names(data_arima_SS3)=c('Date','Monthly Sales')

### CONVERSION TO TIME SERIES DATA
data_procedureA_SS3 = ts((data_arima_SS3$`Monthly Sales`),frequency =12 )
names(data_procedureA_SS3) <- "Monthly Sales"

fit_SS3 = arima(data_procedureA_SS3,order = c(0,1,1), seasonal = list(order = c(0,1,2)))


    
    
analysis=function(input,output,session,timeline,exo,year1,year2,year3,fcType,nWeeks,type,store_wise,reg_wise,seasonality){
    
  
  output$reg_share =renderHighchart({
    North_reg = data.frame(sum(data1[38]))
    South_reg = data.frame(sum(data1[39]))
    region_share = cbind(North_reg,South_reg)
    names(region_share) = c("North_Region","South_Region")
    region_share = data.frame(t(region_share))
    region_share$Region = rownames(region_share)
    names(region_share) = c("Sales","Region")
    region_share$Sales = round(region_share$Sales,0)
    df <- setNames(region_share, c("value", "name"))
    ds <- list_parse(df)
    colors <- colorize(1:3)
    highchart() %>% 
      hc_title(text = "Region-wise Sales") %>% 
      hc_add_series(data = ds, type = "treemap", color='#3366AA') 
    
  }) 
  
  output$Store_share = renderHighchart({
    NORTH_STO1 = data.frame(sum(data1[40]))
    NORTH_STO2 = data.frame(sum(data1[41]))
    SOUTH_STO1 = data.frame(sum(data1[42]))
    SOUTH_STO2 = data.frame(sum(data1[43]))
    SOUTH_STO3 = data.frame(sum(data1[44]))
    store_share = cbind(NORTH_STO1,NORTH_STO2,SOUTH_STO1,SOUTH_STO2,SOUTH_STO3)
    names(store_share) = c("NORTH_STORE1","NORTH_STORE2","SOUTH_STORE1","SOUTH_STORE2","SOUTH_STORE3")
    store_share = data.frame(t(store_share))
    store_share$Store = rownames(store_share)
    names(store_share) = c("Sales","Store")
    store_share$Sales = round(store_share$Sales,0)
    df <- setNames(store_share, c("value", "name"))
    ds <- list_parse(df)
    colors <- colorize(1:5)
    highchart() %>% 
      hc_title(text = "Store-wise Sales") %>% 
      hc_add_series(data = ds, type = "treemap", color='#357EC7') 
    
  }) 
  
  
  output$boxplot =renderPlotly({
    plot_ly(y = data1[,exo()], type = "box", boxpoints = "all", jitter = 0.3,
            pointpos = 0)%>%
      layout(title = "Box plot", 
             xaxis = list( title = exo(),  showgrid = F),      
             yaxis = list(title = "")
             
      )
    
  })
  
  # output$box_stats = render
  
  
  output$hist = renderPlot({
    
    if (data1[,exo()] == data1$NORTH_REGION){
      data1[,exo()]  <- data1[,exo()]/10
    }else{
      data1[,exo()] <- data1[,exo()]
    }
    
    d <- data1[,exo()]
    plot(d, main="Density curve")
    polygon(d, border="blue")
    hist(d,probability = T, col=rgb(0,0.6,1),main = "Histogram",xlab = "")# prob=TRUE for probabilities not counts
    #lines(density(d), col="blue", lwd=2)
    abline(v=mean((data1[,exo()]),col="red",lty = 2))
    text(mean(data1[,exo()]),max(density(data1[,exo()])$y),"mean")
    abline(v=(mean(data1[,exo()])+2*sd(data1[,exo()])),col="red",lty = 2)
    text(mean(data1[,exo()])+2*sd(data1[,exo()]),max(density(data1[,exo()])$y),paste("mean+2*sd",round(mean(data1[,exo()])+2*sd(data1[,exo()]),2),sep = "="))
    abline(v=(mean(data1[,exo()])-2*sd(data1[,exo()])),col="red",lty = 2)
    text(mean(data1[,exo()])-2*sd(data1[,exo()]),max(density(data1[,exo()])$y),paste("mean-2*sd",round(mean(data1[,exo()])-2*sd(data1[,exo()]),2),sep = "="))
    
    
    # m <- ggplot(data1, aes(x= data1[,exo()])) +
    #   geom_histogram(aes(y = ..density..),fill = rgb(0,0.6,1)) + 
    #   geom_density(color = "blue") +
    #   xlab(exo())
    # 
    # y = m + theme(panel.background = element_rect(fill = 'white'))
    # return(y)
    
    
  })
  
  
  
  
  output$per_change =renderHighchart({
    
    perc_change = aggregate(data1[,exo()]~data1[,3],FUN = sum)
    names(perc_change)=c("Date","Sales")
    n = nrow(perc_change)
    perc_change$Date <- as.Date( perc_change$Date , format = "%d/%m/%Y" )
    perc_change=perc_change[order(as.Date(perc_change$Date,format="%d/%m/%Y")),,drop=FALSE]
    x <- data.frame(c(NA,(diff(perc_change$Sales) / head(perc_change$Sales,n-1))*100))
    names(x) = "percentage change"
    percentage_change = data.frame(x$`percentage change`)
    percentage_change$Date = perc_change$Date
    library(xts)
    pc_MONTH = xts(round(percentage_change$x..percentage.change.,3),order.by=percentage_change$Date)
    highchart(type = "stock") %>% 
      hc_add_series(pc_MONTH, id = "percentage change - Month") %>% 
      hc_title(text = paste0("Percentage change in Sales per Month"))
  }) 
  
  
  colm1 <- reactive({
    as.numeric(year1())
  })
  colm2 <- reactive({
    as.numeric(year2())
  })
  colm3 <- reactive({
    as.numeric(year3())
  })
 
  ########  ARIMA forecast highchart #############
  hc_arima = function(series1,series2,series3,xAxis){
    hc1=highchart() %>% 
      hc_title(text = "Monthly Sales for Stores") %>% 
      hc_add_series(name = "Monthly Sales", data = series1,
                    dataLabels = list(enabled = FALSE))%>%
      hc_add_series(name = "Predicted  Sales", data = series2,
                    dataLabels = list(enabled = FALSE))%>%
      hc_add_series(name = "Fitted Sales", data = series3,
                    dataLabels = list(enabled = FALSE))%>%
      hc_xAxis(categories = xAxis)
    return(hc1)
  }
  
  ########  ARIMA forecast highchart #############
  hc_arima1 = function(series2,xAxis){
    hc1=highchart() %>% 
      hc_title(text = "Monthly Sales for stores") %>% 
      hc_add_series(name = "Predicted  Sales", data = series2,
                    dataLabels = list(enabled = FALSE))%>%
      hc_xAxis(categories = xAxis)
    return(hc1)
  }
  ######## decompose tbats ####################
  decomp = function(data,x,y){
    
    if(y==1){
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 4 )
      
    }
    else if(y ==2){
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 6 )
      
    }else{
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 12 )
      
    }
    
    procedureA_decomp = decompose(data_procedureA)
    
    # check seasonality
    # seasonal_effect <- tbats(data_procedureA)
    # seasonal <- !is.null(seasonal_effect$seasonal)
    #seasonal
    
    #plot seasonality, trend
    if(x == 1){
      autoplot(procedureA_decomp$seasonal,main="Seasonal component",ylab="Seasonal")
    } else if(x==2){
      autoplot(procedureA_decomp$trend,main="Trend component",ylab="Trend")
    } else if(x == 3){
      autoplot(procedureA_decomp$random,main="Error component",ylab="Error")
      
    } 
  }
  
  ######### plot one part of seasonality##############
  
  ######## decompose tbats ####################
  season_one = function(data,y){
    
    if(y==1){
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 4 )
      procedureA_decomp = decompose(data_procedureA)
      qtr_part = procedureA_decomp$seasonal
      one_part =qtr_part[1:3]
    }
    else if(y ==2){
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 6 )
      procedureA_decomp = decompose(data_procedureA)
      half_part = procedureA_decomp$seasonal
      one_part =half_part[1:6]
      
    }else{
      data_procedureA =  ts((data$`Monthly Sales`),frequency = 12 )
      procedureA_decomp = decompose(data_procedureA)
      year_part = procedureA_decomp$seasonal
      one_part = year_part[1:12]
      
    }
   
      plot(one_part,main="Seasonal component",ylab="Seasonal", type = "l")
   
  }
  
  
  ######## summary decompose tbats ####################
  decomp_summary = function(data_arima_SS2,x,y){
    if(y==1){
      data_procedureA =  ts((data_arima_SS2$`Monthly Sales`),frequency = 4 )

    }
      else if(y ==2){
        data_procedureA =  ts((data_arima_SS2$`Monthly Sales`),frequency = 6 )
        
      }else{
        data_procedureA =  ts((data_arima_SS2$`Monthly Sales`),frequency = 12 )
        
      }
    
    procedureA_decomp = decompose(data_procedureA)
    
    #summary seasonality, trend
    if(x == 1){
      df= t(data.frame(unclass(summary(procedureA_decomp$seasonal))))
    } else if(x==2){
      df= t(data.frame(unclass(summary(procedureA_decomp$trend))))
      
    } else if(x==3){
      df= t(data.frame(unclass(summary(procedureA_decomp$random))))
      
    }
  }
  
  
  ##### variance decompose ###############
  variance_decompose = function(data_v,x){
    if(x==1){
      # quarterly <- aggregate(data_procedureA_country, nfrequency=4)
      # qtl =decompose(quarterly)
      # se= qtl$seasonal
      data_procedureA =  ts((data_v$`Monthly Sales`),frequency = 4 )
      procedureA_decomp = decompose(data_procedureA)
      se = procedureA_decomp$seasonal
    }else if(x == 2){
      # half_yearly <- aggregate(data_procedureA_country, nfrequency=6)
      # qtl =decompose(half_yearly)
      # se= qtl$seasonal
      data_procedureA =  ts((data_v$`Monthly Sales`),frequency = 6 )
      procedureA_decomp = decompose(data_procedureA)
      se = procedureA_decomp$seasonal
    }else{
      # qtl =decompose(data_procedureA_country)
      # se= qtl$seasonal
      data_procedureA =  ts((data_v$`Monthly Sales`),frequency = 12 )
      procedureA_decomp = decompose(data_procedureA)
      se = procedureA_decomp$seasonal
    }
  
    ob_trend <- data_procedureA - procedureA_decomp$trend
    ob_trend_t <- na.omit(ob_trend)
    
    upper<- max(se) - min(se)
    low <- max(ob_trend_t) - min(ob_trend_t)
    variance <- (upper/low)*100
  }
  
  ########### ARIMA MAPE ################

  
  output$error_metric <- renderTable({
  if(type() == "Country"){
      if(fcType() == "ARIMA"){
        d  <- accuracy(fit_country)
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_north)
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_south)
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_NS1)
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_NS2)
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_SS1)
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          d <- accuracy(fit_SS2)
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        d <- accuracy(fit_SS3)
      }
    }
    return(d)
  })
  
  

  ######### variance value ########
  output$variance = renderText({
      if(type() == "Country"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
           paste(round(variance_decompose(data_arima_country,1),2),"%")
          }else if(seasonality() == "Half-Yearly"){
            paste(round(variance_decompose(data_arima_country,2),2),"%")
          }else if(seasonality() == "Yearly"){
            paste(round(variance_decompose(data_arima_country,3),2),"%")
          }
        }
      }else if(type() == "Region"){
        if(reg_wise() == "NORTH_REGION"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_north,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_north,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_north,3),2),"%")
            }
          }
        }else if(reg_wise() == "SOUTH_REGION"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_south,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_south,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_south,3),2),"%")
            }
          }
        }
      }else if(type() == "Store"){
        if(store_wise() == "NORTH_STORE1"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_NS1,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_NS1,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_NS1,3),2),"%")
            }
          }
        }else if(store_wise() == "NORTH_STORE2"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_NS2,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_NS2,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_NS2,3),2),"%")
            }
          }
        }else if(store_wise() == "SOUTH_STORE1"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_SS1,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_SS1,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_SS1,3),2),"%")
            }
          }
        }else if(store_wise() == "SOUTH_STORE2"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_SS2,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_SS2,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_SS2,3),2),"%")
            }
          }
        }else if(store_wise() == "SOUTH_STORE3"){
          if(fcType() == "ARIMA"){
            if(seasonality() == "Quarterly"){
              paste(round(variance_decompose(data_arima_SS3,1),2),"%")
            }else if(seasonality() == "Half-Yearly"){
              paste(round(variance_decompose(data_arima_SS3,2),2),"%")
            }else if(seasonality() == "Yearly"){
              paste(round(variance_decompose(data_arima_SS3,3),2),"%")
            }
          }
        }
      }
    })
  
  
  ########## random summary ###################
  output$summary_random = renderTable({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){

          decomp_summary(data_arima_country,3,1)
        }else if(seasonality() == "Half-Yearly"){

          decomp_summary(data_arima_country,3,2)
          
        }else if(seasonality() == "Yearly"){
          decomp_summary(data_arima_country,3,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_north,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_north,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_north,3,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_south,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_south,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_south,3,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS1,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS1,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS1,3,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS2,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS2,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS2,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS1,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS1,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS1,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS2,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS2,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS2,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS3,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS3,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS3,3,3)
          }
        }
      }
    }
  })  ########### seasonal summary ###################
  
  output$summary_seasonal = renderTable({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          
          decomp_summary(data_arima_country,1,1)
        }else if(seasonality() == "Half-Yearly"){
          
          decomp_summary(data_arima_country,1,2)
          
        }else if(seasonality() == "Yearly"){
          decomp_summary(data_arima_country,1,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_north,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_north,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_north,1,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_south,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_south,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_south,1,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS1,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS1,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS1,1,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS2,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS2,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS2,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS1,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS1,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS1,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS2,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS2,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS2,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS3,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS3,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS3,1,3)
          }
        }
      }
    }
  })
  
  ####### trend SUmmmary ###################
  output$summary_trend = renderTable({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          
          decomp_summary(data_arima_country,2,1)
        }else if(seasonality() == "Half-Yearly"){
          
          decomp_summary(data_arima_country,2,2)
          
        }else if(seasonality() == "Yearly"){
          decomp_summary(data_arima_country,2,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_north,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_north,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_north,2,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_south,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_south,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_south,2,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS1,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS1,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS1,2,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_NS2,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_NS2,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_NS2,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS1,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS1,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS1,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS2,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS2,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS2,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp_summary(data_arima_SS3,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp_summary(data_arima_SS3,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp_summary(data_arima_SS3,2,3)
          }
        }
      }
    }
  })
  
  output$decompose_seasonal = renderPlot({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          
          decomp(data_arima_country,1,1)
        }else if(seasonality() == "Half-Yearly"){
          
          decomp(data_arima_country,1,2)
          
        }else if(seasonality() == "Yearly"){
          decomp(data_arima_country,1,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_north,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_north,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_north,1,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_south,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_south,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_south,1,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS1,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS1,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS1,1,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS2,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS2,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS2,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS1,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS1,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS1,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS2,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS2,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS2,1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS3,1,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS3,1,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS3,1,3)
          }
        }
      }
    }
  })
  
  output$decompose_trend = renderPlot({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          
          decomp(data_arima_country,2,1)
        }else if(seasonality() == "Half-Yearly"){
          
          decomp(data_arima_country,2,2)
          
        }else if(seasonality() == "Yearly"){
          decomp(data_arima_country,2,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_north,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_north,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_north,2,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_south,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_south,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_south,2,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS1,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS1,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS1,2,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS2,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS2,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS2,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS1,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS1,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS1,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS2,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS2,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS2,2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS3,2,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS3,2,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS3,2,3)
          }
        }
      }
    }
  })
  
  output$decompose_random = renderPlot({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          
          decomp(data_arima_country,3,1)
        }else if(seasonality() == "Half-Yearly"){
          
          decomp(data_arima_country,3,2)
          
        }else if(seasonality() == "Yearly"){
          decomp(data_arima_country,3,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_north,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_north,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_north,3,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_south,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_south,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_south,3,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS1,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS1,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS1,3,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_NS2,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_NS2,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_NS2,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS1,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS1,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS1,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS2,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS2,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS2,3,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            decomp(data_arima_SS3,3,1)
            
          }else if(seasonality() == "Half-Yearly"){
            decomp(data_arima_SS3,3,2)
            
          }else if(seasonality() == "Yearly"){
            decomp(data_arima_SS3,3,3)
          }
        }
      }
    }
  })
  
  output$models <- renderHighchart({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        predicted <- predict(fit_country, n.ahead = nWeeks(), prediction.interval = TRUE)
        all <- cbind(data_procedureA_country, predicted$pred,fitted(fit_country))
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
        f <- format(date_decimal(year), "%b %Y")
        all1=cbind(f,all1)
        
        hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_country)`,all1$f)
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_north, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_north, predicted$pred,fitted(fit_north))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_north)`,all1$f)
          
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_south, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_south, predicted$pred,fitted(fit_south))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_south)`,all1$f)
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_NS1, predicted$pred,fitted(fit_NS1))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_NS1)`,all1$f)
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_NS2, predicted$pred,fitted(fit_NS2))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_NS2)`,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_SS1, predicted$pred,fitted(fit_SS1))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_SS1)`,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(data_procedureA_SS2, predicted$pred,fitted(fit_SS2))
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_SS2)`,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        predicted <- predict(fit_SS3, n.ahead = nWeeks(), prediction.interval = TRUE)
        all <- cbind(data_procedureA_SS3, predicted$pred,fitted(fit_SS3))
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
        f <- format(date_decimal(year), "%b %Y")
        all1=cbind(f,all1)
        
        hc_arima(all1$all.data_procedureA,all1$`all.predicted$pred`,all1$`all.fitted(fit_SS3)`,all1$f)
      }
    }
    
  })
  
  ################ forecast plot ###########
  output$forecast_plot <- renderHighchart({
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        predicted <- predict(fit_country, n.ahead = 5, prediction.interval = TRUE)
        all <- cbind(predicted$pred)
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
        f <- format(date_decimal(year), "%b %Y")
        all1=cbind(f,all1)
        
        hc_arima1(all1$all,all1$f)
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_north, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
        
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_south, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_NS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS1, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          predicted <- predict(fit_SS2, n.ahead = nWeeks(), prediction.interval = TRUE)
          all <- cbind(predicted$pred)
          all1 = as.data.frame(all)
          all1=cbind(index(all),all)
          all1=as.data.frame(all1)
          
          year=all1$`index(all)`
          f <- format(date_decimal(year), "%b %Y")
          all1=cbind(f,all1)
          
          hc_arima1(all1$all,all1$f)
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        predicted <- predict(fit_SS3, n.ahead = nWeeks(), prediction.interval = TRUE)
        all <- cbind(predicted$pred)
        all1 = as.data.frame(all)
        all1=cbind(index(all),all)
        all1=as.data.frame(all1)
        
        year=all1$`index(all)`
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
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_NS1,nWeeks())
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_NS2,nWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_SS1,nWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          pred_sales_func(fit_SS2,nWeeks())
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        pred_sales_func(fit_SS3,nWeeks())
      }
    }
    
    
    
  },options=list(scrollX=TRUE))
  
  output$dec_grap <- renderPlot({
    
    if(type() == "Country"){
      if(fcType() == "ARIMA"){
        if(seasonality() == "Quarterly"){
          season_one(data_arima_country,1)
        }else if(seasonality() == "Half-Yearly"){
          season_one(data_arima_country,2)
        }else if(seasonality() == "Yearly"){
          season_one(data_arima_country,3)
        }
      }
    }else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_north,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_north,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_north,3)
          }
          
        }
      }else if(reg_wise() == "SOUTH_REGION"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_south,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_south,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_south,3)
          }
        }
      }
    }else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_NS1,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_NS1,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_NS1,3)
          }
        }
      }else if(store_wise() == "NORTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_NS2,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_NS2,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_NS2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE1"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_SS1,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_SS1,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_SS1,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE2"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_SS2,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_SS2,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_SS2,3)
          }
        }
      }else if(store_wise() == "SOUTH_STORE3"){
        if(fcType() == "ARIMA"){
          if(seasonality() == "Quarterly"){
            season_one(data_arima_SS3,1)
          }else if(seasonality() == "Half-Yearly"){
            season_one(data_arima_SS3,2)
          }else if(seasonality() == "Yearly"){
            season_one(data_arima_SS3,3)
          }
        }
      }
    }
    
  })
  
  
  
}