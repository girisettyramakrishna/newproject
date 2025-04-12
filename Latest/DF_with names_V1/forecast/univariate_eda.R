
univariate_edaUI = function(id,label ="univariate"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    # highchartOutput(ns("high_agg"))
    br(),
    
    fluidRow(column(6,highchartOutput(ns("reg_share"))),column(6,highchartOutput(ns("Store_share"))))
    ,br(),br(),
    
    fluidRow(column(6,plotlyOutput(ns("boxplot"))),column(6,plotOutput(ns("hist"))))
    ,br(),br(),br(),
    highchartOutput(ns("per_change"))

  )
  
}

univariate_eda=function(input,output,session,timeline,exo,year1,year2,year3,type,store_wise,reg_wise){
  
  # output$high_agg <- renderHighchart({
  #   o <- aggregate(data1[,exo()], list(data1$Year), mean)
  #   highchart() %>% 
  #     hc_title(text = paste("Yearly ",exo()," for Stores")) %>% 
  #     hc_add_series(name = exo(), data = o[,2],
  #                   dataLabels = list(enabled = FALSE))%>%
  #     hc_xAxis(categories = o[,1])
  #   
  # })
  
  
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
  
}
