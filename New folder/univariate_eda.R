
univariate_edaUI = function(id,label ="univariate"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
 # highchartOutput(ns("high_agg")),
  plotlyOutput(ns("boxplot")),
  plotOutput(ns("hist"))

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
  
  
  output$boxplot =renderPlotly({
    plot_ly(y = ~data1[,exo()], type = "box", boxpoints = "all", jitter = 0.3,
            pointpos = 0)%>%
      layout(title = "Box plot", 
             xaxis = list( title = "  ",  showgrid = F),      
             yaxis = list(title = "Factor")     
      )
  })

 # output$box_stats = render
  output$hist = renderPlot({
    m <- ggplot(data1, aes(x= data1[,exo()])) +
      geom_histogram(aes(y = ..density..)) + 
      geom_density() +
      xlab(exo())
    return(m)
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
