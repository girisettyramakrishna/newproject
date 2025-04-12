bivariate_edaUI= function(id,label="bivariate_eda"){
  ns<- NS(id)
  tagList(
   fluidRow(
     column(6,
            highchartOutput(ns("distplot1")),
            verbatimTextOutput(ns("COR1"))
            ),
     column(6, 
            highchartOutput(ns("distplot2")),
            verbatimTextOutput(ns("COR2"))
            )
   )
  )
}

bivariate_eda = function(input,output,session,timeline,year1,year2,year3,External,Internal,type,store_wise,reg_wise){

  colm1 <- reactive({
    as.numeric(year1())
  })
  colm2 <- reactive({
    as.numeric(year2())
  })
  colm3 <- reactive({
    as.numeric(year3())
  })

  hc_int = function(x,y,factor){
    hc1 = highchart() %>%
      hc_title(text = paste("Sales Vs " ,factor)) %>%
      hc_add_series_scatter(x,y)%>%
      hc_xAxis(title = list(text = factor))%>%
      hc_yAxis(title = list(text = "Sales"))
    return(hc1)
  }

  output$COR1 <- renderPrint({

    if(type() == "Country"){
      data2 = subset(data1,data1[,1] == input$Year)
      external1 = External()
      if(timeline() == "Yearly"){
        data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
        correlation = cor(data[,External()],data$Monthly_sales_total)
      }
      else {
        data = data1[data1[,1] == colm3(),]
        correlation = cor(data[,External()],data$Monthly_sales_total)
      }
    } else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$NORTH_REGION)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$NORTH_REGION)
        }
      } else if(reg_wise() == "SOUTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$SOUTH_REGION)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$SOUTH_REGION)
        }
      }
    } else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$NORTH_STORE1)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$NORTH_STORE1)
        }

      }else if(store_wise() == "NORTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$NORTH_STORE2)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$NORTH_STORE2)
        }

      }else if(store_wise() == "SOUTH_STORE1"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$SOUTH_STORE1)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$SOUTH_STORE1)
        }

      }else if(store_wise() == "SOUTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$SOUTH_STORE2)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$SOUTH_STORE1)
        }

      }else{

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation = cor(data[,External()],data$SOUTH_STORE3)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation = cor(data[,External()],data$SOUTH_STORE3)

        }
      }
    }
    cat("The CORRELATION between them is",correlation)
  })
  output$COR2 <- renderPrint({


    if(type() == "Country"){
      data2 = subset(data1,data1[,1] == input$Year)

      if(timeline() == "Yearly"){
        data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
        correlation1 = cor(data[,Internal()],data$Monthly_sales_total)
      }
      else {
        data = data1[data1[,1] == colm3(),]
        correlation1 = cor(data[,Internal()],data$Monthly_sales_total)
      }
    } else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$NORTH_REGION)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$NORTH_REGION)
        }
      } else if(reg_wise() == "SOUTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_REGION)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_REGION)
        }
      }
    } else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$NORTH_STORE1)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$NORTH_STORE1)
        }

      }else if(store_wise() == "NORTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$NORTH_STORE2)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$NORTH_STORE2)
        }

      }else if(store_wise() == "SOUTH_STORE1"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE1)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE1)
        }

      }else if(store_wise() == "SOUTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE2)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE2)
        }

      }else{

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE3)
        }
        else {
          data = data1[data1[,1] == colm3(),]
          correlation1 = cor(data[,Internal()],data$SOUTH_STORE3)
        }
      }
    }
    cat("The CORRELATION between them is",correlation1)

  })

  output$distplot2 <- renderHighchart({

    if(type() == "Country"){
      data2 = subset(data1,data1[,1] == input$Year)

      if(timeline() == "Yearly"){
        data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
        hc_int(data[,Internal()],data$Monthly_sales_total,Internal())
      }
      else {
        data = data1[data1[,1] == colm3(),]
        hc_int(data[,Internal()],data$Monthly_sales_total,Internal())
      }
    } else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$NORTH_REGION,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$NORTH_REGION,Internal())
        }
      } else if(reg_wise() == "SOUTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$SOUTH_REGION,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$SOUTH_REGION,Internal())
        }
      }
    } else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$NORTH_STORE1,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$NORTH_STORE1,Internal())
        }

      }else if(store_wise() == "NORTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$NORTH_STORE2,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$NORTH_STORE2,Internal())
        }

      }else if(store_wise() == "SOUTH_STORE1"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$SOUTH_STORE1,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$SOUTH_STORE1,Internal())
        }

      }else if(store_wise() == "SOUTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$SOUTH_STORE2,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$SOUTH_STORE2,Internal())
        }

      }else{

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,Internal()],data$SOUTH_STORE3,Internal())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,Internal()],data$SOUTH_STORE3,Internal())
        }
      }
    }

  })



  output$distplot1 <- renderHighchart({


    if(type() == "Country"){
      data2 = subset(data1,data1[,1] == input$Year)
      external1 = External()
      if(timeline() == "Yearly"){
        data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
        hc_int(data[,External()],data$Monthly_sales_total,External())
      }
      else {
        data = data1[data1[,1] == colm3(),]
        hc_int(data[,External()],data$Monthly_sales_total,External())
      }
    } else if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$NORTH_REGION,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$NORTH_REGION,External())
        }
      } else if(reg_wise() == "SOUTH_REGION"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$SOUTH_REGION,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$SOUTH_REGION,External())
        }
      }
    } else if(type() == "Store"){
      if(store_wise() == "NORTH_STORE1"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$NORTH_STORE1,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$NORTH_STORE1,External())
        }

      }else if(store_wise() == "NORTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$NORTH_STORE2,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$NORTH_STORE2,External())
        }

      }else if(store_wise() == "SOUTH_STORE1"){
        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$SOUTH_STORE1,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$SOUTH_STORE1,External())
        }

      }else if(store_wise() == "SOUTH_STORE2"){

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$SOUTH_STORE2,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$SOUTH_STORE2,External())
        }

      }else{

        data2 = subset(data1,data1[,1] == input$Year)
        external1 = External()
        if(timeline() == "Yearly"){
          data = data1[data1[,1] >= colm1() & data1[,1] <= colm2(),]
          hc_int(data[,External()],data$SOUTH_STORE3,External())
        }
        else {
          data = data1[data1[,1] == colm3(),]
          hc_int(data[,External()],data$SOUTH_STORE3,External())
        }
      }
    }

  })
}