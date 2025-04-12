
Demand_maximizationUI <- function(id,label ="Demand_maximization"){
  ns <- NS(id)
  tagList(

      p("Click",actionButton(ns("maximize"), "Go!",icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),"to display the maximum sales of given ranges"),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                       tags$img(src = "spinner.gif",id = "loadmessage",style = "width:70px;margin-left:150px;")),
      
      fluidRow(
        column(width = 4),
        column(width = 6,
          div(style="font-size:20px;",tableOutput(ns("final_sales")))
        ),
        column(width = 2)
      ),
      
      fluidRow(
      column(width = 2),
        column(width = 8,
          dataTableOutput(ns("final_values"))
        ),
      column(width = 2)
      )
  )
}

Demand_maximization<- function(input,output,session,Sulphate_content_i,Price_sensitivity_i,Advertising_costs_i,Discount_sensitivity_i){

  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Value = as.character(c(cbind(Sulphate_content_i()[1],Sulphate_content_i()[2]), 
                             
                             cbind(Price_sensitivity_i()[1], Price_sensitivity_i()[2]),
                             cbind(Advertising_costs_i()[1], Advertising_costs_i()[2]),
                             
                             cbind(Discount_sensitivity_i()[1],Discount_sensitivity_i()[2] )
      )),
      stringsAsFactors=FALSE)
    
  }) 
  
  combinations_list <- reactive({
    sc <- seq(as.integer(sliderValues()[1,]),as.integer(sliderValues()[2,]), length.out = 5) ## Sulphate_content
    #ms <- seq(as.integer(sliderValues()[3,]),as.integer(sliderValues()[4,]), length.out = 5) ## Market_Share
    ppu <- seq(as.integer(sliderValues()[3,]),as.integer(sliderValues()[4,]), length.out = 5) ## Price_Per_Unit
    ac <- seq(as.integer(sliderValues()[5,]),as.integer(sliderValues()[6,]), length.out = 5) ## Advertising_costs
    #norl <- seq(as.integer(sliderValues()[9,]),as.integer(sliderValues()[10,]), length.out = 5) ## Number_of_Retail_Locations
    dp <- seq(as.integer(sliderValues()[7,]),as.integer(sliderValues()[8,]), length.out = 5) ## Discount_Percentage
    
    lis <- list() 
    for(i in sc){
      #for(j in ms){
        for(k in ppu){
          for(l in ac){
           # for(m in norl){
              for(n in dp){
                comb <- paste(i,k,l,n)
                lis <- append(comb,lis)
              }
           # }
          }
        }
      #}
    }
    
    
    final_list <- t(data.frame(lis))
    rownames(final_list) <- 1:nrow(final_list)
    
    
    vb <- as.data.frame(gsub(" ", ":",final_list, perl = T))
    out <- as.data.frame(strsplit(as.character(vb$V1),':'))
    combinations <- data.frame(t(out))
    rownames(combinations) <- 1:nrow(combinations)
    names(combinations) <- c(names(data1[,c(16,23,22,36)]))
    return(combinations)
  })
  
  
  sales_df <- reactive({
    cl <- combinations_list()
    
    # Ridge Equation
    outlm <- lm.ridge (as.numeric(unlist(data1[6])) ~   as.numeric(unlist(data1[16]))+
                         as.numeric(unlist(data1[23]))+ as.numeric(unlist(data1[24]))+
                         +as.numeric(unlist(data1[36])),data1,lambda = 1.605 )
    
    ## To Identify lamda
    ##MASS::select(outlm)
    
    final_sales <- list()
    for(j in 1:nrow(cl)){
      sales_pred = sum(coef(outlm)[1],
                       coef(outlm)[2]*as.numeric(as.character(cl[,1][j])),
                       coef(outlm)[3]*as.numeric(as.character(cl[,2][j])),
                       coef(outlm)[4]*as.numeric(as.character(cl[,3][j])),
                       coef(outlm)[5]*as.numeric(as.character(cl[,4][j]))
                       )
      final_sales <- append(sales_pred,final_sales)
    }
    
    final_sales_values <- t(data.frame(final_sales))
    final_sales_values <- data.frame(final_sales_values)
    rownames(final_sales_values) <- 1:nrow(final_sales_values)
    names(final_sales_values) <- c("Monthly_Total_Sales")
    final_sales_values$ind <- 1:nrow(final_sales_values)
    return(final_sales_values)
  })
  
  max_sales <- eventReactive(input$maximize,{
    sd <- sales_df()
    mts <- data.frame(max(sd$Monthly_Total_Sales))
    names(mts) <- "Maximum Total Sales"
    return(mts)
  })
  
  output$final_sales <- renderTable({
   max_sales()
  })

  opt_equation <- eventReactive(input$maximize,{
    sal <- sales_df()
    comb_li <- combinations_list()
    df <- cbind(sal,comb_li)
    j = 0
    for(i in df$Monthly_Total_Sales){
      if(max(df$Monthly_Total_Sales) == i){
        equ <- cbind(df[j,][3],df[j,][4],df[j,][5],df[j,][6])
        break;
      }
      j = j + 1
    }
    trans <- data.frame(t(data.frame(equ)))
    rownames(trans) <- c(names(data1[,c(16,23,22,36)]))
    names(trans) <- "Optimal Value"
    return(trans)
  })

  output$final_values <- renderDataTable({
    # opt_equation()
    oe <- opt_equation()
    row.names(oe) <- c("Sulphate Content","Price per unit","Marketing Expenditure","Discount Percentage")
    return(oe)
  },options = list(dom = 't'))
  
}