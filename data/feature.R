featureUI = function(id,label ="feature"){
  ns <- NS(id)
  tagList(
    
    
    box(width = 5,selectInput(ns("imp_method"),"Select imputation method",selected = "Without_Impute",choices=c("Without_Impute","Mean","Omit","KNN")),
    dataTableOutput(ns("missing"))),
    box(width = 7,radioButtons(ns("Scale_method"),"Transform", choices=c("None","Transform"), selected ="None" , inline = FALSE,width = NULL),
        br(),br(),br(),
        uiOutput(ns("standardized_data")))
    
    )
}

feature=function(input,output,session,datafile){
  standardize <- reactive({
if(input$Scale_method=="Transform"){
    #standardizing the train data Using range method
    feature_data = datafile()[,c(6,23,36:37,24,14:19)]
    for(i in 1:ncol(feature_data)){
      feature_data[is.na(feature_data[,i]), i] <- mean(feature_data[,i], na.rm = TRUE)
    }
    feature_data<- decostand(feature_data,"range")
    feature_data = feature_data+0.001
}
else{
  feature_data = datafile()[,c(6,23,36:37,24,14:19)]
  for(i in 1:ncol(feature_data)){
    feature_data[is.na(feature_data[,i]), i] <- mean(feature_data[,i], na.rm = TRUE)
  }
  feature_data
}   
  })
  
  output$standardized_data <- renderUI({
    HTML(stargazer(standardize(), type="html"))
  
    })

  output$missing <- renderDataTable({
    
    if(input$imp_method == "Mean"){
      vara = datafile()
      for(i in 1:ncol(vara)){
        vara[is.na(vara[,i]), i] <- mean(vara[,i], na.rm = TRUE)
      }
      observeEvent(input$imp_method, {
        showModal(modalDialog(
          title = "mean Imputation",
          "Successfully imputed through Mean",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if(input$imp_method == "Omit"){
      vara = datafile()
      vara <- na.omit(vara)
      observeEvent(input$imp_method, {
        showModal(modalDialog(
          title = "Omit",
          "Successfully imputed through Omit",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if(input$imp_method == "KNN"){
      vara = datafile()
      data_cat = data.frame(vara$Date)
      names(data_cat) ="Date"
      data_num = data1[,-c(3)]
      data_num = knnImputation(data_num,k=5)
      data1 = cbind(data_cat,data_num)
      vara = data.frame(data1[,c(2,3,1,4:44)])
      observeEvent(input$imp_method, {
        showModal(modalDialog(
          title = "KNN",
          "Successfully imputed through KNN",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
    else {
      vara <- datafile() 
      observeEvent(input$imp_method, {
        showModal(modalDialog(
          title = "Without Impute",
          "Percentage of missing Values",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
    
    y = data.frame(sapply(vara[,c(6,23,34:37,24,14:19,12,13)],function(x){ (sum(is.na(x))/length(x))*100}))  
    names(y) <- c("Percentage_of_missing")
    y$Percentage_of_missing = round(y$Percentage_of_missing,2)
    return(y)   
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  }

