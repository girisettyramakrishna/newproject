summary_dataUI = function(id,label ="summary_data"){
  ns <- NS(id)
  uiOutput(ns("featureData"))
}

summary_data=function(input,output,session,datafile){
  output$featureData <- renderUI({
   #data.frame((datafile()))
    HTML(stargazer(datafile()[,c(6,23,36:37,24,14:19)], type="html"))
    
  }) 
}