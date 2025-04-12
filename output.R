error_outputUI <- function(id,label ="error_output"){
  ns <- NS(id)
  tabsetPanel(
    tabPanel(title = "Univariate",
             fluidRow(
               column(width = 1,""),
               column(width = 10,
                      dataTableOutput(ns("error_metric"))),
               column(width = 1,"")
             )
             ),
    # tabPanel(title = "Bivariate",
    #          fluidRow(
    #            column(width = 1,""),
    #            column(width = 10,dataTableOutput(ns("uni")),
    #                   column(width = 1,"")
    #            )
    #          )
    #          ),
    tabPanel(title = "Multivariate",
             fluidRow(
               column(width = 1,""),
               column(width = 10,dataTableOutput(ns("multi_e")),
                      column(width = 1,"")
               )
             )
             ),
    
    tabPanel(title= "Data Profiling Report",  uiOutput("Data_profiling_rep"))
  )
  
}

error_output <- function(input,output,session){
 output$error_metric <- renderDataTable({
   var <- read.csv("error.csv")
   return(var)
 },options=list(scrollX=TRUE))
 
 output$Data_profiling_rep <- renderUI({
   htmlTemplate("myoutputfile.html")
   
 })
 
 output$uni <- renderDataTable({
   var1 <- read.csv("uni_error.csv")
   return(var1)
 },options=list(scrollX=TRUE))
 
 output$multi_e <- renderDataTable({
   var2 <- read.csv("multi.csv")
   return(var2)
 },options=list(scrollX=TRUE))
 
}