tableUI = function(id,label ="table"){
  ns <- NS(id)
  tagList(
    #csvFileInput(ns("datafile"), "User data (.csv format)"),
    # div("Data", style="padding-left:200px;"),
    dataTableOutput(ns("table")),
    # div("Summary", style="padding-left:200px;"),
    dataTableOutput(ns("sum_data"))
  )
  
}

table = function(input,output,session,datafile){
  #callModule(dataset,"dataset")
  # datafile <- callModule(csvFile, "datafile",
  #                        stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    df = datafile()[,c(6,38:44,23,36,34,37,24,35,14:19,12,13)]
    # input_data = read.csv("file:///D:/Sagarika_complete_data/yash/DF_new 18th OCT/Product_data.csv")
    # input_data = input_data[,c(6,38:44,23,36,34,37,24,35,14:19,12,13)]
    names(df)[1] = "Net Dubai Monthly Sales"
    names(df)[2] = "Bur Dubai Region"
    names(df)[3] = "Al Karama Region"
    names(df)[4] = "Al Raffa Store"
    names(df)[5] = "Al Fahidi Store"
    names(df)[6] = "Al Manama Store"
    names(df)[7] = "Talal Store "
    names(df)[8] = "Al Madina Store"
    df
  }, options = list(scrollX = TRUE))
  
  output$sum_data <- renderDataTable({
    input_data =  datafile()[,c(6,38:44,23,36,34,37,24,35,14:19,12,13)]
    names(input_data)[1] = "Net Dubai Monthly Sales"
    names(input_data)[2] = "Bur Dubai Region"
    names(input_data)[3] = "Al Karama Region"
    names(input_data)[4] = "Al Raffa Store"
    names(input_data)[5] = "Al Fahidi Store"
    names(input_data)[6] = "Al Manama Store"
    names(input_data)[7] = "Talal Store "
    names(input_data)[8] = "Al Madina Store"
    
   summary_data = data.frame(mlr::summarizeColumns(input_data))
   summary_data
  
  }, options = list(scrollX = TRUE))
  
}