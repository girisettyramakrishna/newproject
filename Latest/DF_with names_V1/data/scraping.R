scrapingUI = function(id,label ="scraping"){
  ns <- NS(id)
tagList(
  textInput(ns("url"), "URL", 'https://www.wunderground.com/history/airport/OMDB/2004/4/1/MonthlyHistory.html?req_city=Dubai&req_state=DU&req_statename=UAE&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=41194'),
  dateInput(ns("date1"), "Date:", value = "2012-02-29", format = "dd-mm-yyyy"),
  dataTableOutput(ns("scrape"))
)
}

scraping=function(input,output,session){
  output$scrape <- renderDataTable({
    #parse_date_time(input$date1,"dby")
    b = ymd(input$date1)
    #set_config(config(ssl_verifypeer = 0L))
    url2=input$url
    ### scrape monthly mean temperature ###
    session <- html_session(url2)
    form <- html_form(session)[[2]]
    filled_form <- set_values(form,
                              'year' = year(b),'month'= month(b),'day'= day(b))
    temp<- submit_form(session, filled_form) %>%
      html_nodes(xpath='//*[@id="historyTable"]') %>%
      html_table()
    df = data.frame(temp)
    
    df
    names(df) = c('Weather parameter','Max','Avg','Min','Sum')
    df=df[-c(1,5,11,9,14,17),]
    df = data.frame(df)
    row.names(df)= c(1:12)
    df
    
  })
}