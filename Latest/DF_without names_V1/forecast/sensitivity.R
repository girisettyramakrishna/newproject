sensitivityUI <- function(id,label ="sensitivity"){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    highchartOutput(ns("predicted_reg")),
    dataTableOutput(ns("predicted_values"))
  )
}

sensitivity<- function(input,output,session,rbutton,type,store_wise,reg_wise,Discount_sensitivity,price_sensitivity,Advertising_costs,Sulphate_content,Market_Share,Number_of_Retail_Locations){
  output$predicted_reg <- renderHighchart({
    data_reg = data1
    ###############
    ##regression predicts
    ###############
    
    new_predict = data.frame(c(155:164))
    names(new_predict) = names(data_reg[11])
    
    new_predict$Personal_Consumption_Expenditures = c(13147.72 ,13416.17, 13218.91, 13456.71, 13187.31, 13622.37, 13274.24, 13699.26 ,13826.56 ,13554.34)
    new_predict$Population_growth	=c(206118816,208589726,204665148,208047114,206131126,202239509,208301110,208330153,204540434,207634590)
    new_predict$Consumer_Price_Index_Urban=c(268.8246,282.2794,242.5449,283.2271,263.2287,223.6362,284.4897,284.3940,244.7104,274.9463)
    new_predict$Total_Market_Size_Monthly=c( 4167935949,4182167425,4147864167,4185538373,4161593511,4129613688,4187147683,4186911282,4145125811,4177011981)
    new_predict$Expected_consumers_income_millions=c(420.5405,421.4443,421.7349,421.9204,422.5651,421.8581,421.6138,421.9582,423.5242,418.8421)
    new_predict$Health_Consciousness_Rate=c(4.642757,4.644052,4.653496,4.638121,4.610815,4.631967,4.631962,4.631954,4.589161,4.582963)
    new_predict$Temperature = c(26.40237 ,30.61565 ,24.94955, 18.17398, 27.82403, 17.85996, 32.71129, 28.70302, 16.74813, 33.32348)
    
    #training on data 
    outlm <- lm.ridge (as.numeric(unlist(data_reg[6])) ~   as.numeric(unlist(data_reg[11])) + 
                         as.numeric(unlist(data_reg[17])) + 
                         as.numeric(unlist(data_reg[18])) + 
                         as.numeric(unlist(data_reg[19])) +
                         as.numeric(unlist(data_reg[21]))+
                         as.numeric(unlist(data_reg[14]))+
                         as.numeric(unlist(data_reg[15]))+
                         as.numeric(unlist(data_reg[37]))+
                         as.numeric(unlist(data_reg[23]))+
                         as.numeric(unlist(data_reg[36]))+
                         as.numeric(unlist(data_reg[16]))+
                         as.numeric(unlist(data_reg[22]))+
                         as.numeric(unlist(data_reg[24]))+
                         as.numeric(unlist(data_reg[35])),data_reg,lambda = 4.185 )
    
 
    # summary(outlm)
    # MASS::select(outlm)
    # plot(outlm)
    # optimum lambda withminimum GCV = 4.185
    
    
    new_predict$Price_Per_Unit = price_sensitivity()
    new_predict$Discount_Percentage = Discount_sensitivity()
    new_predict$Sulphate_content = Sulphate_content()
    new_predict$Market_Share = Market_Share()
    new_predict$Advertising_costs = Advertising_costs()
    new_predict$Number_of_Retail_Locations = Number_of_Retail_Locations()
    
    predicted = coef(outlm)[1]+coef(outlm)[2]*as.numeric(unlist(data_reg[11]))+coef(outlm)[3]*as.numeric(unlist(data_reg[17]))+
      coef(outlm)[4]*as.numeric(unlist(data_reg[18]))+coef(outlm)[5]*as.numeric(unlist(data_reg[19]))+coef(outlm)[6]*as.numeric(unlist(data_reg[21]))+coef(outlm)[7]*as.numeric(unlist(data_reg[14]))+
      coef(outlm)[8]*as.numeric(unlist(data_reg[15])) + coef(outlm)[9]*as.numeric(unlist(data_reg[37]))+ coef(outlm)[10]*as.numeric(unlist(data_reg[23]))+coef(outlm)[11]*as.numeric(unlist(data_reg[36]))+coef(outlm)[12]*as.numeric(unlist(data_reg[16]))+coef(outlm)[13]*as.numeric(unlist(data_reg[22]))
    + coef(outlm)[14]*as.numeric(unlist(data_reg[24]))+ coef(outlm)[15]*as.numeric(unlist(data_reg[35]))
    
    predicted1 = coef(outlm)[1]+coef(outlm)[2]*new_predict$Time_Variable+coef(outlm)[3]*new_predict$Personal_Consumption_Expenditures+
      coef(outlm)[4]*new_predict$Population_growth+coef(outlm)[5]*new_predict$Consumer_Price_Index_Urban+coef(outlm)[6]*new_predict$Total_Market_Size_Monthly+coef(outlm)[7]*new_predict$Expected_consumers_income_millions+
      coef(outlm)[8]*new_predict$Health_Consciousness_Rate + coef(outlm)[9]*new_predict$Temperature   + coef(outlm)[10]*new_predict$Price_Per_Unit   +coef(outlm)[11]*new_predict$Discount_Percentage   +coef(outlm)[12]*new_predict$Sulphate_content  +coef(outlm)[13]*new_predict$Market_Share
    + coef(outlm)[14]*new_predict$Advertising_costs + coef(outlm)[15]*new_predict$Number_of_Retail_Locations
    
    predicted1 = data.frame(predicted1)
    predicted = data.frame(predicted)
    #predicted
    names(predicted1) = c("predicted")
    #to show an increasing trend
    #predicted1 = predicted1+1000000000
    names(predicted) =c("predicted")
    actual_pred <- rbind(predicted,predicted1)  
    
    highchart() %>% 
      hc_title(text = "Monthly Sales") %>% 
      hc_add_series(name = "Sales", data =actual_pred$predicted,
                    dataLabels = list(enabled = FALSE))
    
    
    
    
    # # #predicted
    # # names(predicted1) = c("predicted")
    # # #to show an increasing trend
    # # #predicted1 = predicted1+1000000000
    # # names(predicted) =c("predicted")
    # #actual_pred <- rbind(predicted,predicted1)  
    # 
    # predicted$d = NA
    # predicted1$e =NA
    # predicted1 = predicted1[,c(2,1)]
    # #to show an increasing trend
    # #predicted1 = predicted1+1000000000
    # names(predicted) =c("predicted","f")
    # names(predicted1) = c("predicted","f")
    # 
    # ggg = rbind(predicted,predicted1)
    # 
    # #to show an increasing trend
    # #predicted1 = predicted1+1000000000
    # highchart() %>% 
    #   hc_title(text = "Monthly Sales") %>% 
    #   hc_add_series(name = "Sales", data =ggg$predicted,
    #                 dataLabels = list(enabled = FALSE))%>% 
    #   hc_add_series(name = "Predicted", data =ggg$f,
    #                 dataLabels = list(enabled = FALSE))%>%
    #   hc_xAxis(categories = index(ggg))
    # 
    
    # highchart() %>% 
    #   hc_title(text = "Monthly Sales") %>% 
    #   hc_add_series(name = "Sales", data =actual_pred$predicted,
    #                 dataLabels = list(enabled = FALSE))
  })
  
  output$predicted_values = renderDataTable({
    data_reg = data1
    ###############
    ##regression predicts
    ###############
    new_predict = data.frame(c(155:164))
    names(new_predict) = names(data_reg[11])
    
    new_predict$Personal_Consumption_Expenditures = c(13047.72 ,13016.17, 13118.91, 13156.71, 13187.31, 13222.37, 13264.24, 13299.26 ,13326.56 ,13354.34)
    new_predict$Population_growth	=c(206118816,207589726,207665148,208047114,208131126,208239509,208301110,208330153,208540434,208634590)
    new_predict$Consumer_Price_Index_Urban=c(238.8246,242.2794,242.5449,243.2271,243.2287,243.6362,244.4897,244.3940,244.7104,244.9463)
    new_predict$Total_Market_Size_Monthly=c( 4057935949,4082167425,4107864167,4135538373,4161593511,4189613688,4007147683,4146911282,4095125811,4077011981)
    new_predict$Expected_consumers_income_millions=c(420.5405,421.4443,421.7349,421.9204,422.5651,421.8581,421.6138,421.9582,423.5242,418.8421)
    new_predict$Health_Consciousness_Rate=c(4.642757,4.644052,4.653496,4.638121,4.610815,4.631967,4.631962,4.631954,4.589161,4.582963)
    new_predict$Temperature = c( 26.40237 ,30.61565 ,31.94955, 29.17398, 27.82403, 29.85996, 32.71129, 28.70302, 26.74813, 33.32348)
    
    #new_predict$Total_Market_Size_Cumulative	=c(44317399546,44445583578,44573942614,44703178482,42789473109,44963102329,45093794156,45224975167,43474908503,45488812530)
    
    
    #training on data 
    outlm <- lm.ridge (as.numeric(unlist(data_reg[6])) ~   as.numeric(unlist(data_reg[1])) + 
                         as.numeric(unlist(data_reg[17])) + 
                         as.numeric(unlist(data_reg[18])) + 
                         as.numeric(unlist(data_reg[19])) +
                         as.numeric(unlist(data_reg[21]))+
                         as.numeric(unlist(data_reg[14]))+
                         as.numeric(unlist(data_reg[15]))+
                         as.numeric(unlist(data_reg[37]))+
                         as.numeric(unlist(data_reg[23]))+
                         as.numeric(unlist(data_reg[36]))+
                         as.numeric(unlist(data_reg[16]))+
                         as.numeric(unlist(data_reg[22]))+
                         as.numeric(unlist(data_reg[24]))+
                         as.numeric(unlist(data_reg[35])),data_reg,lambda = 4.185 )
    
    
    # summary(outlm)
    # MASS::select(outlm)
    # plot(outlm)
    # optimum lambda withminimum GCV = 4.185
    
    
    new_predict$Price_Per_Unit = price_sensitivity()
    new_predict$Discount_Percentage = Discount_sensitivity()
    new_predict$Sulphate_content = Sulphate_content()
    new_predict$Market_Share = Market_Share()
    new_predict$Advertising_costs = Advertising_costs()
    new_predict$Number_of_Retail_Locations = Number_of_Retail_Locations()
    
   
    
    predicted1 = coef(outlm)[1]+coef(outlm)[2]*new_predict$Time_Variable+coef(outlm)[3]*new_predict$Personal_Consumption_Expenditures+
      coef(outlm)[4]*new_predict$Population_growth+coef(outlm)[5]*new_predict$Consumer_Price_Index_Urban+coef(outlm)[6]*new_predict$Total_Market_Size_Monthly+coef(outlm)[7]*new_predict$Expected_consumers_income_millions+
      coef(outlm)[8]*new_predict$Health_Consciousness_Rate + coef(outlm)[9]*new_predict$Temperature   + coef(outlm)[10]*new_predict$Price_Per_Unit   +coef(outlm)[11]*new_predict$Discount_Percentage   +coef(outlm)[12]*new_predict$Sulphate_content  +coef(outlm)[13]*new_predict$Market_Share
    + coef(outlm)[14]*new_predict$Advertising_costs + coef(outlm)[15]*new_predict$Number_of_Retail_Locations
    
    predicted1 = data.frame(predicted1)
    predicted1 = data.frame(round((predicted1$predicted1)/10^6,3))
  #  data.frame(round((reqd/10^6),2))
    colnames(predicted1)=c("Predicted Values(in millions)")
    predicted1
  })
}