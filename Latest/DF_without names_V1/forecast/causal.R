causalUI = function(id,label ="causal"){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(6,
             plotOutput(ns("response_curve")),
             verbatimTextOutput(ns("EOD"))
             ),
      column(6,
             plotOutput(ns("response_curve_2")),
             verbatimTextOutput(ns("EOD_2"))
             )
    )
    # plotOutput(ns("response_curve")),
    # # highchartOutput(ns("resp_curve")),
    # #dataTableOutput(ns("summary_resp")),
    # #verbatimTextOutput(ns("model_summary")),
    # verbatimTextOutput(ns("EOD"))
    # 
    
    
  )
}

causal=function(input,output,session,rbutton,Personal_Consumption,Expected_consumers,feature_causal,feature_causal_2,price,Discount,type,store_wise,reg_wise){
  data_reg<- data1 
  #names(data_reg)
  
  
  new_predict1 = data.frame(c(1:10
  ))
  names(new_predict1) = "Time_Variable"
  
  output$response_curve <- renderPlot({
    
    if(feature_causal() == "Price_Per_Unit" ){
      library(ggplot2)
      p = qplot(Price_Per_Unit, Monthly_sales_total,data = data_reg)
      #p + geom_line() + ylab("y")
      p + geom_smooth(aes(method = "lm", se = FALSE))
    }
    else{
      if(feature_causal() == "Discount_Percentage" ){
        p = qplot(Discount_Percentage, Monthly_sales_total,data = data_reg)
        p + geom_line() + ylab("y")
        p + geom_smooth(aes(method = "lm", se = FALSE))
      }
      else{
        if(feature_causal() == "Expected_consumers_income_millions" ){
          p = qplot(Expected_consumers_income_millions,Monthly_sales_total, data = data_reg)
          p + geom_line() + ylab("y")
          p + geom_smooth(aes(method = "lm", se = FALSE))
        }
        else{
          p = qplot(Personal_Consumption_Expenditures,Monthly_sales_total, data = data_reg)
          p + geom_line() + ylab("y")
          p + geom_smooth(aes(method = "lm", se = T))
        }
      }
    }
  })
  
  output$model_summary <-  renderPrint({
    fmla = as.formula(paste0("Monthly_sales_total~", paste(feature_causal())))
    outlm <- lm(fmla, data=data_reg)
    summary(outlm)
  }) 
  output$EOD <- renderPrint({
    
    
    
    data_reg<- data1[,-c(1,2,3)]
    # names(data_reg)
    # library(vegan)
    # data_reg<- decostand(data_reg,"range")
    # data_reg = data_reg+0.001
    
    outlm = lm(data_reg$Monthly_sales_total ~ Sales_Scale_Constant + Market__Share +Expected_consumers_income_millions+
                 Time_Variable + Personal_Consumption_Expenditures + Total_Market_Size_Monthly +
                 Temperature+Discount_Percentage+Price_Per_Unit,data_reg)
    
    
    
    
    Time_Variable. = max(data_reg$Time_Variable)
    Market__Share. = max(data_reg$Market__Share)
    Expected_consumers_income_millions. =max(data_reg$Expected_consumers_income_millions)
    Personal_Consumption_Expenditures. = max(data_reg$Personal_Consumption_Expenditures)
    Temperature. = mean(data_reg$Temperature)
    Total_Market_Size_Monthly. = max(data_reg$Total_Market_Size_Monthly)
    Sales_Scale_Constant. =max(data_reg$Sales_Scale_Constant)
    Discount_Percentage. = max(data_reg$Discount_Percentage)
    Price_Per_Unit. =min(data_reg$Price_Per_Unit)
    
    
    if(feature_causal() == "Price_Per_Unit" ){
      coeff = data.frame(1)
      coeff$Price_Per_Unit =  outlm$coefficients["Price_Per_Unit"]
      coeff$`(Intercept)` = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
      )
      
      pre = data.frame(12)
      names(pre) = "Price_Per_Unit"
      q1 = (pre$Price_Per_Unit[1]*coeff$Price_Per_Unit[1])+coeff$`(Intercept)`[1]
      pre1 = data.frame(13)
      names(pre1) = "Price_Per_Unit"
      q2 = (pre1$Price_Per_Unit[1]*coeff$Price_Per_Unit[1])+coeff$`(Intercept)`[1]
      P = mean(data_reg$Price_Per_Unit)
      Q = mean(data_reg$Monthly_sales_total)
      PE = ((q2-q1)/(13-12)) * (P/Q)
      cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(13-12)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Price by mean Sales.\nThis determines the increase in the price by 1 unit will decrease the sales by", abs((q2-q1)/(13-12)) * (P/Q),"PERCENT")
      
      
    }
    else{
      if(feature_causal() == "Discount_Percentage" ){
        coeff = data.frame(1)
        coeff$Discount_Percentage =  outlm$coefficients["Discount_Percentage"]
        coeff$Intercept = (outlm$coefficients["(Intercept)"]
                           +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                           +(outlm$coefficients["Market__Share"]*Market__Share.)
                           +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                           +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                           +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                           +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                           +(outlm$coefficients["Temperature"]*Temperature.)
                           
                           +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
        pre = data.frame(13)
        names(pre) = "Discount_Percentage"
        q1 = (pre$Discount_Percentage[1]*coeff$Discount_Percentage[1])+coeff$Intercept[1]
        pre1 = data.frame(12)
        names(pre1) = "Discount_Percentage"
        q2 = (pre1$Discount_Percentage[1]*coeff$Discount_Percentage[1])+coeff$Intercept[1]
        P = mean(data_reg$Discount_Percentage)
        Q = mean(data_reg$Monthly_sales_total)
        PE = ((q2-q1)/(12-13)) * (P/Q)
        
        
        cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(12-13)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean DISCOUNT by mean Sales.\nThis determines the increase in the DISCOUNT by 1 unit will INCREASE the sales by", ((q2-q1)/(12-13)) * (P/Q),"PERCENT")
        
      }
      else{
        if(feature_causal() == "Expected_consumers_income_millions" ){
          coeff = data.frame(1)
          coeff$Expected_consumers_income_millions =  outlm$coefficients["Expected_consumers_income_millions"]
          coeff$Intercept = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
                             +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
          pre = data.frame(418)
          names(pre) = "Expected_consumers_income_millions"
          q1 = (pre$Expected_consumers_income_millions[1]*coeff$Expected_consumers_income_millions[1])+coeff$Intercept[1]
          pre1 = data.frame(419)
          names(pre1) = "Expected_consumers_income_millions"
          q2 = (pre1$Expected_consumers_income_millions[1]*coeff$Expected_consumers_income_millions[1])+coeff$Intercept[1]
          P = mean(data_reg$Expected_consumers_income_millions)
          Q = mean(data_reg$Monthly_sales_total)
          
          PE = ((q2-q1)/(419-418)) * (P/Q)
          
          
          cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(419-418)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Expected consumers income by mean Sales.\nThis determines the increase in the Expected consumers income by 1 unit will INCREASE the sales by",  ((q2-q1)/(419-418)) * (P/Q),"PERCENT")
        }
        else{
          coeff = data.frame(1)
          coeff$Personal_Consumption_Expenditures =  outlm$coefficients["Personal_Consumption_Expenditures"]
          coeff$Intercept = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
                             +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.))
          
          pre = data.frame(12500)
          names(pre) = "Personal_Consumption_Expenditures"
          q1 = (pre$Personal_Consumption_Expenditures[1]*coeff$Personal_Consumption_Expenditures[1])+coeff$Intercept[1]
          pre1 = data.frame(13000)
          names(pre1) = "Personal_Consumption_Expenditures"
          q2 = (pre1$Personal_Consumption_Expenditures[1]*coeff$Personal_Consumption_Expenditures[1])+coeff$Intercept[1]
          P = mean(data_reg$Personal_Consumption_Expenditures)
          Q = mean(data_reg$Monthly_sales_total)
          PE = ((q2-q1)/(13000-12000)) * (P/Q)
          
          cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(13000-12000)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Expected consumers income by mean Sales.\nThis determines the increase in the Personal Consumption Expenditures by 500 units will INCREASE the sales by",  ((q2-q1)/(13000-12000)) * (P/Q),"PERCENT")
        }
      }
    }
    
    
    
    
    
  })
  
  
  output$response_curve_2 <- renderPlot({
    
    if(feature_causal_2() == "Price_Per_Unit" ){
      library(ggplot2)
      p = qplot(Price_Per_Unit, Monthly_sales_total,data = data_reg)
      #p + geom_line() + ylab("y")
      p + geom_smooth(aes(method = "lm", se = FALSE))
    }
    else{
      if(feature_causal_2() == "Discount_Percentage" ){
        p = qplot(Discount_Percentage, Monthly_sales_total,data = data_reg)
        p + geom_line() + ylab("y")
        p + geom_smooth(aes(method = "lm", se = FALSE))
      }
      else{
        if(feature_causal_2() == "Expected_consumers_income_millions" ){
          p = qplot(Expected_consumers_income_millions,Monthly_sales_total, data = data_reg)
          p + geom_line() + ylab("y")
          p + geom_smooth(aes(method = "lm", se = FALSE))
        }
        else{
          p = qplot(Personal_Consumption_Expenditures,Monthly_sales_total, data = data_reg)
          p + geom_line() + ylab("y")
          p + geom_smooth(aes(method = "lm", se = T))
        }
      }
    }
  })
  
  output$EOD_2 <- renderPrint({
    
    
    
    data_reg<- data1[,-c(1,2,3)]
    # names(data_reg)
    # library(vegan)
    # data_reg<- decostand(data_reg,"range")
    # data_reg = data_reg+0.001
    
    outlm = lm(data_reg$Monthly_sales_total ~ Sales_Scale_Constant + Market__Share +Expected_consumers_income_millions+
                 Time_Variable + Personal_Consumption_Expenditures + Total_Market_Size_Monthly +
                 Temperature+Discount_Percentage+Price_Per_Unit,data_reg)
    
    
    
    
    Time_Variable. = max(data_reg$Time_Variable)
    Market__Share. = max(data_reg$Market__Share)
    Expected_consumers_income_millions. =max(data_reg$Expected_consumers_income_millions)
    Personal_Consumption_Expenditures. = max(data_reg$Personal_Consumption_Expenditures)
    Temperature. = mean(data_reg$Temperature)
    Total_Market_Size_Monthly. = max(data_reg$Total_Market_Size_Monthly)
    Sales_Scale_Constant. =max(data_reg$Sales_Scale_Constant)
    Discount_Percentage. = max(data_reg$Discount_Percentage)
    Price_Per_Unit. =min(data_reg$Price_Per_Unit)
    
    
    if(feature_causal_2() == "Price_Per_Unit" ){
      coeff = data.frame(1)
      coeff$Price_Per_Unit =  outlm$coefficients["Price_Per_Unit"]
      coeff$`(Intercept)` = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
      )
      
      pre = data.frame(12)
      names(pre) = "Price_Per_Unit"
      q1 = (pre$Price_Per_Unit[1]*coeff$Price_Per_Unit[1])+coeff$`(Intercept)`[1]
      pre1 = data.frame(13)
      names(pre1) = "Price_Per_Unit"
      q2 = (pre1$Price_Per_Unit[1]*coeff$Price_Per_Unit[1])+coeff$`(Intercept)`[1]
      P = mean(data_reg$Price_Per_Unit)
      Q = mean(data_reg$Monthly_sales_total)
      PE = ((q2-q1)/(13-12)) * (P/Q)
      cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(13-12)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Price by mean Sales.\nThis determines the increase in the price by 1 unit will decrease the sales by", abs((q2-q1)/(13-12)) * (P/Q),"PERCENT")
      
      
    }
    else{
      if(feature_causal_2() == "Discount_Percentage" ){
        coeff = data.frame(1)
        coeff$Discount_Percentage =  outlm$coefficients["Discount_Percentage"]
        coeff$Intercept = (outlm$coefficients["(Intercept)"]
                           +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                           +(outlm$coefficients["Market__Share"]*Market__Share.)
                           +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                           +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                           +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                           +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                           +(outlm$coefficients["Temperature"]*Temperature.)
                           
                           +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
        pre = data.frame(13)
        names(pre) = "Discount_Percentage"
        q1 = (pre$Discount_Percentage[1]*coeff$Discount_Percentage[1])+coeff$Intercept[1]
        pre1 = data.frame(12)
        names(pre1) = "Discount_Percentage"
        q2 = (pre1$Discount_Percentage[1]*coeff$Discount_Percentage[1])+coeff$Intercept[1]
        P = mean(data_reg$Discount_Percentage)
        Q = mean(data_reg$Monthly_sales_total)
        PE = ((q2-q1)/(12-13)) * (P/Q)
        
        
        cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(12-13)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean DISCOUNT by mean Sales.\nThis determines the increase in the DISCOUNT by 1 unit will INCREASE the sales by", ((q2-q1)/(12-13)) * (P/Q),"PERCENT")
        
      }
      else{
        if(feature_causal_2() == "Expected_consumers_income_millions" ){
          coeff = data.frame(1)
          coeff$Expected_consumers_income_millions =  outlm$coefficients["Expected_consumers_income_millions"]
          coeff$Intercept = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
                             +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
          pre = data.frame(418)
          names(pre) = "Expected_consumers_income_millions"
          q1 = (pre$Expected_consumers_income_millions[1]*coeff$Expected_consumers_income_millions[1])+coeff$Intercept[1]
          pre1 = data.frame(419)
          names(pre1) = "Expected_consumers_income_millions"
          q2 = (pre1$Expected_consumers_income_millions[1]*coeff$Expected_consumers_income_millions[1])+coeff$Intercept[1]
          P = mean(data_reg$Expected_consumers_income_millions)
          Q = mean(data_reg$Monthly_sales_total)
          
          PE = ((q2-q1)/(419-418)) * (P/Q)
          
          
          cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(419-418)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Expected consumers income by mean Sales.\nThis determines the increase in the Expected consumers income by 1 unit will INCREASE the sales by",  ((q2-q1)/(419-418)) * (P/Q),"PERCENT")
        }
        else{
          coeff = data.frame(1)
          coeff$Personal_Consumption_Expenditures =  outlm$coefficients["Personal_Consumption_Expenditures"]
          coeff$Intercept = (outlm$coefficients["(Intercept)"]
                             +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
                             +(outlm$coefficients["Market__Share"]*Market__Share.)
                             +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
                             +(outlm$coefficients["Time_Variable"]*Time_Variable.)
                             
                             +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
                             +(outlm$coefficients["Temperature"]*Temperature.)
                             +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
                             +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.))
          
          pre = data.frame(12500)
          names(pre) = "Personal_Consumption_Expenditures"
          q1 = (pre$Personal_Consumption_Expenditures[1]*coeff$Personal_Consumption_Expenditures[1])+coeff$Intercept[1]
          pre1 = data.frame(13000)
          names(pre1) = "Personal_Consumption_Expenditures"
          q2 = (pre1$Personal_Consumption_Expenditures[1]*coeff$Personal_Consumption_Expenditures[1])+coeff$Intercept[1]
          P = mean(data_reg$Personal_Consumption_Expenditures)
          Q = mean(data_reg$Monthly_sales_total)
          PE = ((q2-q1)/(13000-12000)) * (P/Q)
          
          cat("Demand Elasticity Estimation \nTo calculate Demand Elasticity of Demand we use the formula:\nPE = (dQ/dP) * (P/Q)\n(dQ/dP) = ",((q2-q1)/(13000-12000)), "is determined by the coefficient of the study variable in our regression formula.\n(P/Q) = ",(P/Q),"is the mean Expected consumers income by mean Sales.\nThis determines the increase in the Personal Consumption Expenditures by 500 units will INCREASE the sales by",  ((q2-q1)/(13000-12000)) * (P/Q),"PERCENT")
        }
      }
    }
    
    
    
    
    
  })
  # output$summary_resp  <- renderDataTable({
  #   
  #   data_reg<- data1[,-c(1,2,3)]
  #   names(data_reg)
  #   library(vegan)
  #   data_reg<- decostand(data_reg,"range")
  #   data_reg = data_reg+0.001
  #   outlm = lm(data_reg$Monthly_sales_total ~ Sales_Scale_Constant + Market__Share +Expected_consumers_income_millions+
  #                Time_Variable + Personal_Consumption_Expenditures + Total_Market_Size_Monthly +
  #                Temperature+Discount_Percentage+Price_Per_Unit,data_reg)
  #   
  #   
  #   
  #   
  #   Time_Variable. = 0.9421765
  #   Market__Share. = 0.66856757
  #   Expected_consumers_income_millions. =0.9268146
  #   Personal_Consumption_Expenditures. = 0.8927217
  #   Temperature. = mean(data_reg$Temperature)
  #   Total_Market_Size_Monthly. = 0.9408206
  #   Sales_Scale_Constant. =0.331963632
  #   Discount_Percentage. = 1.00100000
  #   Price_Per_Unit. =0.00100000
  #   
  #   
  #   if(feature_causal() == "Price_Per_Unit" ){
  #     coeff = data.frame(1)
  #     coeff$Price_Per_Unit =  outlm$coefficients["Price_Per_Unit"]
  #     coeff$`(Intercept)` = (outlm$coefficients["(Intercept)"]
  #                            +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
  #                            +(outlm$coefficients["Market__Share"]*Market__Share.)
  #                            +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
  #                            +(outlm$coefficients["Time_Variable"]*Time_Variable.)
  #                            +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
  #                            +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
  #                            +(outlm$coefficients["Temperature"]*Temperature.)
  #                            +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
  #     )
  #     
  #   }
  #   else{
  #     if(feature_causal() == "Discount_Percentage" ){
  #       coeff = data.frame(1)
  #       coeff$Discount_Percentage =  outlm$coefficients["Discount_Percentage"]
  #       coeff$Intercept = (outlm$coefficients["(Intercept)"]
  #                          +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
  #                          +(outlm$coefficients["Market__Share"]*Market__Share.)
  #                          +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
  #                          +(outlm$coefficients["Time_Variable"]*Time_Variable.)
  #                          +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
  #                          +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
  #                          +(outlm$coefficients["Temperature"]*Temperature.)
  #                          
  #                          +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
  #     }
  #     else{
  #       if(feature_causal() == "Expected_consumers_income_millions" ){
  #         coeff = data.frame(1)
  #         coeff$Expected_consumers_income_millions =  outlm$coefficients["Expected_consumers_income_millions"]
  #         coeff$Intercept = (outlm$coefficients["(Intercept)"]
  #                            +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
  #                            +(outlm$coefficients["Market__Share"]*Market__Share.)
  #                            
  #                            +(outlm$coefficients["Time_Variable"]*Time_Variable.)
  #                            +(outlm$coefficients["Personal_Consumption_Expenditures"]*Personal_Consumption_Expenditures.)
  #                            +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
  #                            +(outlm$coefficients["Temperature"]*Temperature.)
  #                            +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
  #                            +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.)) 
  #       }
  #       else{
  #         coeff = data.frame(1)
  #         coeff$Personal_Consumption_Expenditures =  outlm$coefficients["Personal_Consumption_Expenditures"]
  #         coeff$Intercept = (outlm$coefficients["(Intercept)"]
  #                            +(outlm$coefficients["Sales_Scale_Constant"]*Sales_Scale_Constant.)
  #                            +(outlm$coefficients["Market__Share"]*Market__Share.)
  #                            +(outlm$coefficients["Expected_consumers_income_millions"]*Expected_consumers_income_millions.)
  #                            +(outlm$coefficients["Time_Variable"]*Time_Variable.)
  #                            
  #                            +(outlm$coefficients["Total_Market_Size_Monthly"]*Total_Market_Size_Monthly.)
  #                            +(outlm$coefficients["Temperature"]*Temperature.)
  #                            +(outlm$coefficients["Discount_Percentage"]*Discount_Percentage.)
  #                            +(outlm$coefficients["Price_Per_Unit"]*Price_Per_Unit.))
  #       }
  #     }
  #   }
  #   
  #   
  #   x= data.frame(coeff[,c(3,2)])
  #   x
  # })
  

  
}
