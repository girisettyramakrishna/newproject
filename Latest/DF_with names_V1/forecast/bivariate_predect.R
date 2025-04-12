bivariate_predectUI = function(id,label ="bivariate_predect"){
  ns <- NS(id)
  tagList(
     
    highchartOutput(ns("resp_curve_predect")),
    div(p( strong("EQUATION"))),
    verbatimTextOutput(ns("reg_equation_predect")),
    div(p( strong("EVALUATION METRICS"))),
    dataTableOutput(ns("regre_metric_predect")),
    div(p( strong("COEFFICIENTS"))),
    dataTableOutput(ns("equation_predect"))
  )
}
bivariate_predect=function(input,output,session,type,store_wise,reg_wise,endo){
  
  hc_reg_main = function(data,data2){
    highchart() %>% 
      hc_title(text = "Monthly Sales") %>% 
      hc_add_series(name = "Actual Sales", data =data,
                    dataLabels = list(enabled = FALSE))%>% 
      hc_add_series(name = "Fitted Sales", data =data2,
                    dataLabels = list(enabled = FALSE)) 
  }
  
  data_reg<- data1[,-c(1,2,3,4,8,9,10,12,13,25:33)]
  names(data_reg)
  library(vegan)
  data_reg<- decostand(data_reg,"range")
  data_reg = data_reg+0.001
  
  output$resp_curve_predect <- renderHighchart({
    if(type() == "Country"){
      fmla = as.formula(paste0("Monthly_sales_total~", paste(endo())))
      outlm <- lm(fmla, data=data_reg)
      b=fitted(outlm)
      b=as.data.frame(b)
      hc_reg_main(data_reg$Monthly_sales_total,b$b)
      
      
    }
    else{
      if(type() == "Region"){
        if(reg_wise() == "NORTH_REGION"){
          fmla = as.formula(paste0("NORTH_REGION~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$NORTH_REGION,b$b)
          
        }
        else {
          fmla = as.formula(paste0("SOUTH_REGION~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$SOUTH_REGION,b$b)
          
        }
        
        
      } 
      else{
        if(store_wise() =="NORTH_STORE1"){
          fmla = as.formula(paste0("NORTH_STORE1~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$NORTH_STORE1,b$b)
          
        }
        else if(store_wise() =="NORTH_STORE2"){
          fmla = as.formula(paste0("NORTH_STORE2~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$NORTH_STORE2,b$b)
          
        }else if(store_wise() =="SOUTH_STORE1"){
          fmla = as.formula(paste0("SOUTH_STORE1~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$SOUTH_STORE1,b$b)
          
        }else if(store_wise() =="SOUTH_STORE2"){
          fmla = as.formula(paste0("SOUTH_STORE2~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$SOUTH_STORE2,b$b)
          
        }else{
          fmla = as.formula(paste0("SOUTH_STORE3~", paste(endo())))
          outlm <- lm(fmla, data=data_reg)
          b=fitted(outlm)
          b=as.data.frame(b)
          hc_reg_main(data_reg$SOUTH_STORE3,b$b)
          
        }
        
        
      }
      
    }
    
  
  })
  
  
  output$reg_equation_predect <- renderPrint({ 
  if(type() == "Country"){
   # 
    fmla = as.formula(paste0("Monthly_sales_total~", paste(endo())))
    outlm <- lm(fmla, data=data_reg)
    
  }
  else{
    if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        fmla = as.formula(paste0("NORTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
      }
      else {
        fmla = as.formula(paste0("SOUTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
        
      }
      
      
    } 
    else{
      if(store_wise() =="NORTH_STORE1"){
        fmla = as.formula(paste0("NORTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }
      else if(store_wise() =="NORTH_STORE2"){
        fmla = as.formula(paste0("NORTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }else if(store_wise() =="SOUTH_STORE1"){
        fmla = as.formula(paste0("SOUTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }else if(store_wise() =="SOUTH_STORE2"){
        fmla = as.formula(paste0("SOUTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
        
      }else{
        fmla = as.formula(paste0("SOUTH_STORE3~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }
      
      
    }
    
  }
    x = data.frame(outlm$coefficients)
    names(x) = "coefficients"
    x$coefficients
    rownames(x)[1]
    cat("Sales =",x$coefficients[1],"+(",rownames(x)[2],"*",x$coefficients[2],")")
   
})
  
  
  
  output$regre_metric_predect <- renderDataTable({
  if(type() == "Country"){
    
    fmla = as.formula(paste0("Monthly_sales_total~", paste(endo())))
    outlm <- lm(fmla, data=data_reg)
    predicted = predict(outlm,data_reg)
    actuals_preds<- data.frame(cbind(actuals=data_reg$Monthly_sales_total, predicteds=predicted))  
    accuracy <- cor(actuals_preds) 
    
    mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
    mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
    rmse <- sqrt(mse)
    metrics = data.frame(accuracy[2],mape,mse,rmse)
    names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
    metrics = round(metrics,3)
    metrics
    
  }
  else{
    if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        
        fmla = as.formula(paste0("NORTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$NORTH_REGION, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
      }
      else {
        
        fmla = as.formula(paste0("SOUTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$SOUTH_REGION, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
        
      }
      
      
    } 
    else{
      if(store_wise() =="NORTH_STORE1"){
        
        fmla = as.formula(paste0("NORTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$NORTH_STORE1 , predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
      }
      else if(store_wise() =="NORTH_STORE2"){
        
        fmla = as.formula(paste0("NORTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$NORTH_STORE2, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
      }else if(store_wise() =="SOUTH_STORE1"){
        
        fmla = as.formula(paste0("SOUTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$SOUTH_STORE1, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
      }else if(store_wise() =="SOUTH_STORE2"){
        
        fmla = as.formula(paste0("SOUTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$SOUTH_STORE2, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
        
      }else{
        
        fmla = as.formula(paste0("SOUTH_STORE3~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=data_reg$SOUTH_STORE3, predicteds=predicted))  
        accuracy <- cor(actuals_preds) 
        
        mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
        mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
        rmse <- sqrt(mse)
        metrics = data.frame(accuracy[2],mape,mse,rmse)
        names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
        metrics = round(metrics,3)
        metrics
      }
      
      
    }
    
  }
  },options=list())
  
  
  output$equation_predect <- renderDataTable({
  
  if(type() == "Country"){
    
    fmla = as.formula(paste0("Monthly_sales_total~", paste(endo())))
    outlm <- lm(fmla, data=data_reg)
    
    
  }
  else{
    if(type() == "Region"){
      if(reg_wise() == "NORTH_REGION"){
        
        fmla = as.formula(paste0("NORTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
      }
      else {
        
        fmla = as.formula(paste0("SOUTH_REGION~", paste(endo())))
        
        #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
        
      }
      
      
    } 
    else{
      if(store_wise() =="NORTH_STORE1"){
        
        fmla = as.formula(paste0("NORTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }
      else if(store_wise() =="NORTH_STORE2"){
        
        fmla = as.formula(paste0("NORTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }else if(store_wise() =="SOUTH_STORE1"){
        
        fmla = as.formula(paste0("SOUTH_STORE1~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }else if(store_wise() =="SOUTH_STORE2"){
        
        fmla = as.formula(paste0("SOUTH_STORE2~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
        
      }else{
        
        fmla = as.formula(paste0("SOUTH_STORE3~", paste(endo())))
        outlm <- lm(fmla, data=data_reg)
        
      }
      
      
    }
    
  }
    x = data.frame(outlm$coefficients)
    names(x) = "coefficients"
    round(x,3)
},options=list())
}