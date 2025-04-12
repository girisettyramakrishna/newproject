multi_variateUI = function(id,label ="multi_variate"){
  ns <- NS(id)
  
  tagList(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$h3("Loading....",style = "margin-left:150px;margin-top:100px;"),
                     tags$img(src = "spinner.gif",id = ns("loadmessage"),style = "width:70px;margin-left:150px;")),
    
    highchartOutput(ns("regression")),hr(),
    #h4(tags$b("EQUATION")),
    #div(p( strong("EQUATION"))),
    verbatimTextOutput(ns("reg_equation")),hr(),
    #div(p( strong("EVALUATION METRICS"))),
    h4(tags$b("EVALUATION METRICS")),
    dataTableOutput(ns("regre_metric")),hr(),
    #div(p( strong("COEFFICIENTS"))),
    h4(tags$b("COEFFICIENTS")),
    dataTableOutput(ns("equation")),
    fluidRow(column(6,h4(tags$b("Forecasted Plot")),highchartOutput(ns("forecast_plot_in_mul"))),column(6,h4(tags$b("Forecasted Values")),dataTableOutput(ns("forecast_val_in_mul"))))
    
  )
}

multi_variate=function(input,output,session,rbutton,features,price,Discount,type,store_wise,reg_wise,Personal_Consumption,Expected_consumers,feature_causal){
  
  names(data1)
  data_reg<- data1[,-c(1,2,3,4,8,9,10,12,13,25:33)]
  library(vegan)
  data_reg<- decostand(data_reg,"range")
  # str(data_reg)
  #data_reg = data_reg+0.001
  dict = c(names(data_reg[,c(4:13,3,14,1,15:19)]))
  names(dict) = c(names(data_reg[,c(4:13,3,14,1,15:19)]))

  
  #####################
  ##########regression_main
  #####################
 
  
 ##forecasted_table 
   forecast_table<- reactive({
    
    if(type() == "Country"){    
      forecasted = data.frame("forecasted" = c(1480805832,1634560210,1679637934,1696079344,1714156679,1753414280,1836249639,1973680044,2037758418,2110760125))
    } 
    else{
      if(type() == "Region"){
        if(reg_wise() == "NORTH_REGION"){
          forecasted = data.frame("forecasted" = c(643962546.8,660334359.9,681954624.9,633517855.1,576983043.8,685223088.3,657854420.6,710134941.7,759788587.7,703784749.2))
        }
        else{
          forecasted = data.frame("forecasted" = c(667445713.2,956562573.5,822208278.3,829525350.2,962456762,773833467.3,981722956.1,1052544544,1094861598,1129354482))
        }
        
      }
      else{
        if(store_wise() =="NORTH_STORE1"){
          forecasted = data.frame("forecasted" = c(310191355.9,319163843.7,286998545.6,288838455.6,276476884,299502539.9,271167446.3,328438441.8,405660557.8,320039203.8))
          
        }
        else{
          if(store_wise() =="NORTH_STORE2"){
            forecasted = data.frame("forecasted" = c(300482963,316261027.9,396098924.4,325466767.7,275386588.1,384423962.5,375843899.1,383944833.9,355173494.6,381004507))
          }
          else{
            if(store_wise() =="SOUTH_STORE1"){
              forecasted = data.frame("forecasted" = c(246633304.7,322916210.8,279144309.1,320987211.8,280772046.6,351637075,423245698.9,312739757.3,499529032.7,410285197.5))
            }
            else{
              if(store_wise() =="SOUTH_STORE2"){
                
                
                forecasted = data.frame("forecasted" = c(173427907.3,247913015.6,222294733.9,201912825.4,295994049.5,173120032.5,256402135.4,287348519.9,232838087,287964049.1))
                
              }
              else{
                
                forecasted = data.frame("forecasted" = c(257591129,265356758.4,275695144.1,311587006.2,313434678.7,266332601.8,300586126.4,327875015.3,282630527.3,289162715.7))
                
              }
            }
          }
        }
      }
    }
     print(forecasted)
    return(forecasted)
  })   

  
####regression _ plot
  
  
  output$regression <- renderHighchart({
    if(rbutton() == "Manual"){
      if(type() == "Country"){
        features = dict[features()]
        fmla = as.formula(paste0(data_reg[2],"~", paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        b=fitted(outlm)
        b=as.data.frame(b)
       
        hc_reg_main(as.numeric(unlist(data_reg[2])),b$b)
      }
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[20])),b$b)
            
          }
          else {
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[21])),b$b)
          }
        } 
        else{
          if(store_wise() =="NORTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[22],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[22])),b$b)
            
          }
          else if(store_wise() =="NORTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[23],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[23])),b$b)
            
          }else if(store_wise() =="SOUTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[24],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[24])),b$b)
            
          }else if(store_wise() =="SOUTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[25],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[25])),b$b)
            
          }else{
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[26],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            b=fitted(outlm)
            b=as.data.frame(b)
            hc_reg_main(as.numeric(unlist(data_reg[26])),b$b)
          }
        }
      }
    }
    else{
      if(type() == "Country"){    
        
        outlm = lm(as.numeric(unlist(data_reg[2])) ~ as.numeric(unlist(data_reg[1])) + as.numeric(unlist(data_reg[3])) +
                     as.numeric(unlist(data_reg[4])) + as.numeric(unlist(data_reg[8])) + as.numeric(unlist(data_reg[12])) +
                     as.numeric(unlist(data_reg[19]))+ as.numeric(unlist(data_reg[18]))+ as.numeric(unlist(data_reg[14])),data_reg)
        
        
        predicted <- predict(outlm, data_reg) 
        #predicted
        actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[2])), predicteds=predicted))  
        b=fitted(outlm)
        b=as.data.frame(b)
        dummy = data.frame("forecasted"=c(1:nrow(b)))
        dummy$forecasted = 0
        # m= forecasted
        
        
        forecasted = data.frame("forecasted" = c(1480805832,1634560210,1679637934,1696079344,1714156679,1753414280,1836249639,1973680044,2037758418,2110760125))
        forecasted = rbind(dummy,forecasted)
        forecasted<- decostand(forecasted,"range")
        forecasted[forecasted == 0] <- NA
        highchart() %>% 
          hc_title(text = "Monthly Sales") %>% 
          hc_add_series(name = "Actual Sales", data = as.numeric(unlist(data_reg[2])),
                        dataLabels = list(enabled = FALSE))%>% 
          hc_add_series(name = "Fitted Sales", data =b$b,
                        dataLabels = list(enabled = FALSE)) %>% 
          hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                        dataLabels = list(enabled = FALSE),color = "#FF6347")  
      } 
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            outlm = lm(as.numeric(unlist(data_reg[20])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[15])) + 
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[20])), predicteds=predicted))  
            b=fitted(outlm)
            b=as.data.frame(b)
            dummy = data.frame("forecasted"=c(1:nrow(b)))
            dummy$forecasted = 0
            forecasted = data.frame("forecasted" = c(643962546.8,660334359.9,681954624.9,633517855.1,576983043.8,685223088.3,657854420.6,710134941.7,759788587.7,703784749.2))
            forecasted = rbind(dummy,forecasted)
            forecasted<- decostand(forecasted,"range")
            forecasted[forecasted == 0] <- NA
            highchart() %>% 
              hc_title(text = "Monthly Sales") %>% 
              hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[20]))),
                            dataLabels = list(enabled = FALSE))%>% 
              hc_add_series(name = "Fitted Sales", data =b$b,
                            dataLabels = list(enabled = FALSE)) %>% 
              hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                            dataLabels = list(enabled = FALSE),color = "#FF6347")  
            
          }
          else{
            
            outlm = lm(as.numeric(unlist(data_reg[21])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[6])) + 
                         as.numeric(unlist(data_reg[9])) + 
                         as.numeric(unlist(data_reg[15])) +
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])) +
                         as.numeric(unlist(data_reg[18])) +
                         as.numeric(unlist(data_reg[19])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[21])), predicteds=predicted))  
            b=fitted(outlm)
            
            b=as.data.frame(b)
            dummy = data.frame("forecasted"=c(1:nrow(b)))
            dummy$forecasted = 0
            forecasted = data.frame("forecasted" = c(667445713.2,956562573.5,822208278.3,829525350.2,962456762,773833467.3,981722956.1,1052544544,1094861598,1129354482))
            forecasted = rbind(dummy,forecasted)
            forecasted<- decostand(forecasted,"range")
            forecasted[forecasted == 0] <- NA
            highchart() %>% 
              hc_title(text = "Monthly Sales") %>% 
              hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[21]))),
                            dataLabels = list(enabled = FALSE))%>% 
              hc_add_series(name = "Fitted Sales", data =b$b,
                            dataLabels = list(enabled = FALSE)) %>% 
              hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                            dataLabels = list(enabled = FALSE),color = "#FF6347")  
          }
          
        }
        else{
          if(store_wise() =="NORTH_STORE1"){
            
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[22])) ~ as.numeric(unlist(data_reg[3])) + 
                         as.numeric(unlist(data_reg[4])) + 
                         as.numeric(unlist(data_reg[12])) + 
                         as.numeric(unlist(data_reg[15])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[22])), predicteds=predicted))  
            b=fitted(outlm)
            
            b=as.data.frame(b)
            dummy = data.frame("forecasted"=c(1:nrow(b)))
            dummy$forecasted = 0
            forecasted = data.frame("forecasted" = c(310191355.9,319163843.7,286998545.6,288838455.6,276476884,299502539.9,271167446.3,328438441.8,405660557.8,320039203.8))
            forecasted = rbind(dummy,forecasted)
            forecasted<- decostand(forecasted,"range")
            forecasted[forecasted == 0] <- NA
            highchart() %>% 
              hc_title(text = "Monthly Sales") %>% 
              hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[22]))),
                            dataLabels = list(enabled = FALSE))%>% 
              hc_add_series(name = "Fitted Sales", data =b$b,
                            dataLabels = list(enabled = FALSE)) %>% 
              hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                            dataLabels = list(enabled = FALSE),color = "#FF6347")  
          }
          else{
            if(store_wise() =="NORTH_STORE2"){
              
              #training on train data 
              outlm = lm(as.numeric(unlist(data_reg[23])) ~ as.numeric(unlist(data_reg[3])) + 
                           as.numeric(unlist(data_reg[11])) + 
                           as.numeric(unlist(data_reg[12])) +
                           as.numeric(unlist(data_reg[15])) + 
                           as.numeric(unlist(data_reg[16])),data_reg)
              
              predicted <- predict(outlm, data_reg) 
              #predicted
              actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[23])), predicteds=predicted))  
              b=fitted(outlm)
              
              b=as.data.frame(b)
              dummy = data.frame("forecasted"=c(1:nrow(b)))
              dummy$forecasted = 0
              forecasted = data.frame("forecasted" = c(300482963,316261027.9,396098924.4,325466767.7,275386588.1,384423962.5,375843899.1,383944833.9,355173494.6,381004507))
              forecasted = rbind(dummy,forecasted)
              forecasted<- decostand(forecasted,"range")
              forecasted[forecasted == 0] <- NA
              highchart() %>% 
                hc_title(text = "Monthly Sales") %>% 
                hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[23]))),
                              dataLabels = list(enabled = FALSE))%>% 
                hc_add_series(name = "Fitted Sales", data =b$b,
                              dataLabels = list(enabled = FALSE)) %>% 
                hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                              dataLabels = list(enabled = FALSE),color = "#FF6347")  
            }
            else{
              if(store_wise() =="SOUTH_STORE1"){
                
                #training on train data 
                outlm = lm(as.numeric(unlist(data_reg[24])) ~ as.numeric(unlist(data_reg[3])) + 
                             as.numeric(unlist(data_reg[9])) +
                             as.numeric(unlist(data_reg[10])) + 
                             as.numeric(unlist(data_reg[11])) + 
                             as.numeric(unlist(data_reg[12])) + 
                             as.numeric(unlist(data_reg[16])) +
                             as.numeric(unlist(data_reg[17])) + 
                             as.numeric(unlist(data_reg[19])),data_reg)
                
                predicted <- predict(outlm, data_reg) 
                #predicted
                actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[24])), predicteds=predicted))  
                b=fitted(outlm)
                
                b=as.data.frame(b)
                dummy = data.frame("forecasted"=c(1:nrow(b)))
                dummy$forecasted = 0
                forecasted = data.frame("forecasted" = c(246633304.7,322916210.8,279144309.1,320987211.8,280772046.6,351637075,423245698.9,312739757.3,499529032.7,410285197.5))
                forecasted = rbind(dummy,forecasted)
                forecasted<- decostand(forecasted,"range")
                forecasted[forecasted == 0] <- NA
                highchart() %>% 
                  hc_title(text = "Monthly Sales") %>% 
                  hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[24]))),
                                dataLabels = list(enabled = FALSE))%>% 
                  hc_add_series(name = "Fitted Sales", data =b$b,
                                dataLabels = list(enabled = FALSE)) %>% 
                  hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                                dataLabels = list(enabled = FALSE),color = "#FF6347")  
              }
              else{
                if(store_wise() =="SOUTH_STORE2"){
                  
                  #############################
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[2])) ~ as.numeric(unlist(data_reg[3])) + 
                               as.numeric(unlist(data_reg[4])) + 
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[12])) + 
                               as.numeric(unlist(data_reg[14])) +
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])) +
                               as.numeric(unlist(data_reg[18])),data_reg)
                  
                  predicted <- predict(outlm, data_reg) 
                  #predicted
                  actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[25])), predicteds=predicted))  
                  b=fitted(outlm)
                  
                  b=as.data.frame(b)
                  dummy = data.frame("forecasted"=c(1:nrow(b)))
                  dummy$forecasted = 0
                  forecasted = data.frame("forecasted" = c(173427907.3,247913015.6,222294733.9,201912825.4,295994049.5,173120032.5,256402135.4,287348519.9,232838087,287964049.1))
                  forecasted = rbind(dummy,forecasted)
                  forecasted<- decostand(forecasted,"range")
                  forecasted[forecasted == 0] <- NA
                  highchart() %>% 
                    hc_title(text = "Monthly Sales") %>% 
                    hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[25]))),
                                  dataLabels = list(enabled = FALSE))%>% 
                    hc_add_series(name = "Fitted Sales", data =b$b,
                                  dataLabels = list(enabled = FALSE)) %>% 
                    hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                                  dataLabels = list(enabled = FALSE),color = "#FF6347")  
                }
                else{
                  #Outputlm1 <- lm(data_reg$SOUTH_STORE3 ~., data_reg[-c(2,20:25)])
                  # Use stepAIC to build model based on AIC
                  #  stepaic = stepAIC(Outputlm1, direction = "both")
                  #############################
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[26])) ~ as.numeric(unlist(data_reg[1])) +
                               as.numeric(unlist(data_reg[3])) + 
                               as.numeric(unlist(data_reg[6])) + 
                               as.numeric(unlist(data_reg[7])) + 
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[11])) +
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])),data_reg)
                  
                  predicted <- predict(outlm, data_reg)
                  #predicted
                  actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[26])), predicteds=predicted))  
                  b=fitted(outlm)
                  
                  b=as.data.frame(b)
                  dummy = data.frame("forecasted"=c(1:nrow(b)))
                  dummy$forecasted = 0
                  forecasted = data.frame("forecasted" = c(257591129,265356758.4,275695144.1,311587006.2,313434678.7,266332601.8,300586126.4,327875015.3,282630527.3,289162715.7))
                  forecasted = rbind(dummy,forecasted)
                  forecasted<- decostand(forecasted,"range")
                  forecasted[forecasted == 0] <- NA
                  highchart() %>% 
                    hc_title(text = "Monthly Sales") %>% 
                    hc_add_series(name = "Actual Sales", data =(as.numeric(unlist(data_reg[26]))),
                                  dataLabels = list(enabled = FALSE))%>% 
                    hc_add_series(name = "Fitted Sales", data =b$b,
                                  dataLabels = list(enabled = FALSE)) %>% 
                    hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                                  dataLabels = list(enabled = FALSE),color = "#FF6347")  
                }
              }
            }
          }
        }
      }
    }
  })
  ###############
  ##regression metrics
  ###############
  hc_reg_main = function(data,data2){
    highchart() %>% 
      hc_title(text = "Monthly Sales") %>% 
      hc_add_series(name = "Actual Sales", data =data,
                    dataLabels = list(enabled = FALSE))%>% 
      hc_add_series(name = "Fitted Sales", data =data2,
                    dataLabels = list(enabled = FALSE)) 
  }
  output$regre_metric <- renderDataTable({
    if(rbutton() == "Manual"){
      if(type() == "Country"){
        features = dict[features()]
        fmla = as.formula(paste0(data_reg[2],"~", paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        predicted = predict(outlm,data_reg)
        actuals_preds<- data.frame(cbind(actuals=  as.numeric(unlist(data_reg[2])), predicteds=predicted))  
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
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[20])), predicteds=predicted))  
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
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[21])), predicteds=predicted))  
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
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[22],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[22])) , predicteds=predicted))  
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
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[23],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[23])), predicteds=predicted))  
            accuracy <- cor(actuals_preds) 
            
            mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
            mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
            rmse <- sqrt(mse)
            metrics = data.frame(accuracy[2],mape,mse,rmse)
            names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
            metrics = round(metrics,3)
            metrics
          }else if(store_wise() =="SOUTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[24],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[24])), predicteds=predicted))  
            accuracy <- cor(actuals_preds) 
            
            mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
            mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
            rmse <- sqrt(mse)
            metrics = data.frame(accuracy[2],mape,mse,rmse)
            names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
            metrics = round(metrics,3)
            metrics
          }else if(store_wise() =="SOUTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[25],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[25])), predicteds=predicted))  
            accuracy <- cor(actuals_preds) 
            
            mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)
            mse <- mean((actuals_preds$predicteds - actuals_preds$actuals)^2)
            rmse <- sqrt(mse)
            metrics = data.frame(accuracy[2],mape,mse,rmse)
            names(metrics) = c("ACCURACY","MAPE","MSE","RMSE")  
            metrics = round(metrics,3)
            metrics
            
          }else{
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[26],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            predicted = predict(outlm,data_reg)
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[26])), predicteds=predicted))  
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
    }
    else{
      if(type() == "Country"){    
       
        #training on train data 
        outlm = lm(as.numeric(unlist(data_reg[2])) ~ as.numeric(unlist(data_reg[1])) +
                     as.numeric(unlist(data_reg[3])) + 
                     as.numeric(unlist(data_reg[4])) +
                     as.numeric(unlist(data_reg[8])) + 
                     as.numeric(unlist(data_reg[12])) + 
                     as.numeric(unlist(data_reg[19]))+
                     as.numeric(unlist(data_reg[18]))+
                     as.numeric(unlist(data_reg[14])),data_reg)
        
        predicted <- predict(outlm, data_reg) 
        #predicted
        actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[2])), predicteds=predicted))  
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
            #Outputlm1 <- lm(data_reg$NORTH_REGION ~., data_reg[-c(2,21:26)])
            # Use stepAIC to build model based on AIC
            #stepaic = stepAIC(Outputlm1, direction = "both")
            #############################
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[20])) ~ as.numeric(unlist(data_reg[3])) + 
                         as.numeric(unlist(data_reg[15])) +
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[20])), predicteds=predicted))  
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
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[21])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[6])) + 
                         as.numeric(unlist(data_reg[9])) + 
                         as.numeric(unlist(data_reg[15])) + 
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])) + 
                         as.numeric(unlist(data_reg[18])) + 
                         as.numeric(unlist(data_reg[19])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[21])), predicteds=predicted))  
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
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[22])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[4])) + 
                         as.numeric(unlist(data_reg[12])) + 
                         as.numeric(unlist(data_reg[15])),data_reg)
            
            predicted <- predict(outlm, data_reg) 
            #predicted
            actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[22])), predicteds=predicted))  
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
            if(store_wise() =="NORTH_STORE2"){
              #training on train data 
              outlm = lm(as.numeric(unlist(data_reg[23])) ~ as.numeric(unlist(data_reg[3])) +
                           as.numeric(unlist(data_reg[11])) + 
                           as.numeric(unlist(data_reg[12])) + 
                           as.numeric(unlist(data_reg[15])) + 
                           as.numeric(unlist(data_reg[16])),data_reg)
              
              predicted <- predict(outlm, data_reg) 
              #predicted
              actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[23])), predicteds=predicted))  
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
              if(store_wise() =="SOUTH_STORE1"){
                #training on train data 
                outlm = lm(as.numeric(unlist(data_reg[24])) ~ as.numeric(unlist(data_reg[3])) +
                             as.numeric(unlist(data_reg[9])) + 
                             as.numeric(unlist(data_reg[10])) + 
                             as.numeric(unlist(data_reg[11])) +
                             as.numeric(unlist(data_reg[12])) + 
                             as.numeric(unlist(data_reg[16])) + 
                             as.numeric(unlist(data_reg[17])) +
                             as.numeric(unlist(data_reg[19])),data_reg)
                
                predicted <- predict(outlm, data_reg) 
                #predicted
                actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[24])), predicteds=predicted))  
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
                if(store_wise() =="SOUTH_STORE2"){
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[25])) ~ as.numeric(unlist(data_reg[3])) +
                               as.numeric(unlist(data_reg[4])) +
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[12])) +
                               as.numeric(unlist(data_reg[14])) +
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])) +
                               as.numeric(unlist(data_reg[18])),data_reg)
                  
                  predicted <- predict(outlm, data_reg) 
                  #predicted
                  actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[25])), predicteds=predicted))  
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
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[26])) ~ as.numeric(unlist(data_reg[1])) +
                               as.numeric(unlist(data_reg[3])) + 
                               as.numeric(unlist(data_reg[6])) +
                               as.numeric(unlist(data_reg[7])) +
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[11])) + 
                               as.numeric(unlist(data_reg[15])) +
                               as.numeric(unlist(data_reg[17])),data_reg)
                  
                  predicted <- predict(outlm, data_reg) 
                  #predicted
                  actuals_preds<- data.frame(cbind(actuals=as.numeric(unlist(data_reg[26])), predicteds=predicted))  
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
          }
          
        }
      }
    }
    
    
  },options=list(dom = 't'))
  
  
  
  ###############
  ##regression predicts
  ###############
  ###############
  new_predict = data.frame(c(1.00713497,
                             1.01326994,
                             1.01940491,
                             1.02553988,
                             1.03167581,
                             1.03780982,
                             1.04394479,
                             1.05007976,
                             1.05621473,
                             1.06221473
  ))
  names(new_predict) = names(data_reg[4])
  
  new_predict$Market__Share = mean(as.numeric(unlist(data_reg[3])))
  new_predict$Personal_Consumption_Expenditures = mean(as.numeric(unlist(data_reg[8])))
  new_predict$Temperature = mean(as.numeric(unlist(data_reg[19])))
  new_predict$Total_Market_Size_Monthly = mean(as.numeric(unlist(data_reg[12])))
  

  output$reg_equation <- renderPrint({  
    if(rbutton() == "Manual"){
      if(type() == "Country"){
        features = dict[features()]
        fmla = as.formula(paste0(data_reg[2],"~", paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
        
      }
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            outlm <- lm(fmla, data=data_reg)
            
          }
          else {
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }
        } 
        else{
          if(store_wise() =="NORTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[22],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }
          else if(store_wise() =="NORTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[23],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            
          }else if(store_wise() =="SOUTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[24],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            
          }else if(store_wise() =="SOUTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[25],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            
            
          }else{
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[26],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            
          }
          
          
        }
        
      }
    }
    else{
      if(type() == "Country"){    
        #training on train data 
        outlm = lm(as.numeric(unlist(data_reg[2])) ~ as.numeric(unlist(data_reg[1])) +
                     as.numeric(unlist(data_reg[3])) + 
                     as.numeric(unlist(data_reg[4])) + 
                     as.numeric(unlist(data_reg[8])) +
                     as.numeric(unlist(data_reg[12])) + 
                     as.numeric(unlist(data_reg[19]))+
                     as.numeric(unlist(data_reg[18]))+
                     as.numeric(unlist(data_reg[14])),data_reg)
        x = data.frame(outlm$coefficients)
        names(x) = "coefficients"
        rownames(x) <- c("Intercept",names(data_reg[,c(1,3,4,8,12,19,18,14)]))
        
      } 
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[20])) ~ as.numeric(unlist(data_reg[3])) + 
                         as.numeric(unlist(data_reg[15])) +
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])),data_reg)
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,15,16,17)]))
          }
          else{
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[21])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[6])) + 
                         as.numeric(unlist(data_reg[9])) + 
                         as.numeric(unlist(data_reg[15])) + 
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])) + 
                         as.numeric(unlist(data_reg[18])) + 
                         as.numeric(unlist(data_reg[19])),data_reg)
            
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,6,9,15,16,17,18,19)]))
          }
        }
        else{
          if(store_wise() =="NORTH_STORE1"){
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[22])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[4])) + as.numeric(unlist(data_reg[12])) + 
                         as.numeric(unlist(data_reg[15])),data_reg)
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,4,12,15)]))
          }
          else{
            if(store_wise() =="NORTH_STORE2"){
              #training on train data 
              outlm = lm(as.numeric(unlist(data_reg[23])) ~ as.numeric(unlist(data_reg[3])) +
                           as.numeric(unlist(data_reg[11])) + 
                           as.numeric(unlist(data_reg[12])) + 
                           as.numeric(unlist(data_reg[15])) +
                           as.numeric(unlist(data_reg[16])),data_reg)
              x = data.frame(outlm$coefficients)
              names(x) = "coefficients"
              rownames(x) <- c("Intercept",names(data_reg[,c(3,11,12,15,16)]))
            }
            else{
              if(store_wise() =="SOUTH_STORE1"){
                #training on train data 
                outlm = lm(as.numeric(unlist(data_reg[24])) ~ as.numeric(unlist(data_reg[3])) + 
                             as.numeric(unlist(data_reg[9])) +
                             as.numeric(unlist(data_reg[10])) + 
                             as.numeric(unlist(data_reg[11])) + 
                             as.numeric(unlist(data_reg[12])) + 
                             as.numeric(unlist(data_reg[16])) + 
                             as.numeric(unlist(data_reg[17])) + 
                             as.numeric(unlist(data_reg[19])),data_reg)
                x = data.frame(outlm$coefficients)
                names(x) = "coefficients"
                rownames(x) <- c("Intercept",names(data_reg[,c(3,9,10,11,12,16,17,19)]))
              }
              else{
                if(store_wise() =="SOUTH_STORE2"){
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[25])) ~ as.numeric(unlist(data_reg[3])) +
                               as.numeric(unlist(data_reg[4])) + 
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[12])) +
                               as.numeric(unlist(data_reg[14])) +
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])) + 
                               as.numeric(unlist(data_reg[18])),data_reg)
                  x = data.frame(outlm$coefficients)
                  names(x) = "coefficients"
                  rownames(x) <- c("Intercept",names(data_reg[,c(3,4,9,12,14,15,17,18)]))
                }
                else{
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[26])) ~ as.numeric(unlist(data_reg[1])) +
                               as.numeric(unlist(data_reg[3])) + 
                               as.numeric(unlist(data_reg[6])) +
                               as.numeric(unlist(data_reg[7])) +
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[11])) + 
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])),data_reg)
                  x = data.frame(outlm$coefficients)
                  names(x) = "coefficients"
                  rownames(x) <- c("Intercept",names(data_reg[,c(1,3,6,7,9,11,15,17)]))
                }
              }
            }
          }
          
        }
      }
    }
    if(rbutton() == "Manual"){
      paste("Sales =",x$coefficients[1],"+(",rownames(x)[2],"*",x$coefficients[2],")+(",rownames(x)[3],"*",x$coefficients[3],")+(",rownames(x)[4],"*",x$coefficients[4],")+(",rownames(x)[5],"*",x$coefficients[5],")+(",rownames(x)[6],"*",x$coefficients[6],")+(",rownames(x)[7],"*",x$coefficients[7],")+(",rownames(x)[8],"*",x$coefficients[8],")+(",rownames(x)[9],"*",x$coefficients[9],")")
    }
    else{
      paste("Sales =",x$coefficients[1],"+(",rownames(x)[2],"*",x$coefficients[2],")+(",rownames(x)[3],"*",x$coefficients[3],")+(",rownames(x)[4],"*",x$coefficients[4],")+(",rownames(x)[5],"*",x$coefficients[5],")+(",rownames(x)[6],"*",x$coefficients[6],")+(",rownames(x)[7],"*",x$coefficients[7],")+(",rownames(x)[8],"*",x$coefficients[8],")+(",rownames(x)[9],"*",x$coefficients[9],")")  
    }
  })
  
  
  ###############################
  output$equation <- renderDataTable({
    if(rbutton() == "Manual"){
      if(type() == "Country"){
        features = dict[features()]
        fmla = as.formula(paste0(data_reg[2],"~", paste(features,collapse="+")))
        outlm <- lm(fmla, data=data_reg)
        
        
      }
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
            
          }
          else {
            features = dict[features()]
            fmla = as.formula(paste(reg_wise(), paste(features,collapse=" + "), sep = " ~ "))
            #fmla = as.formula(paste0("input$REGION" ~, paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }
        } 
        else{
          if(store_wise() =="NORTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[22],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }
          else if(store_wise() =="NORTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[23],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }else if(store_wise() =="SOUTH_STORE1"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[24],"~" , paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }else if(store_wise() =="SOUTH_STORE2"){
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[25],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }else{
            features = dict[features()]
            fmla = as.formula(paste0(data_reg[26],"~", paste(features,collapse="+")))
            outlm <- lm(fmla, data=data_reg)
          }
        }
      }
      x = data.frame(outlm$coefficients)
      names(x) = "coefficients"
      round(x,3)
    }
    else{
      if(type() == "Country"){    
        #training on train data 
        outlm = lm(as.numeric(unlist(data_reg[2])) ~ as.numeric(unlist(data_reg[1])) +
                     as.numeric(unlist(data_reg[3])) + 
                     as.numeric(unlist(data_reg[4])) +
                     as.numeric(unlist(data_reg[8])) +
                     as.numeric(unlist(data_reg[12])) + 
                     as.numeric(unlist(data_reg[19]))+
                     as.numeric(unlist(data_reg[18]))+
                     as.numeric(unlist(data_reg[14])),data_reg)
        x = data.frame(outlm$coefficients)
        names(x) = "coefficients"
        rownames(x) <- c("Intercept",names(data_reg[,c(1,3,4,8,12,19,18,14)]))
        round(x,3)
      } 
      else{
        if(type() == "Region"){
          if(reg_wise() == "NORTH_REGION"){
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[20])) ~ as.numeric(unlist(data_reg[3])) + 
                         as.numeric(unlist(data_reg[15])) + 
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])),data_reg)
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,15,16,17)]))
            round(x,3)
          }
          else{
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[21])) ~ as.numeric(unlist(data_reg[3])) + 
                         as.numeric(unlist(data_reg[6])) + 
                         as.numeric(unlist(data_reg[9])) + 
                         as.numeric(unlist(data_reg[15])) +
                         as.numeric(unlist(data_reg[16])) + 
                         as.numeric(unlist(data_reg[17])) +
                         as.numeric(unlist(data_reg[18])) + 
                         as.numeric(unlist(data_reg[19])),data_reg)
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,6,9,15,16,17,18,19)]))
            round(x,3)
          }
        }
        else{
          if(store_wise() =="NORTH_STORE1"){
            #training on train data 
            outlm = lm(as.numeric(unlist(data_reg[22])) ~ as.numeric(unlist(data_reg[3])) +
                         as.numeric(unlist(data_reg[4])) + 
                         as.numeric(unlist(data_reg[12])) + 
                         as.numeric(unlist(data_reg[15])),data_reg)
            x = data.frame(outlm$coefficients)
            names(x) = "coefficients"
            rownames(x) <- c("Intercept",names(data_reg[,c(3,4,12,15)]))
            round(x,3)
          }
          else{
            if(store_wise() == "NORTH_STORE2"){
              #training on train data 
              outlm = lm(as.numeric(unlist(data_reg[23])) ~ as.numeric(unlist(data_reg[3])) +
                           as.numeric(unlist(data_reg[11])) + 
                           as.numeric(unlist(data_reg[12])) +
                           as.numeric(unlist(data_reg[15])) +
                           as.numeric(unlist(data_reg[16])),data_reg)
              x = data.frame(outlm$coefficients)
              names(x) = "coefficients"
              rownames(x) <- c("Intercept",names(data_reg[,c(3,11,12,15,16)]))
              round(x,3)
            }
            else{
              if(store_wise() =="SOUTH_STORE1"){
                #training on train data 
                outlm = lm(as.numeric(unlist(data_reg[24])) ~ as.numeric(unlist(data_reg[3])) + 
                             as.numeric(unlist(data_reg[9])) +
                             as.numeric(unlist(data_reg[10])) + 
                             as.numeric(unlist(data_reg[11])) +
                             as.numeric(unlist(data_reg[12])) + 
                             as.numeric(unlist(data_reg[16])) +
                             as.numeric(unlist(data_reg[17])) +
                             as.numeric(unlist(data_reg[19])),data_reg)
                x = data.frame(outlm$coefficients)
                names(x) = "coefficients"
                rownames(x) <- c("Intercept",names(data_reg[,c(3,9,10,11,12,16,17,19)]))
                round(x,3)
              }
              else{
                if(store_wise() =="SOUTH_STORE2"){
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[25])) ~ as.numeric(unlist(data_reg[3])) +
                               as.numeric(unlist(data_reg[4])) +
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[12])) +
                               as.numeric(unlist(data_reg[14])) +
                               as.numeric(unlist(data_reg[15])) + 
                               as.numeric(unlist(data_reg[17])) +
                               as.numeric(unlist(data_reg[18])),data_reg)
                  x = data.frame(outlm$coefficients)
                  names(x) = "coefficients"
                  rownames(x) <- c("Intercept",names(data_reg[,c(3,4,9,12,14,15,17,18)]))
                  round(x,3)
                }
                else{
                  #training on train data 
                  outlm = lm(as.numeric(unlist(data_reg[26])) ~ as.numeric(unlist(data_reg[1])) +
                               as.numeric(unlist(data_reg[3])) + 
                               as.numeric(unlist(data_reg[6])) +
                               as.numeric(unlist(data_reg[7])) +
                               as.numeric(unlist(data_reg[9])) + 
                               as.numeric(unlist(data_reg[11])) + 
                               as.numeric(unlist(data_reg[15])) +
                               as.numeric(unlist(data_reg[17])),data_reg)
                  x = data.frame(outlm$coefficients)
                  names(x) = "coefficients"
                  rownames(x) <- c("Intercept",names(data_reg[,c(1,3,6,7,9,11,15,17)]))
                  round(x,3)
                }
              }
            }
          }
          
        }
      }
    }
       
  },options=list())
  
  
  output$forecast_plot_in_mul <- renderHighchart({
    forecasted = forecast_table()
    
    highchart() %>% 
      hc_title(text = "Forecasted") %>% 
      hc_add_series(name = "Forecasted Sales", data =forecasted$forecasted,
                    dataLabels = list(enabled = FALSE),color = "#FF6347")  
    
    
  }) 
  
  
  output$forecast_val_in_mul <- renderDataTable({
    forecasted = forecast_table()
    forecasted
  },options=list(pageLength=7, scrollX=TRUE)) 
  
  
  
  
  
  
  
  
  
  
}