# List of all the required packages
if (!("shiny" %in% row.names(installed.packages())))
 install.packages("shiny")
library(shiny)

if (!("ggplot2" %in% row.names(installed.packages())))
 install.packages("ggplot2")
library(ggplot2)

if (!("shinydashboard" %in% row.names(installed.packages())))
    install.packages("shinydashboard")
library(shinydashboard)

if (!("shinyjs" %in% row.names(installed.packages())))
   install.packages("shinyjs")
library(shinyjs)

if (!("fma" %in% row.names(installed.packages())))
   install.packages("fma")
library(fma)

if (!("forecast" %in% row.names(installed.packages())))
   install.packages("forecast")
library(forecast)

if (!("highcharter" %in% row.names(installed.packages())))
   install.packages("highcharter")
library(highcharter)

if (!("DT" %in% row.names(installed.packages())))
   install.packages("DT")
library(DT)

if (!("Boruta" %in% row.names(installed.packages())))
   install.packages("Boruta")
library(Boruta)

if (!("zoo" %in% row.names(installed.packages())))
   install.packages("zoo")
library(zoo)

if (!("lubridate" %in% row.names(installed.packages())))
   install.packages("lubridate")
library(lubridate)

if (!("CombMSC" %in% row.names(installed.packages())))
   install.packages("CombMSC")
library(CombMSC)

if (!("DMwR" %in% row.names(installed.packages())))
   install.packages("DMwR")
library(DMwR)

if (!("vegan" %in% row.names(installed.packages())))
   install.packages("vegan")
library(vegan)

if (!("MASS" %in% row.names(installed.packages())))
   install.packages("MASS")
library(MASS)

if (!("car" %in% row.names(installed.packages())))
   install.packages("car")
library(car)

if (!("devtools" %in% row.names(installed.packages())))
   install.packages("devtools")
library(devtools)

if (!("Rcpp" %in% row.names(installed.packages())))
   install.packages("Rcpp")
library(Rcpp)

if (!("rCharts" %in% row.names(installed.packages())))
  install_github('ramnathv/rcharts')
library(rCharts)

if (!("rvest" %in% row.names(installed.packages())))
   install.packages("rvest")
library(rvest)

if (!("stargazer" %in% row.names(installed.packages())))
   install.packages("stargazer")
library(stargazer)

if (!("httr" %in% row.names(installed.packages())))
   install.packages("httr")
library(httr)

if (!("plotly" %in% row.names(installed.packages())))
   install.packages("plotly")
library(plotly)

if (!("Hmisc" %in% row.names(installed.packages())))
   install.packages("Hmisc")
library(Hmisc)

if (!("nnet" %in% row.names(installed.packages())))
   install.packages("nnet")
library(nnet)

if (!("forecastxgb" %in% row.names(installed.packages())))
  devtools::install_github("ellisp/forecastxgb-r-package/pkg", force=T)
library(forecastxgb)

if (!("corrplot" %in% row.names(installed.packages())))
   install.packages("corrplot")
library(corrplot)

if (!("RMySQL" %in% row.names(installed.packages())))
   install.packages("RMySQL")
library(RMySQL)

if (!("lme4" %in% row.names(installed.packages())))
    install.packages("lme4")
library(lme4) 

if (!("mlr" %in% row.names(installed.packages())))
  install.packages("mlr")
