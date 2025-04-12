## Demand Forecasting 
## Programmers : Tharun S, Sagarika T, Sai T
## Date : 10th May 2017

# Clear the Environment
rm(list=ls(all=TRUE))
#load("dataframes.RData")
# Packages to be installed
source("packages_to_be_installed.R")
source("forecast.R")
source("data.R")
source("univariate.R")
source("bivariate.R")
source("output.R")

ui <- fluidPage(
  tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             "td { padding:5px; }",
             ".skin-blue .wrapper{ background-color:#ffffff; }",
             ".well {position:fixed; width:450px;}",
             ".sidebar-toggle {display:none;}",
             ".skin-blue .main-header .navbar {background-color:#2c3e50;}",
             ".skin-blue .main-header .logo {background-color:#2c3e50 ; }",
             "#box_variance{background-color:lightblue;width: 200px; height: 80px;border:1px solid black; margin-left:30px; padding-left:10px;}"
            # ".small-box .inner { padding:0px;margin-left:20px; margin-top:5px; }"
  ),
  
  dashboardPage(
    dashboardHeader(title = "Demand Forecasting",titleWidth = 300),
    dashboardSidebar( width = 0, 
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Demand Forecasting", tbName = "tab_four", icon = icon("line-chart"))
                        )
    ),
   
   dashboardBody(
      tags$head(tags$style(HTML('.content-wrapper,
                                 .right-side {
                              background-color: #ffffff;
                              }'))),
      
      
      
      tabsetPanel(
        tabPanel(title="Home",
                 div(style = 'margin-left:100px;margin-top:30px;text-align: justify;',
                     p(style = 'color:darkblue;font-size: 30px',"Demand Forecasting "),
                     p(style = 'color:black;font-size: 18px;font-style:italic',"A demand forecast is the prediction of what will happen to your company's existing product sales.
                       Demand Forecasting enables a firm to accurately and efficiently allocate resources to a level of 
                       production that meets anticipated demand there by enabling efficient production planning and 
                       inventory  replenishment  strategies. "),
                     p(style = 'color:darkblue;font-size: 30px',"Current Solutions- Limitations"),
                     p(style = 'color:black;font-size: 18px;font-style:italic',"
                       Traditional tools lack the ability to integrate diverse data sources like IoT sensors, social media causing forecast accuracy to stagnate.
                       Non-linear patterns are difficult to capture and Outliers can bias the estimation of the model parameters.
                       Generally, lack of statistical skills and correct consumer marketing acumen results in poor regression. Because it is manually intensive, it suffered from persistent bias and poor planner productivity. 
                       "),
                     p(style = 'color:darkblue;font-size: 30px',"Data Sciences as Solution Provider: (Approach & Methodology)"),
                     p(style = 'color:black;font-size: 18px;font-style:italic',"
                       Using time series methods like ARIMA for forecasting demand.
	Perform causal analysis by incorporating variables from a multitude of structured and unstructured data sources. 
Forecast demand for finished goods using advanced ML methods like multi-variate, lasso and ridge regression.
Sensitivity Scenario analysis enables decision makers understand the impact of endogenous factors on sales.
Variable importance analysis help identify the  most significant variables affecting net sales 
Patterns and outliers can be easily identified by effective exploratory data analysis.

"),
                     p(style = 'color:darkblue;font-size: 30px',"Business Benefits:"),
                     p(style = 'color:black;font-size: 18px;font-style:italic',"
Profit maximisation is ensured by offering better control of factors or parameters identified by causal analysis.
Integration of various data sources like IoT sensors, social media account for a vast range of factors affecting net sales.
Sensitivity Scenario analysis enables decision makers understand the impact of endogenous factors on sales and help maximise their profits.
                       
")
                     
                     )
                     ),
        tabPanel(title="Data",
                 dataUI("data")
        ),
        tabPanel(title="Descriptive Analysis",
                univariateUI("univariate")
        ),
        # tabPanel(title="Bivariate",
        #          bivariateUI("bivariate")),
        tabPanel(title="Multivariate",
                 forecastUI("forecast")),
        tabPanel(title="Outcome",
                 error_outputUI("error_output"))

        )
    )
  ))

options(shiny.maxRequestSize=180*1024^2)

server <- function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  callModule(data,"data")
  callModule(forecast,"forecast")
  # callModule(bivariate,"bivariate")
  callModule(univariate,"univariate")
  callModule(error_output,"error_output")
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)