library(shiny)

ui <- fluidPage(
  # Add UI elements here
  titlePanel("My Shiny App")
)

server <- function(input, output, session) {
  # Add server logic here
}

shinyApp(ui, server)

