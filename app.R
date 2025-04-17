library(shiny)

ui <- fluidPage(
                  tags$head(
                                tags$title("Demand Forecasting"),
                                    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")  # Optional
                                  ),
                  div(
                          style = "text-align:center;",
                              tags$img(src = "logo.png", height = "100px")
                            ),
                  div(
                          style = "width: 300px; margin: 50px auto; padding: 20px; box-shadow: 0px 0px 10px #ccc;",
                              textInput("user", "Username"),
                              passwordInput("pass", "Password"),
                                  actionButton("login", "Login")
                                )
                  )

server <- function(input, output, session) {
          # Auth logic here
}

shinyApp(ui = ui, server = server)

