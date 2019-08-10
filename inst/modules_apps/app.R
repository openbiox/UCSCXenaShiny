library(shiny)

data(iris)

# load separate module and function scripts
source("xx_modules.R")

ui <- fluidPage(
  headerPanel("Iris k-means clustering"),
  xx_mod_ui("input1")
)

server <- function(input, output, session) {
  # res <- callModule(xx_mod, "input1")

  res <- callModule(xx_mod_server, "input1")
}

shinyApp(ui, server)
