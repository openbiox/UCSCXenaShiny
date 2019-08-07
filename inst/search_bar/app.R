# searchInput(
#   inputId = "id", 
#   label = "Enter your search :", 
#   placeholder = "This is a placeholder", 
#   btnSearch = icon("search"), 
#   btnReset = icon("remove"), 
#   width = "100%"
# )

print(here)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
ui <- dashboardPage(
  dashboardHeader(title = "Web Page"),
  dashboardSidebar(
    width = 0
  ),
  dashboardBody(
    box(title = "Web Page Search", status = "primary",height = "155" 
        ,solidHeader = T,
        uiOutput("search_plot")),
    box( title = "Web Page", status = "primary", height = "860px",solidHeader = T, 
         uiOutput("wep_page"))))
server <- function(input, output) 
{ 
  output$search_plot <- renderUI({
    searchInput(inputId = "Id009", 
                label = "Enter the address",
                btnSearch = icon("search"), 
                btnReset = icon("remove"), 
                value='https://',
                width = "100%")
  })
  output$wep_page <- renderUI({
    print(input$Id009)
    tags$iframe(src=input$Id009,width='100%',height='800px')
  })
}
shinyApp(ui, server)