ui.modules_pancan_anatomy <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression Anatomy Visualization"),
    fluidRow(
      tags$div(
        style = "margin-left: 30px;",
        fluidRow(
          shinyWidgets::searchInput(
            inputId = ns("Pancan_search"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          ),
          shinyBS::bsPopover(ns("Pancan_search"),
                             title = "Tips",
                             content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
                             placement = "right", options = list(container = "body"))
        )
      )),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidPage(
          fluidRow(
            selectInput(inputId = ns("Gender"), label = h4("Select Gender for plot"), choices = c("Male","Female"), selected = "Female")
          )
        )
      ),
      mainPanel = mainPanel(
        column(
          12,
          plotOutput(ns("pancan_anatomy"))
        )
      )
    )
  )
}

server.modules_pancan_anatomy  <- function(input, output, session) {
  observeEvent(input$Pancan_search, {
    output$pancan_anatomy <- renderPlot({
      vis_pancan_anatomy(
        Gene = input$Pancan_search,
        Gender = input$Gender
      )
    })
  })
}
