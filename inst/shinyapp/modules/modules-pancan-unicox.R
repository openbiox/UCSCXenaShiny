ui.modules_pancan_unicox  <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Uni-cox analysis"),
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
            selectInput(inputId = ns("measure"), label = h4("Select Measure for plot"), choices = c("OS","PFI","DSS","DFI"), selected = "OS"),
            selectInput(inputId = ns("threshold"), label = h4("Select Threshold for plot"), choices = c(0.25,0.5), selected = 0.5)
          )
        )
      ),
      mainPanel = mainPanel(
        column(
          6,
          plotOutput(ns("unicox_gene_tree"))
        )
      )
    )
    
  )
}



server.modules_pancan_unicox <- function(input, output, session) {
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      output$unicox_gene_tree <- renderPlot({
        vis_unicox_tree(
          Gene = input$Pancan_search,
          measure = input$measure,
          threshold = input$threshold
        )
      })
    }
  })
}
