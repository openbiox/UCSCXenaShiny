
ui.modules_pancan_gene_cor <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: TCGA Gene-Gene Correlation"),
    fluidRow(
      tags$div(
        style = "margin-left: 30px;",
        fluidRow(
          shinyWidgets::searchInput(
            inputId = ns("pancan_search1"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          ),
          shinyWidgets::searchInput(
            inputId = ns("pancan_search2"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          )
        )
      )),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
       fluidPage(
         fluidRow(
           materialSwitch(ns("purity_adj"), "Adjust Purity", inline = TRUE)
         )
       )
      ),
      mainPanel = mainPanel(
        column(
          8,wellPanel(
            plotOutput(ns("gene_cor"),height = "600px")
          )
        )
      )
    )
  )  
}

server.modules_pancan_gene_cor <- function(input, output, session) {
  ns <- session$ns
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_cor"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- reactive({
    if (nchar(input$pancan_search1) >= 1 & nchar(input$pancan_search2) >= 1) {
      p <- vis_gene_cor(
        Gene1 = input$pancan_search1,
        Gene2 = input$pancan_search2, 
        purity_adj = input$purity_adj, 
        split = FALSE)
    }
    return(p)
  })
  
  observeEvent(list(input$pancan_search1,input$pancan_search2), {
    output$gene_cor <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })
  
}