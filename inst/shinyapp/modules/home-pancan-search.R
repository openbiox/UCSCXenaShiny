ui.home_search_box <- function(id) {
  ns <- NS(id)

  shinyWidgets::searchInput(
    inputId = ns("Pancan_search"),
    label = NULL,
    btnSearch = icon("search"),
    btnReset = icon("remove"),
    placeholder = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
    width = "80%"
  )
}

server.home_search_box <- function(input, output, session) {
  ns = session$ns
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      showModal(
        modalDialog(
          title = paste("Pancan distribution of gene", input$Pancan_search),
          size = "l",
          textOutput(ns("gene_pancan_dist"))
          # DT::DTOutput(
          #   "table_query"
          # )
        )
      )
      output$gene_pancan_dist <- renderText("Sorry, this feature is under development!")
    }
  })
}
