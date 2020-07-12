ui.modules_pancan_immune <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression vs Immune Gene Signature"),
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
            selectInput(inputId = ns("immune_sig"), "Select the immune signature source", selected = "Cibersort",
                        choices= c("Yasin","Wolf","Attractors","ICR","c7atoms","Bindea","Cibersort"))
          )
        )
      ),
      mainPanel = mainPanel(
        column(
          12,
          plotOutput(ns("hm_gene_immune_cor"))
        )
      )
    )
  )
}




server.modules_pancan_immune <- function(input, output, session) {
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      output$hm_gene_immune_cor <- renderPlot({
        vis_gene_immune_cor(
          Gene = input$Pancan_search,
          Immune_sig_type = input$immune_sig
        )
      })
    }
  })
}