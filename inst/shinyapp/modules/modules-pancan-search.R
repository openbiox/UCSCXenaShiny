ui.modules_pancan_dist <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Module: gene pancan expression distribution"),
            fluidRow(  
              shinyWidgets::searchInput(
                inputId = ns("Pancan_search"),
                label = NULL,
                btnSearch = icon("search"),
                btnReset = icon("remove"),
                placeholder = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
                width = "40%"
              )),
            fluidRow(
              materialSwitch(ns("pdist_mode"), "Show violin plot", inline=TRUE),
              materialSwitch(ns("pdist_show_p_value"), "Show P value", inline=TRUE),
              materialSwitch(ns("pdist_show_p_label"), "Show P label", inline=TRUE)
            ),
            column(12,
                   plotOutput(ns("gene_pancan_dist"))),
            column(12,
                   h4("NOTEs:"),
                   h5("1. The data query may take some time based on your network. Wait until a plot shows"),
                   h5("2. The unit of gene expression is log2(tpm+0.001)"),
                   h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels" ))
  )
  
  
}

server.modules_pancan_dist <- function(input, output, session) {
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      output$gene_pancan_dist <- renderPlot(
        {
          vis_toil_TvsN(Gene = input$Pancan_search,
                        Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"), 
                        Show.P.value = input$pdist_show_p_value, 
                        Show.P.label = input$pdist_show_p_label)
        }
      )
      
      
    }})
}

