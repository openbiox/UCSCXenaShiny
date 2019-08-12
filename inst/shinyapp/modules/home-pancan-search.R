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
  ns <- session$ns
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      showModal(
        modalDialog(
          title = paste("Pancan distribution of gene", input$Pancan_search),
          size = "l",
          fluidPage(
            fluidRow(
              column(3, pickerInput(ns("pdist_mode"), "Mode", 
                                    choices = c("Boxplot", "Violinplot"),
                                    selected = "Boxplot", width = "fit")),
              column(3, prettyCheckbox(ns("pdist_show_p_value"), "Show P value", 
                                       icon = icon("check"))),
              column(3, prettyCheckbox(ns("pdist_show_p_label"), "Show P label", 
                                       icon = icon("check"))),
              column(3, actionButton(ns("pdist_show_button"), "Show!"))
            ),
            column(12,
                   plotOutput(ns("gene_pancan_dist"))),
            column(12,
                   h4("NOTEs:"),
                   h5("The data query may take some time based on your network. Wait until a plot shows..."),
                   h5("The unit is log2(tpm+0.001)"))
            
          )
        )
      )

      # pdist_mode <- eventReactive(input$pdist_show_button, {
      #   input$pdist_mode
      #   print(input$pdist_mode)
      # })
      
      output$gene_pancan_dist <- renderPlot(
        {
          vis_toil_TvsN(Gene = input$Pancan_search,
                        Mode = input$pdist_mode, 
                        Show.P.value = input$pdist_show_p_value, 
                        Show.P.label = input$pdist_show_p_label)
        }
      )
  }})
}
