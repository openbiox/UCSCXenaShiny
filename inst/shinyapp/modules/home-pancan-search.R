ui.home_search_box <- function(id) {
  ns <- NS(id)

  # 如果使用的话，需要进行服务端加速，参考下文
  # ref: https://shiny.rstudio.com/articles/selectize.html
  selectizeInput(inputId = ns("Pancan_search"),
                 label = NULL,
                 choices = c("", "TP53", "KRAS"),
                 selected = "",
                 multiple = FALSE, # allow for multiple inputs
                 options = list(
                   create = TRUE,
                   maxOptions = 5,
                   placeholder = 'Enter a gene symbol, e.g. TP53'))
  # shinyWidgets::searchInput(
  #   inputId = ns("Pancan_search"),
  #   label = NULL,
  #   btnSearch = icon("search"),
  #   btnReset = icon("remove"),
  #   placeholder = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
  #   width = "80%"
  # )
}

server.home_search_box <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$Pancan_search, {
    print(input$Pancan_search)
    if (nchar(input$Pancan_search) >= 1) {
      showModal(
        modalDialog(
          title = paste("Pancan distribution of gene", input$Pancan_search),
          size = "l",
          fluidPage(
            fluidRow(
              materialSwitch(ns("pdist_mode"), "Show violin plot", inline = TRUE),
              materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
              materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE)
            ),
            column(
              12,
              plotOutput(ns("gene_pancan_dist"))
            ),
            column(
              12,
              h4("NOTEs:"),
              h5("1. The data query may take some time based on your network. Wait until a plot shows"),
              h5("2. The unit of gene expression is log2(tpm+0.001)"),
              h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels")
            )
          )
        )
      )

      output$gene_pancan_dist <- renderPlot({
        vis_toil_TvsN(
          Gene = input$Pancan_search,
          Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
          Show.P.value = input$pdist_show_p_value,
          Show.P.label = input$pdist_show_p_label
        )
      })
    }
  })
}
