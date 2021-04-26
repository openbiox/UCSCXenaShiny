ui.home_search_box <- function(id) {
  ns <- NS(id)

  # ref: https://shiny.rstudio.com/articles/selectize.html
  # https://stackoverflow.com/questions/51343552/dynamic-selectizeinput-in-shiny
  fluidRow(
    column(
      8,
      selectizeInput(
        inputId = ns("Pancan_search"),
        label = NULL,
        choices = NULL,
        width = "100%",
        options = list(
          create = TRUE,
          maxOptions = 5,
          placeholder = "Enter a gene symbol, e.g. TP53",
          plugins = list("restore_on_backspace")
        )
      )
    ),
    column(
      4,
      actionBttn(
        inputId = ns("search"),
        label = "Go!",
        color = "primary",
        style = "bordered",
        size = "sm"
      )
    )
  )
}

server.home_search_box <- function(input, output, session) {
  ns <- session$ns
  observe({
    updateSelectizeInput(
      session,
      "Pancan_search",
      choices = pancan_identifiers$gene,
      selected = "TP53",
      server = TRUE
    )
  })

  observeEvent(input$search, {
    message(input$Pancan_search, " is queried by user from home search box.")
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
              h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels"),
              h5("4. If a void plot shows, please check your input"),
              h5("5. You can get more features for this plot in page 'Quick PanCan Analysis'")
            )
          )
        )
      )

      output$gene_pancan_dist <- renderPlot({
        p <- vis_toil_TvsN(
          Gene = input$Pancan_search,
          Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
          Show.P.value = input$pdist_show_p_value,
          Show.P.label = input$pdist_show_p_label
        )

        p + cowplot::theme_cowplot() + ggpubr::rotate_x_text(45)
      })
    }
  })
}
