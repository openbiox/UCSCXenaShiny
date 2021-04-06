ui.modules_pancan_immune <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression vs Immune Gene Signature"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
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
        ),
        shinyBS::bsPopover(ns("Pancan_search"),
          title = "Tips",
          content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
          placement = "right", options = list(container = "body")
        ),
        selectInput(
          inputId = ns("immune_sig"), "Select the immune signature source", selected = "Cibersort",
          choices = c("Yasin", "Wolf", "Attractors", "ICR", "c7atoms", "Bindea", "Cibersort")
        ),
        selectInput(
          inputId = ns("Cor_method"),
          label = "Select Correlation method",
          choices = c("spearman", "pearson"),
          selected = "spearman"
        ),
        numericInput(inputId = ns("height"), label = "Height", value = 8),
        numericInput(inputId = ns("width"), label = "Width", value = 12),
        prettyRadioButtons(
          inputId = ns("device"),
          label = "Choose plot format",
          choices = c("pdf", "png"),
          selected = "pdf",
          inline = TRUE,
          icon = icon("check"),
          animation = "jelly",
          fill = TRUE
        ),
        downloadBttn(
          outputId = ns("download"),
          # label = "Download Plot",
          style = "gradient",
          color = "default",
          block = TRUE,
          size = "sm"
        ),
        width = 3
      ),
      mainPanel(
        plotOutput(ns("hm_gene_immune_cor"), height = "500px"),
        width = 9
      )
    )
  )
}

server.modules_pancan_immune <- function(input, output, session) {
  # observeEvent(input$Pancan_search, {
  #   if (nchar(input$Pancan_search) >= 1) {
  #     output$hm_gene_immune_cor <- renderPlot({
  #       vis_gene_immune_cor(
  #         Gene = input$Pancan_search,
  #         Immune_sig_type = input$immune_sig,
  #         Cor_method = input$Cor_method
  #       )
  #     })
  #   }
  # })
  #
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
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("hm_gene_immune_cor"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_gene_immune_cor(
        Gene = input$Pancan_search,
        Immune_sig_type = input$immune_sig,
        Cor_method = input$Cor_method
      )
    }
    return(p)
  })

  observeEvent(input$Pancan_search, {
    # output$colorvalues = reactive({c(input$tumor_col,input$normal_col)
    #   })
    output$hm_gene_immune_cor <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, " gene_pancan_immune.", input$device)
    },
    content = function(file) {
      p <- plot_func()
      if (input$device == "pdf") {
        pdf(file, width = input$width, height = input$height)
        print(p)
        dev.off()
      } else {
        png(file, width = input$width, height = input$height, res = 300, units = "in")
        print(p)
        dev.off()
      }

      # ggplot2::ggsave(filename = file, plot = print(p), device = input$device, width = input$width, height = input$height, dpi = 600)
    }
  )
}
