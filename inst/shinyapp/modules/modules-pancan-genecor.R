choices <- c(
  "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", "LUAD", "LUSC"
)

ui.modules_pancan_gene_cor <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: TCGA Gene-Gene Correlation"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectizeInput(
          inputId = ns("pancan_search1"),
          label = NULL,
          choices = NULL,
          width = "100%",
          options = list(
            create = TRUE,
            maxOptions = 5,
            placeholder = "Enter a gene symbol, e.g. CSF1R",
            plugins = list("restore_on_backspace")
          )
        ),
        selectizeInput(
          inputId = ns("pancan_search2"),
          label = NULL,
          choices = NULL,
          width = "100%",
          options = list(
            create = TRUE,
            maxOptions = 5,
            placeholder = "Enter a gene symbol, e.g. JAK3",
            plugins = list("restore_on_backspace")
          )
        ),
        shinyWidgets::actionBttn(
          inputId = ns("search_bttn"), label = NULL,
          style = "simple",
          icon = icon("search"),
          color = "primary",
          block = T,
          size = "sm"
        ),
        tags$br(),
        materialSwitch(ns("purity_adj"), "Adjust Purity", inline = TRUE),
        selectInput(inputId = ns("Cancer"), label = "Filter Cancer", choices = choices, selected = "ACC"),
        selectInput(
          inputId = ns("cor_method"),
          label = "Select Correlation method",
          choices = c("spearman", "pearson"),
          selected = "spearman"
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("gene_cor"), height = "600px"),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. The unit of gene expression is log2(tpm+0.001)"),
        p("3. You could choose correlation method or whether adjust tumor purity when calculating"),
        width = 6
      )
    )
  )
}

server.modules_pancan_gene_cor <- function(input, output, session) {
  ns <- session$ns

  observe({
    updateSelectizeInput(
      session,
      "pancan_search1",
      choices = pancan_identifiers$gene,
      selected = "CSF1R",
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "pancan_search2",
      choices = pancan_identifiers$gene,
      selected = "JAK3",
      server = TRUE
    )
  })

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_cor"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- reactive({
    if (nchar(input$pancan_search1) >= 1 & nchar(input$pancan_search2) >= 1) {
      p <- vis_gene_cor_cancer(
        Gene1 = input$pancan_search1,
        Gene2 = input$pancan_search2,
        purity_adj = input$purity_adj,
        cancer_choose = input$Cancer,
        cor_method = input$cor_method,
        split = FALSE
      )
    }
    p <- p + theme_classic(base_size = 15)

    return(p)
  })

  observeEvent(input$search_bttn, {
    output$gene_cor <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })
}
