ui.modules_ccle_drug_response_diff <- function(id) {
  ns <- NS(id)
  fluidPage(
    #titlePanel("Module: Gene CCLE Expression Distribution"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidRow(
          column(
            9,
            selectizeInput(
              inputId = ns("ccle_search"),
              label = "Input a gene or list (as signature)",
              choices = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                create = TRUE,
                maxOptions = 5,
                placeholder = "Enter a gene symbol, e.g. TP53",
                plugins = list("restore_on_backspace")
              )
            ),
          ),
          column(
            3,
            shinyWidgets::actionBttn(
              inputId = ns("search_bttn"), label = NULL,
              style = "simple",
              icon = icon("search"),
              color = "primary",
              block = FALSE,
              size = "sm"
            ),
            # actionButton(ns("search_bttn"), "Go"),
          ),
          shinyBS::bsPopover(ns("ccle_search"),
                             title = "Tips",
                             content = "Enter a gene symbol to show its distribution, e.g. TP53",
                             placement = "right", options = list(container = "body")
          ),
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("gene_ccle_drug_response_diff"), height = "600px")
      )
    )
  )
}

server.modules_ccle_drug_response_diff <- function(input, output, session) {
  ns <- session$ns
  
  profile_choices <- reactive({
    switch("mRNA",
           mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
           protein = list(all = UCSCXenaShiny:::.all_ccle_proteins, default = "p53_Caution"),
           cnv = list(all = pancan_identifiers$gene, default = "TP53"),
           list(all = "NONE", default = "NONE"))
  })
  
  observe({
    updateSelectizeInput(
      session,
      "ccle_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default,
      server = TRUE
    )
  })
  
  # Show waiter for plot
  #w <- waiter::Waiter$new(id = ns("gene_ccle_drug_target"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$ccle_search[1]) >= 1) {
      p <- vis_gene_drug_response_diff(Gene = input$ccle_search)
    }
    return(p)
  })
  
  output$gene_ccle_drug_response_diff <- renderPlot({
    plot_func()
  })
  
}