ui.modules_ccle_drug_target_asso <- function(id) {
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
        selectInput(inputId = ns("output_form"), 
                    label = "Plot output form", 
                    choices = c("plotly","ggplot2"), 
                    selected = "plotly"),
        selectInput(inputId = ns("x_axis_type"), 
                    label = "X axis type ", 
                    choices = c("mean.diff","median.diff"), 
                    selected = "mean.diff"),
        numericInput(inputId = ns("height"), label = "Height", value = 6),
        numericInput(inputId = ns("width"), label = "Width", value = 8),
        prettyRadioButtons(
          inputId = ns("device"),
          label = "Choose plot format (only support ggplot2)",
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
      mainPanel = mainPanel(
      #   tabsetPanel(
      #     tabPanel("Plotly", plotly::plotlyOutput("gene_ccle_drug_target")), 
      #     tabPanel("ggplot2", plotOutput("gene_ccle_drug_target"))
      # )
        plotly::plotlyOutput(ns("gene_ccle_drug_target"), height = "600px")
      )
    )
  )
}

server.modules_ccle_drug_target_asso <- function(input, output, session) {
  ns <- session$ns
  
  # profile_choices <- reactive({
  #   switch("mRNA",
  #          mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
  #          protein = list(all = UCSCXenaShiny:::.all_ccle_proteins, default = "p53_Caution"),
  #          cnv = list(all = pancan_identifiers$gene, default = "TP53"),
  #          list(all = "NONE", default = "NONE"))
  # })
  
  observe({
    updateSelectizeInput(
      session,
      "ccle_search",
      choices = pancan_identifiers$gene,
      selected = "TP53",
      server = TRUE
    )
  })
  
  # Show waiter for plot
  #w <- waiter::Waiter$new(id = ns("gene_ccle_drug_target"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$ccle_search[1]) >= 1) {
      p <- vis_gene_drug_response_asso(Gene = input$ccle_search, output_form = input$output_form, x_axis_type = input$x_axis_type)
    }
    return(p)
  })
  
  output$gene_ccle_drug_target <- plotly::renderPlotly({
    plot_func()
  })
  
  # output$gene_ccle_drug_target <- renderPlot({
  #   plot_func2()
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search,"_ccle_target_response.", input$device)
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
    }
  )
  
}