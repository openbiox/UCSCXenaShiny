ui.modules_pancan_til <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression vs TIL"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidRow(
          column(
            9,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("profile"), label = "Select a genomic profile:",
              choiceValues = c("mRNA", "transcript", "methylation","protein","miRNA", "cnv_gistic2"),
              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation","Protein Expression","miRNA Expression", "Copy Number Variation"),
              animation = "jelly"
            ),
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
            3,
            shinyWidgets::actionBttn(
              inputId = ns("search_bttn"), label = NULL,
              style = "simple",
              icon = icon("search"),
              color = "primary",
              block = FALSE,
              size = "sm"
            )
          )
        ),
        shinyBS::bsPopover(ns("Pancan_search"),
                           title = "Tips",
                           content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
                           placement = "right", options = list(container = "body")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("immune_sig"),
          label = "Cell types :", 
          selected = c("B cell_TIMER",
                      "T cell CD4+_TIMER",
                      "T cell CD8+_TIMER",
                      "Neutrophil_TIMER", 
                      "Macrophage_TIMER",
                      "Myeloid dendritic cell_TIMER"),
          choices = TIL_signatures,
          options = list(
            `actions-box` = TRUE),
          multiple = TRUE),
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
        hr(),
        h5("NOTEs:"),
        p("1. ", tags$a(href = "http://timer.cistrome.org/", "TIL data source")),
        p("2. ", tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source")),
        DT::DTOutput(outputId = ns("tbl")),
        shinyjs::hidden(
          wellPanel(
            id = ns("save_csv"),
            downloadButton(ns("downloadTable"), "Save as csv")
          )
        ),
        width = 9
      )
    )
  )
}

server.modules_pancan_til <- function(input, output, session) {
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
  
  profile_choices <- reactive({
    switch(input$profile,
           mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
           methylation = list(all = pancan_identifiers$gene, default = "TP53"),
           protein = list(all = pancan_identifiers$protein, default = "P53"),
           transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"), # 暂时
           miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
           cnv_gistic2 = list(all = pancan_identifiers$gene, default = "TP53"),
           list(all = "NONE", default = "NONE"))
  })
  
  
  observe({
    updateSelectizeInput(
      session,
      "Pancan_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default,
      server = TRUE
    )
  })
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("hm_gene_immune_cor"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_gene_TIL_cor(
        Gene = input$Pancan_search,
        sig = input$immune_sig,
        Cor_method = input$Cor_method,
        data_type = input$profile
      )
    }
    return(p)
  })
  

  output$hm_gene_immune_cor <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })

  
  ##return data
  return_data <- eventReactive(input$search_bttn,{
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
      p <- vis_gene_TIL_cor(
        Gene = input$Pancan_search,
        sig = input$immune_sig,
        Cor_method = input$Cor_method,
        data_type = input$profile
      )
      data <- p$data
      return(data)
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })
  

  output$tbl <- renderDT(
    data <- return_data(),
    options = list(lengthChange = FALSE)
  )

  
  ##downloadTable
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search,"_",input$profile,"_pancan_TIL.csv")
    },
    content = function(file) {
      write.csv(data <- return_data(), file, row.names = FALSE)
    }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search,"_",input$profile,"_pancan_TIL.", input$device)
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
