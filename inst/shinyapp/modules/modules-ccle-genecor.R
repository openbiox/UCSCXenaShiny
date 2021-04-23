ui.modules_ccle_genecor <- function(id) {
  ns <- NS(id)
  fluidPage(
    #titlePanel("Module: Gene CCLE Expression Correlation"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidRow(
          column(
            9,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("profile1"), label = "Select a genomic profile:",
              choiceValues = c("mRNA"),
              choiceNames = c("mRNA Expression"),
              # choiceValues = c("mRNA", "protein","cnv"),
              # choiceNames = c("mRNA Expression",  "Protein Expression", "Copy Number Variation"),
              animation = "jelly"
            ),
            selectizeInput(
              inputId = ns("ccle_search1"),
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
            shinyWidgets::prettyRadioButtons(
              inputId = ns("profile2"), label = "Select a genomic profile:",
              choiceValues = c("mRNA"),
              choiceNames = c("mRNA Expression"),
              # choiceValues = c("mRNA", "protein","cnv"),
              # choiceNames = c("mRNA Expression",  "Protein Expression", "Copy Number Variation"),
              animation = "jelly"
            ),
            selectizeInput(
              inputId = ns("ccle_search2"),
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
        selectInput(
          inputId = ns("cor_method"),
          label = "Select Correlation method",
          choices = c("spearman", "pearson"),
          selected = "spearman"
        ),
        materialSwitch(ns("use_log_x"), "x axis log", inline = FALSE),
        materialSwitch(ns("use_log_y"), "y axis log", inline = FALSE),
        materialSwitch(ns("use_regline"), "Use regression line", inline = TRUE),
        sliderTextInput(
          inputId = ns("alpha"),
          label = "Choose a transparent value", 
          choices = seq(from = 0,
                        to = 1,
                        by = 0.1),
          selected = "0.5",  
          grid = TRUE
        ),
        colourpicker::colourInput(inputId = ns("color"), "Point color", "#000000"),
        tags$br(),
        #selectInput(inputId = ns("phenotype"), label = "phenotype", choices = ccle_choices, selected = "Type"),
        numericInput(inputId = ns("height"), label = "Height", value = 8),
        numericInput(inputId = ns("width"), label = "Width", value = 8),
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
        hr(),
        tags$a(href = "https://xenabrowser.net/datapages/?cohort=Cancer%20Cell%20Line%20Encyclopedia%20(CCLE)&removeHub=https%3A%2F%2Ficgc.xenahubs.net", "Genomic profile data source"),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("gene_ccle_gene_cor"), height = "600px"),
        width = 6
      )
    )
  )
}

server.modules_ccle_genecor <- function(input, output, session) {
  ns <- session$ns
  
  profile_choices1 <- reactive({
    switch(input$profile1,
           mRNA = list(all = pancan_identifiers$gene, default = "CSF1R"),
           methylation = list(all = pancan_identifiers$gene, default = "CSF1R"),
           protein = list(all = pancan_identifiers$protein, default = "P53"),
           transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"), # 暂时
           miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
           cnv_gistic2 = list(all = pancan_identifiers$gene, default = "TP53"),
           list(all = "NONE", default = "NONE"))
  })
  
  observe({
    updateSelectizeInput(
      session,
      "ccle_search1",
      choices = profile_choices1()$all,
      selected = profile_choices1()$default,
      server = TRUE
    )
  })
  
  profile_choices2 <- reactive({
    switch(input$profile2,
           mRNA = list(all = pancan_identifiers$gene, default = "JAK3"),
           methylation = list(all = pancan_identifiers$gene, default = "JAK3"),
           protein = list(all = pancan_identifiers$protein, default = "P53"),
           transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"), # 暂时
           miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
           cnv_gistic2 = list(all = pancan_identifiers$gene, default = "TP53"),
           list(all = "NONE", default = "NONE"))
  })
  
  observe({
    updateSelectizeInput(
      session,
      "ccle_search2",
      choices = profile_choices2()$all,
      selected = profile_choices2()$default,
      server = TRUE
    )
  })
  
  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$ccle_search1) >= 1 & nchar(input$ccle_search2) >= 1) {
      p <- vis_ccle_gene_cor(
        Gene1 = input$ccle_search1,
        Gene2 = input$ccle_search2,
        data_type1 = input$profile1,
        data_type2 = input$profile2,
        cor_method = input$cor_method,
        use_log_x = input$use_log_x,
        use_log_y = input$use_log_y,
        use_regline = input$use_regline,
        color = input$color,
        alpha = input$alpha
      )
    }
    p <- p + theme_classic(base_size = 20) +
      ggplot2::theme(legend.position = "none")
    
    return(p)
  })
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_ccle_gene_cor"), html = waiter::spin_hexdots(), color = "white")
  
  output$gene_ccle_gene_cor <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, " gene_ccle_genecor.", input$device)
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
