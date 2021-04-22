choices <- c(
  "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", "LUAD", "LUSC"
)

ui.modules_pancan_gene_cor <- function(id) {
  ns <- NS(id)
  fluidPage(
    #titlePanel("Module: TCGA Gene-Gene Correlation"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        shinyWidgets::prettyRadioButtons(
          inputId = ns("profile1"), label = "Select a genomic profile:",
          choiceValues = c("mRNA", "transcript", "methylation","protein","miRNA", "cnv_gistic2"),
          choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation","Protein Expression","miRNA Expression", "Copy Number Variation"),
          animation = "jelly"
        ),
        selectizeInput(
          inputId = ns("Pancan_search1"),
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
        shinyWidgets::prettyRadioButtons(
          inputId = ns("profile2"), label = "Select a genomic profile:",
          choiceValues = c("mRNA", "transcript", "methylation","protein","miRNA", "cnv_gistic2"),
          choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation","Protein Expression","miRNA Expression", "Copy Number Variation"),
          animation = "jelly"
        ),
        selectizeInput(
          inputId = ns("Pancan_search2"),
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
        materialSwitch(ns("use_regline"), "Use regression line", inline = TRUE),
        selectInput(
          inputId = ns("cor_method"),
          label = "Select Correlation method",
          choices = c("spearman", "pearson"),
          selected = "spearman"
        ),
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
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("gene_cor"), height = "600px"),
        hr(),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. You could choose correlation method or whether adjust tumor purity when calculating"),
        p("3. ", tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source"))
      )
    )
  )
}

server.modules_pancan_gene_cor <- function(input, output, session) {
  ns <- session$ns
  
  profile_choices1 <- reactive({
    switch(input$profile1,
           mRNA = list(all = pancan_identifiers$gene, default = "CSF1R"),
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
      "Pancan_search1",
      choices = profile_choices1()$all,
      selected = profile_choices1()$default,
      server = TRUE
    )
  })
  
  profile_choices2 <- reactive({
    switch(input$profile2,
           mRNA = list(all = pancan_identifiers$gene, default = "JAK3"),
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
      "Pancan_search2",
      choices = profile_choices2()$all,
      selected = profile_choices2()$default,
      server = TRUE
    )
  })
  
  
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_cor"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$Pancan_search1) >= 1 & nchar(input$Pancan_search2) >= 1) {
      p <- vis_gene_cor_cancer(
        Gene1 = input$Pancan_search1,
        Gene2 = input$Pancan_search2,
        data_type1 = input$profile1,
        data_type2 = input$profile2,
        purity_adj = input$purity_adj,
        cancer_choose = input$Cancer,
        cor_method = input$cor_method,
        use_regline = input$use_regline
      )
    }
    p <- p + theme_classic(base_size = 20) +
      ggplot2::theme(legend.position = "none")

    return(p)
  })


  output$gene_cor <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })
  
  #download module
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search1,"_",input$profile1,"_",input$Pancan_search2,"_",input$profile2,"_pancan_gene_cor.", input$device)
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
