ui.modules_pancan_radar <- function(id) {
  ns <- NS(id)
  fluidPage(
    #titlePanel("Module: Gene Pancan Radar"),
    sidebarLayout(
      sidebarPanel(
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
            ),
            # actionButton(ns("search_bttn"), "Go"),
          )
        ),
        shinyBS::bsPopover(ns("Pancan_search"),
                           title = "Tips",
                           content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
                           placement = "right", options = list(container = "body")
        ),
        selectInput(inputId = ns("Type"), label = "Select a feature", choices = c("stemness","TMB","MSI"),selected = "stemness"),
        selectInput(
          inputId = ns("cor_method"),
          label = "Select Correlation method",
          choices = c("spearman", "pearson"),
          selected = "spearman"
        ),
        numericInput(inputId = ns("height"), label = "Height", value = 5),
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
      mainPanel = mainPanel(
        plotOutput(ns("gene_pancan_radar"), height = "500px"),
        hr(),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. If a void plot shows, please check your input"),
        p("3. ", tags$a(href = "https://toil.xenahubs.net/", "Genomic profile data source")),
        width = 9
      )
    )
  )
}




server.modules_pancan_radar <- function(input, output, session) {
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
  w <- waiter::Waiter$new(id = ns("gene_pancan_radar"), html = waiter::spin_hexdots(), color = "black")
  
  plot_func <- eventReactive(input$search_bttn,{
    if (nchar(input$Pancan_search) >= 1) {
      if(input$Type == "stemness"){
        p <- vis_gene_stemness_cor(Gene = input$Pancan_search, cor_method = input$cor_method,data_type = input$profile)
      } else if(input$Type == "tmb"){
        p <- vis_gene_tmb_cor(Gene = input$Pancan_search, cor_method = input$cor_method,data_type = input$profile)
      } else if(input$Type == "msi"){
        p <- vis_gene_msi_cor(Gene = input$Pancan_search, cor_method = input$cor_method,data_type = input$profile)
      }
      pdata = p$data
      df = pdata %>% select(cor, cancer) %>% pivot_wider(names_from = cancer, values_from = cor)
      plot <- ggradar::ggradar(
        df[1, ], 
        font.radar = "sans",
        values.radar = c("-1", "0", "1"),
        grid.min = -1, grid.mid = 0, grid.max = 1,
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        # Polygons
        group.line.width = 1, 
        group.point.size = 3,
        group.colours = "#00AFBB",
        plot.title = paste0(input$Pancan_search," ",input$profile," ",input$Type," ")
      ) + theme(title = element_text(hjust = .5))
      
    }
    return(plot)
  })
  
  output$gene_pancan_radar <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search,"_",input$profile,"_pancan_radar.", input$device)
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
