ui.modules_pancan_radar <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("1. Data", align = "center"),
          div(actionButton(ns("toggleBtn"), "Modify datasets[opt]",icon = icon("folder-open")),
              style = "margin-bottom: 5px;"),
          conditionalPanel(
            ns = ns,
            condition = "input.toggleBtn % 2 == 1",
            mol_origin_UI(ns("mol_origin2quick"), database = "toil")
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("profile"), label = "Select a genomic profile:",
            choiceValues = c("mRNA", "transcript", "methylation", "protein", "miRNA", "cnv"),
            choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "Protein Expression", "miRNA Expression", "Copy Number Variation"),
            animation = "jelly"
          ),

          virtualSelectInput(
            inputId = ns("Pancan_search"),
            label = "Input a gene or formula (as signature)",
            choices = NULL,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          selectInput(inputId = ns("Type"), label = "Select a feature", choices = c("stemness", "TMB", "MSI"), selected = "stemness"),
        ),
        wellPanel( 
          h4("2. Parameters", align = "center"), 
          selectInput(
            inputId = ns("cor_method"),
            label = "Select Correlation method",
            choices = c("spearman", "pearson"),
            selected = "spearman"
          ),
          tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
          shinyWidgets::actionBttn(
            inputId = ns("search_bttn"),
            label = "Go!",
            style = "gradient",
            icon = icon("search"),
            color = "primary",
            block = TRUE,
            size = "sm"
          )
        ),
        wellPanel(
          h4("3. Download", align = "center"),
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
          tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
          downloadBttn(
            outputId = ns("download"),
            style = "gradient",
            color = "primary",
            block = TRUE,
            size = "sm"
          )
        ),
      ),
      column(
        9,
        plotOutput(ns("gene_pancan_radar"), height = "500px"),
        hr(),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. If a void plot shows, please check your input"),
        p("3. ", tags$a(href = "https://toil.xenahubs.net/", "Genomic profile data source")),
        p("4. Check description of ", tags$a(href = "https://zenodo.org/record/5531172", "this link"), " for TMB/Stemness/MSI data source"),
        tags$br(),
        DT::DTOutput(outputId = ns("tbl")),
        shinyjs::hidden(
          wellPanel(
            id = ns("save_csv"),
            downloadButton(ns("downloadTable"), "Save as csv")
          )
        )
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
      transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"),
      miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
      cnv = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
  })

  observe({
    updateVirtualSelect(
      "Pancan_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default
    )
  })

  opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database = "toil")

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_pancan_radar"), html = waiter::spin_hexdots(), color = "black")

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      vis_fun <- switch(input$Type,
        stemness = vis_gene_stemness_cor,
        TMB = vis_gene_tmb_cor,
        MSI = vis_gene_msi_cor
      )

      p <- vis_fun(Gene = input$Pancan_search, cor_method = input$cor_method, 
                   data_type = input$profile, opt_pancan = opt_pancan())
      
      if (is.null(p)) {
        sendSweetAlert(session, title = "Warning", text = "No data could be queried!")
        return(NULL)
      }

      pdata <- p$data %>%
        dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))
      df <- pdata %>%
        select(cor, cancer) %>%
        pivot_wider(names_from = cancer, values_from = cor) %>%
        tibble::rownames_to_column()
      
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
        plot.title = paste0(input$Pancan_search, " ", input$profile, " ", input$Type, " ")
      ) + theme(plot.title = element_text(hjust = .5))
      return(list(plot = plot, data = pdata))
    } else {
      sendSweetAlert(session, title = "Warning", text = "You must input something before analysis!")
    }
  })

  output$gene_pancan_radar <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()$plot
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_", input$Type, "_pancan_radar.", input$device)
    },
    content = function(file) {
      p <- plot_func()$plot
      if (input$device == "pdf") {
        pdf(file, width = input$width, height = input$height)
        print(p)
        dev.off()
      } else {
        png(file, width = input$width, height = input$height, res = 600, units = "in")
        print(p)
        dev.off()
      }
    }
  )

  output$tbl <- renderDT(
    plot_func()$data %>%
      dplyr::rename('Cancer'='cancer',
        'Cor'='cor', 'P.value'='p.value') %>%
      dplyr::select(Cancer, Cor, P.value) %>%
      tibble::remove_rownames(),
    options = list(lengthChange = FALSE)
  )

  observeEvent(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })

  ## downloadTable
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_", input$Type, "_pancan_radar.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Cancer'='cancer',
          'Cor'='cor', 'P.value'='p.value') %>%
        dplyr::select(Cancer, Cor, P.value) %>%
        tibble::remove_rownames()
      write.csv(plot_func()$data, file, row.names = FALSE)
    }
  )
}
