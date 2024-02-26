ui.modules_pancan_unicox <- function(id) {
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
        ),
      ),
      column(
        3,
        wellPanel(
          h4("2. Parameters", align = "center"),
          selectInput(inputId = ns("measure"), label = "Select Measure for plot", choices = c("OS", "PFI", "DSS", "DFI"), selected = "OS"),
          selectInput(inputId = ns("threshold"), label = "Select Threshold for plot", choices = c(0.25, 0.5), selected = 0.5),
          colourpicker::colourInput(inputId = ns("first_col"), "First color", "#6A6F68"),
          colourpicker::colourInput(inputId = ns("second_col"), "Second color", "#E31A1C"),
          colourpicker::colourInput(inputId = ns("third_col"), "Third color", "#377DB8"),
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
          numericInput(inputId = ns("height"), label = "Height", value = 8),
          numericInput(inputId = ns("width"), label = "Width", value = 6),
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
        )
      ),
      column(6,
             fluidRow(
              column(8, offset = 2,
               plotOutput(ns("unicox_gene_tree"), height = "600px")
              )
             ),
             # plotOutput(ns("unicox_gene_tree"), height = "600px"),
             hr(),
             h5("NOTEs:"),
             p("1. We define gene in certain cancer type as risky (log(Hazard Ratio) > 0) or protective (log(Hazard Ratio) < 0) or NS (No statistical significance, P value > 0.05)"),
             p("2. We divide patients into different groups for comparison according to gene expression, you could choose the threshold for grouping (0.5 by default)"),
             p("3. ", tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source")),
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


server.modules_pancan_unicox <- function(input, output, session) {
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
  w <- waiter::Waiter$new(id = ns("unicox_gene_tree"), html = waiter::spin_hexdots(), color = "white")
  
  colors <- reactive({
    c(input$first_col, input$second_col, input$third_col)
  })
  
  observeEvent(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })
  
  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_unicox_tree(
        Gene = input$Pancan_search,
        measure = input$measure,
        threshold = input$threshold,
        data_type = input$profile,
        values = colors(),
        opt_pancan = opt_pancan()
      )
      pdata <- p$data %>% 
        as.data.frame() %>%
        dplyr::select(cancer, measure, n_contrast, n_ref, beta, HR_log, lower_95_log, upper_95_log, Type, p.value)
      return(list(plot = p, data = pdata))
    }
  })
  
  output$unicox_gene_tree <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()$plot
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_", input$measure, "_pancan_unicox.", input$device)
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
      dplyr::rename('Cancer'='cancer', 'Event'='measure','Samples'='n_ref',
        'P.value' = 'p.value') %>%
      dplyr::select(Cancer,Event,Samples,HR_log,lower_95_log,upper_95_log,Type,P.value),
    options = list(lengthChange = FALSE)
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_", input$measure, "_pancan_unicox.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Cancer'='cancer', 'Event'='measure','Samples'='n_ref',
          'P.value' = 'p.value') %>%
        dplyr::select(Cancer,Event,Samples,HR_log,lower_95_log,upper_95_log,Type,P.value)
      write.csv(data, file, row.names = FALSE)
    }
  )
}
