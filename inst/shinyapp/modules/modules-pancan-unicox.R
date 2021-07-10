ui.modules_pancan_unicox <- function(id) {
  ns <- NS(id)
  fluidPage(
        fluidRow(
          column(
            3,
            wellPanel(
            shinyWidgets::prettyRadioButtons(
              inputId = ns("profile"), label = "Select a genomic profile:",
              choiceValues = c("mRNA", "transcript", "methylation", "protein", "miRNA", "cnv_gistic2"),
              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "Protein Expression", "miRNA Expression", "Copy Number Variation"),
              animation = "jelly"
            ),
            selectizeInput(
              inputId = ns("Pancan_search"),
              label = "Input a gene or formula (as signature)",
              choices = NULL,
              width = "100%",
              options = list(
                create = TRUE,
                maxOptions = 5,
                placeholder = "Enter a gene symbol, e.g. TP53",
                plugins = list("restore_on_backspace")
              )
            ),
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
        )),
        wellPanel(
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
        downloadBttn(
          outputId = ns("download"),
          style = "gradient",
          color = "default",
          block = TRUE,
          size = "sm"
        ))
      ),
      column(9,
        plotOutput(ns("unicox_gene_tree"), height = "500px",width= "350px"),
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
      cnv_gistic2 = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
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
        values = colors()
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
    plot_func()$data,
    options = list(lengthChange = FALSE)
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_", input$measure, "_pancan_unicox.csv")
    },
    content = function(file) {
      write.csv(plot_func()$data, file, row.names = FALSE)
    }
  )
}
