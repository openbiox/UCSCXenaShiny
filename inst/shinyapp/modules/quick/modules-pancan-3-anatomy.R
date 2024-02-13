ui.modules_pancan_anatomy <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3,
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
            shinyBS::bsPopover(ns("Pancan_search"),
              title = "Tips",
              content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
              placement = "right", options = list(container = "body")
            ),
        ),
        wellPanel(
          h4("2. Parameters", align = "center"),
          selectInput(inputId = ns("Gender"), label = "Select Gender for plot", choices = c("Male", "Female"), selected = "Female"),
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
          numericInput(inputId = ns("width"), label = "Width", value = 10),
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
          ),
        ),
        # tags$a(href = "https://toil.xenahubs.net/", "Genomic profile data source"),
        # width = 3
      ),
      column(
        fluidRow(
          column(6, offset = 3,
            plotOutput(ns("pancan_anatomy"), height = "500px"))
        ),
        
        hr(),
        h5("NOTEs:"),
        # p("1. GISTIC2 thresholded copy number -2,-1,0,1,2, representing homozygous deletion,single copy deletion,diploid normal copy,low-level copy number amplification,or high-level copy number amplification"),
        # tags$br(),
        p("1. ",tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source")),
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

server.modules_pancan_anatomy <- function(input, output, session) {
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
  w <- waiter::Waiter$new(id = ns("pancan_anatomy"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      out <- vis_pancan_anatomy(
        Gene = input$Pancan_search,
        Gender = input$Gender,
        data_type = input$profile,
        opt_pancan = opt_pancan()
      )
      return(out)
    }
  })

  observeEvent(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })

  output$tbl <- renderDT(
    plot_func()$data %>%
      dplyr::rename('Cancer'='tissue', 'Group'='type.x','Organ'='organ','Value'='value') %>%
      dplyr::select(Cancer, Group, Organ, Value),
    options = list(lengthChange = FALSE)
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_pancan_anatomy.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Cancer'='tissue', 'Group'='type.x','Organ'='organ','Value'='value') %>%
        dplyr::select(Cancer, Group, Organ, Value)
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$pancan_anatomy <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()$plot
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_pancan_anatomy.", input$device)
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
}
