ui.modules_ccle_dist <- function(id) {
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
            mol_origin_UI(ns("mol_origin2quick"), database = "ccle")
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("profile"), label = "Select a genomic profile:",
            choiceValues = c("mRNA", "protein", "cnv"),
            choiceNames = c("mRNA Expression", "Protein Expression", "Copy Number Variation"),
            animation = "jelly"
          ),
          virtualSelectInput(
            inputId = ns("ccle_search"),
            label = NULL,
            choices = NULL,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyBS::bsPopover(ns("ccle_search"),
            title = "Tips",
            content = "Enter a gene symbol to show its distribution, e.g. TP53",
            placement = "right", options = list(container = "body")
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
          h4("2. Download", align = "center"),
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
          ),
          hr(),
          tags$a(href = "https://xenabrowser.net/datapages/?cohort=Cancer%20Cell%20Line%20Encyclopedia%20(CCLE)&removeHub=https%3A%2F%2Ficgc.xenahubs.net", "Genomic profile data source"),
        )
      ),
      column(
        9,
        plotOutput(ns("gene_ccle_dist"), height = "600px"),
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

server.modules_ccle_dist <- function(input, output, session) {
  ns <- session$ns

  profile_choices <- reactive({
    switch(input$profile,
      mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
      protein = list(all = UCSCXenaShiny:::.all_ccle_proteins, default = "p53_Caution"),
      cnv = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
  })

  observe({
    updateVirtualSelect(
      "ccle_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default
    )
  })

  opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database= "ccle")

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_ccle_dist"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$ccle_search) >= 1) {
      p <- vis_ccle_tpm(
        Gene = input$ccle_search,
        data_type = input$profile,
        opt_pancan = opt_pancan()
      )
    }
    return(p)
  })


  output$gene_ccle_dist <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })


  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, "_gene_ccle_dist.", input$device)
    },
    content = function(file) {
      p <- plot_func()
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

  ## return data
  observeEvent(input$search_bttn, {
    if (nchar(input$ccle_search) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })

  output$tbl <- renderDT(
    plot_func()$data %>%
      dplyr::rename('Cell_line'='cell','Value'='tpm') %>%
      dplyr::select(Cell_line, Cell_line_aliases, Site_Primary, Value),
    options = list(lengthChange = FALSE)
  )

  ## downloadTable
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, "_gene_ccle_dist.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Cell_line'='cell','Value'='tpm') %>%
        dplyr::select(Cell_line, Cell_line_aliases, Site_Primary, Value)
      write.csv(plot_func()$data, file, row.names = FALSE)
    }
  )
}
