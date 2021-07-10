ui.modules_ccle_drug_response_diff <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3,
        wellPanel(
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
          materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
          colourpicker::colourInput(inputId = ns("high_col"), "High group color", "#DF2020"),
          colourpicker::colourInput(inputId = ns("low_col"), "Low group color", "#DDDF21"),
          selectInput(
            inputId = ns("tissue"), label = "Filter Tissue",
            choices = ccle_drug_related_tissues, selected = "lung",
            multiple = TRUE
          ),
          sliderTextInput(
            inputId = ns("alpha"),
            label = "Choose a transparent value",
            choices = seq(
              from = 0,
              to = 1,
              by = 0.1
            ),
            selected = "0.5",
            grid = TRUE
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
            style = "gradient",
            color = "default",
            block = TRUE,
            size = "sm"
          )
        )
      ),
      column(9,
        plotOutput(ns("gene_ccle_drug_response_diff"), height = "600px"),
        h5("Method:"),
        p("Analyze difference of drug response (IC50 value (uM)) between gene (or signature) high and low expression."),
        p("When there are multiple genes, geometrical mean of expression of these genes are used as a signature."),
        p("NOTEs: You can select multiple tissues, even ALL for all tissues.
          In this case 'number_of_cell_lines' indicates sample size for each gene-drug group instead of gene-drug-tissue group."),
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

server.modules_ccle_drug_response_diff <- function(input, output, session) {
  ns <- session$ns

  profile_choices <- reactive({
    switch("mRNA",
      mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
      protein = list(all = UCSCXenaShiny:::.all_ccle_proteins, default = "p53_Caution"),
      cnv = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "ccle_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default,
      server = TRUE
    )
  })

  colors <- reactive({
    c(input$high_col, input$low_col)
  })

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$ccle_search[1]) >= 1) {
      p <- vis_gene_drug_response_diff(
        Gene = input$ccle_search,
        values = colors(),
        tissue = input$tissue,
        Method = "wilcox.test",
        Show.P.label = input$pdist_show_p_value,
        alpha = input$alpha
      )
    }
    return(p)
  })

  output$gene_ccle_drug_response_diff <- renderPlot({
    plot_func()
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, "_ccle_target_response_diff.", input$device)
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

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, "_ccle_target_response_diff.csv")
    },
    content = function(file) {
      write.csv(plot_func()$data, file, row.names = FALSE)
    }
  )

  observeEvent(input$search_bttn, {
    if (nchar(input$ccle_search[1]) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })


  output$tbl <- renderDT(
    plot_func()$data,
    options = list(lengthChange = FALSE)
  )
}
