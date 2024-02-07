ui.modules_ccle_drug_target_asso <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("1. Data", align = "center"),
          virtualSelectInput(
            inputId = ns("ccle_search"),
            label = "Input a gene or list (as signature)",
            choices = NULL,
            multiple = TRUE,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
        ),
        wellPanel(
          h4("2. Parameters", align = "center"),
          selectInput(
            inputId = ns("output_form"),
            label = "Plot output form",
            choices = c("plotly", "ggplot2"),
            selected = "plotly"
          ),
          selectInput(
            inputId = ns("x_axis_type"),
            label = "X axis type ",
            choices = c("mean.diff", "median.diff"),
            selected = "mean.diff"
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
          numericInput(inputId = ns("height"), label = "Height", value = 6),
          numericInput(inputId = ns("width"), label = "Width", value = 8),
          prettyRadioButtons(
            inputId = ns("device"),
            label = "Choose plot format (only support ggplot2)",
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
      column(
        width = 9,
        shinyjs::hidden(
          plotly::plotlyOutput(ns("gene_ccle_drug_target.plotly"), height = "600px")
        ),
        shinyjs::hidden(
          plotOutput(ns("gene_ccle_drug_target.ggplot2"), height = "600px")
        ),
        hr(),
        h5("Method:"),
        p("Analyze partial correlation of gene-drug association after controlling for tissue average expression."),
        p("When there are multiple genes, geometrical mean of expression of these genes are used as a signature."),
        p("NOTEs: CCLE gene expression data was quantile normalized among all different cell lines for partial correlation,
          and then Z-score normalization was applied in each tissue to calculate the expression difference between High-Low (use median as cutoff) IC50 groups.
          The X axis indicates mean/median expression difference across tissues."),
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

server.modules_ccle_drug_target_asso <- function(input, output, session) {
  ns <- session$ns

  observe({
    updateVirtualSelect(
      "ccle_search",
      choices = pancan_identifiers$gene,
      selected = "TP53"
    )
  })

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$ccle_search[1]) >= 1) {
      p <- vis_gene_drug_response_asso(
        Gene = input$ccle_search,
        output_form = input$output_form,
        x_axis_type = input$x_axis_type
      )
      print(class(p))
    }
    return(p)
  })

  observeEvent(input$search_bttn, {
    if (input$output_form == "ggplot2") {
      shinyjs::hide("gene_ccle_drug_target.plotly")
      shinyjs::show("gene_ccle_drug_target.ggplot2")
    } else {
      shinyjs::hide("gene_ccle_drug_target.ggplot2")
      shinyjs::show("gene_ccle_drug_target.plotly")
    }
  })

  output$gene_ccle_drug_target.plotly <- plotly::renderPlotly({
    plot_func()
  })
  output$gene_ccle_drug_target.ggplot2 <- renderPlot({
    plot_func()
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, "_ccle_drug_target.", input$device)
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
      paste0(input$ccle_search, "_ccle_drug_target.csv")
    },
    content = function(file) {
      write.csv(return_data(), file, row.names = FALSE)
    }
  )

  ## return data
  return_data <- eventReactive(input$search_bttn, {
    if (nchar(input$ccle_search[1]) >= 1) {
      shinyjs::show(id = "save_csv")
      data <- analyze_gene_drug_response_asso(input$ccle_search, combine = TRUE)
      return(data)
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })


  output$tbl <- renderDT(
    return_data(),
    options = list(lengthChange = FALSE)
  )
}
