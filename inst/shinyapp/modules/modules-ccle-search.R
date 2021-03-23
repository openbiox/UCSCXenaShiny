ccle_choices <- c(
  "Gender", "Site_Primary", "Histology", "Hist_Subtype1", "Source", "Expression_arrays", "SNP_arrays", "Hybrid_Capture_Sequencing", "Type"
)

ui.modules_ccle_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene CCLE Expression Distribution"),
    fluidRow(
      tags$div(
        style = "margin-left: 30px;",
        fluidRow(
          shinyWidgets::searchInput(
            inputId = ns("ccle_search"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          ),
          shinyBS::bsPopover(ns("ccle_search"),
            title = "Tips",
            content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
            placement = "right", options = list(container = "body")
          )
        )
      )
    ),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidPage(
          fluidRow(
            selectInput(inputId = ns("x.axis"), label = "Select Parameter to display in x.axis", choices = ccle_choices, selected = "Type"),
            fluidRow(
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
              )
            )
          )
        )
      ),
      mainPanel = mainPanel(
        column(
          12, wellPanel(
            plotOutput(ns("gene_ccle_dist"), height = "600px")
          )
        ),
      )
    )
  )
}

server.modules_ccle_dist <- function(input, output, session) {
  ns <- session$ns

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_ccle_dist"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- reactive({
    if (nchar(input$ccle_search) >= 1) {
      p <- vis_ccle_tpm(
        Gene = input$ccle_search,
        x.axis = input$x.axis
      )
    }
    return(p)
  })

  observeEvent(input$ccle_search, {
    output$gene_ccle_dist <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search, " gene_ccle_dist.", input$device)
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
