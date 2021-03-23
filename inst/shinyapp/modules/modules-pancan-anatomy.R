ui.modules_pancan_anatomy <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression Anatomy Visualization"),
    fluidRow(
      tags$div(
        style = "margin-left: 30px;",
        fluidRow(
          shinyWidgets::searchInput(
            inputId = ns("Pancan_search"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          ),
          shinyBS::bsPopover(ns("Pancan_search"),
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
            selectInput(inputId = ns("Gender"), label = "Select Gender for plot", choices = c("Male", "Female"), selected = "Female"),
            selectInput(inputId = ns("Pal"), label = "Select Color Palettes for plot", choices = c("A", "B", "C", "D", "E"), selected = "D")
          ),
          fluidRow(
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
      ),
      mainPanel = mainPanel(
        column(
          10, wellPanel(
            plotOutput(ns("pancan_anatomy"))
          )
        )
      )
    )
  )
}

server.modules_pancan_anatomy <- function(input, output, session) {
  ns <- session$ns

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("pancan_anatomy"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_pancan_anatomy(
        Gene = input$Pancan_search,
        Gender = input$Gender,
        option = input$Pal
      )
      return(p)
    }
  })


  observeEvent(input$Pancan_search, {
    output$pancan_anatomy <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, " gene_pancan_anatomy.", input$device)
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
