ui.modules_pancan_anatomy <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression Anatomy Visualization"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidRow(
          column(9,
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
        )),
      column(3,
        shinyWidgets::actionBttn(
          inputId = ns("search_bttn"), label = NULL,
          style = "simple",
          icon = icon("search"),
          color = "primary",
          block = FALSE,
          size = "sm")
        ))
        ,
        shinyBS::bsPopover(ns("Pancan_search"),
          title = "Tips",
          content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
          placement = "right", options = list(container = "body")
        ),

        selectInput(inputId = ns("Gender"), label = "Select Gender for plot", choices = c("Male", "Female"), selected = "Female"),
        selectInput(inputId = ns("Pal"), label = "Select Color Palettes for plot", choices = c("A", "B", "C", "D", "E"), selected = "D"),
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
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("pancan_anatomy"), height = "500px"),
        width = 9
      )
    )
  )
}

server.modules_pancan_anatomy <- function(input, output, session) {
  ns <- session$ns
  
  observe({
    updateSelectizeInput(
      session,
      "Pancan_search",
      choices = pancan_identifiers$gene,
      selected = "TP53",
      server = TRUE
    )
  })
  
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


  observeEvent(input$search_bttn, {
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
