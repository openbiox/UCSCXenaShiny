ui.modules_pancan_unicox <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Uni-cox analysis"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
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
        ),
        shinyBS::bsPopover(ns("Pancan_search"),
          title = "Tips",
          content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
          placement = "right", options = list(container = "body")
        ),
        selectInput(inputId = ns("measure"), label = "Select Measure for plot", choices = c("OS", "PFI", "DSS", "DFI"), selected = "OS"),
        selectInput(inputId = ns("threshold"), label = "Select Threshold for plot", choices = c(0.25, 0.5), selected = 0.5),
        colourpicker::colourInput(inputId = ns("first_col"), "First color", "#6A6F68"),
        colourpicker::colourInput(inputId = ns("second_col"), "Second color", "#E31A1C"),
        colourpicker::colourInput(inputId = ns("third_col"), "Third color", "#377DB8"),
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
          # label = "Download Plot",
          style = "gradient",
          color = "default",
          block = TRUE,
          size = "sm"
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput(ns("unicox_gene_tree"), height = "500px"),
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


server.modules_pancan_unicox <- function(input, output, session) {
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
  w <- waiter::Waiter$new(id = ns("unicox_gene_tree"), html = waiter::spin_hexdots(), color = "white")

  colors <- reactive({
    c(input$first_col, input$second_col, input$third_col)
  })

  return_data <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
      p <- vis_unicox_tree(
        Gene = input$Pancan_search,
        measure = input$measure,
        threshold = input$threshold,
        values = colors()
      )
      data <- p$data
      data <- data %>%
        as.data.frame() %>%
        dplyr::select(cancer, measure, n_contrast, n_ref, beta, HR_log, lower_95_log, upper_95_log, Type, p.value)
      return(data)
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })


  plot_func <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_unicox_tree(
        Gene = input$Pancan_search,
        measure = input$measure,
        threshold = input$threshold,
        values = colors()
      )
      
      p = p + theme_cowplot()
      
    }

    return(p)
  })

  observeEvent(input$Pancan_search, {
    # output$colorvalues = reactive({c(input$tumor_col,input$normal_col)
    #   })
    output$unicox_gene_tree <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, " gene_pancan_unicox.", input$device)
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


  output$tbl <- renderDT(
    data <- return_data(),
    options = list(lengthChange = FALSE)
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$measure, "_gene_pancan_unicox.csv")
    },
    content = function(file) {
      write.csv(data <- return_data(), file, row.names = FALSE)
    }
  )
}
