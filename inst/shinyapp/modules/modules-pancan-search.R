
themes <- list("Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "Classic" = theme_classic(),
               "Gray" = theme_gray())

ui.modules_pancan_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Pancan Expression Distribution"),
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
                             placement = "right", options = list(container = "body"))
        )
    )),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fluidPage(
          fluidRow(
            materialSwitch(ns("pdist_mode"), "Show violin plot", inline = TRUE),
            materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
            materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE),
            materialSwitch(ns("pdist_dataset"), "TCGA Dataset only", inline = FALSE),
            colourpicker::colourInput(inputId = ns("tumor_col"), "Tumor sample color", "#DF2020"),
            colourpicker::colourInput(inputId = ns("normal_col"), "Normal sample color",  "#DDDF21"),
            selectInput(inputId = ns("theme"), label = "Select theme for plot", choices = names(themes), selected = "Light"),
            fluidRow(
              numericInput(inputId = ns("height"),label = "Height",value = 5),
              numericInput(inputId = ns("width"),label = "Width",value = 12),
              prettyRadioButtons(
                inputId = ns("device"),
                label = "Choose plot format",
                choices = c("pdf","png"),
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
        ),width = 2
      )
      ,
      mainPanel = mainPanel(
        column(
          12,wellPanel(
            plotOutput(ns("gene_pancan_dist"))
            )
        ),
        column(
          12,
          h4("NOTEs:"),
          h5("1. The data query may take some time based on your network. Wait until a plot shows"),
          h5("2. The unit of gene expression is log2(tpm+0.001)"),
          h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels")
        ),width = 10
      )
    )
  )
}

server.modules_pancan_dist <- function(input, output, session) {
  ns <- session$ns
  colors <- reactive({c(input$tumor_col,input$normal_col)})
  
  plot_theme <- reactive({themes[[input$theme]]})
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_pancan_dist"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_toil_TvsN(
        Gene = input$Pancan_search,
        Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
        Show.P.value = input$pdist_show_p_value,
        Show.P.label = input$pdist_show_p_label,
        TCGA.only = input$pdist_dataset,
        values = colors(),
      ) + plot_theme() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))
    }
    return(p)
  })
  
  observeEvent(input$Pancan_search, {
      # output$colorvalues = reactive({c(input$tumor_col,input$normal_col)
      #   })
      output$gene_pancan_dist <- renderPlot({
        w$show() # Waiter add-ins
        plot_func()
      })
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$Pancan_search," gene_pancan_dist.", input$device)
    },
    content = function(file) {
      p <- plot_func()
      ggplot2::ggsave(filename = file, plot = print(p), device = input$device, width = input$width, height = input$height, dpi = 600)
    }
  )
  
}
