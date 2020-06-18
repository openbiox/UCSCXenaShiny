
themes <- list("Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "Classic" = theme_classic(),
               "Gray" = theme_gray())

ui.modules_pancan_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: gene pancan expression distribution"),
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
            selectInput(inputId = ns("theme"), label = h4("Select theme for plot"), choices = names(themes), selected = "Light")
          )
        ),width = 2
      )
      ,
      mainPanel = mainPanel(
        column(
          12,
          plotOutput(ns("gene_pancan_dist"))
        ),
        column(
          12,
          h4("NOTEs:"),
          h5("1. The data query may take some time based on your network. Wait until a plot shows"),
          h5("2. The unit of gene expression is log2(tpm+0.001)"),
          h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels")
        ),width = 10
      )
<<<<<<< HEAD
=======
    ),
    fluidRow(
      materialSwitch(ns("pdist_mode"), "Show violin plot", inline = TRUE),
      materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
      materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE),
      materialSwitch(ns("pdist_dataset"), "TCGA Dataset only", inline = FALSE),
      colourpicker::colourInput(inputId = ns("tumor_col"), "Tumor sample color", "#DF2020"),
      colourpicker::colourInput(inputId = ns("normal_col"), "Normal sample color",  "#DDDF21"),
      selectInput(inputId = ns("theme"), label = h4("Select theme for plot"), choices = names(themes), selected = "Light")
    ),
    column(
      12,
      plotOutput(ns("gene_pancan_dist"))
    ),
    column(
      12,
      h4("NOTEs:"),
      h5("1. The data query may take some time based on your network. Wait until a plot shows"),
      h5("2. The unit of gene expression is log2(tpm+0.001)"),
      h5("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels")
>>>>>>> 5b36faccfb41fd66f54da84ac83564a98066119e
    )
  )
}

server.modules_pancan_dist <- function(input, output, session) {
  
  colors <- reactive({c(input$tumor_col,input$normal_col)})
  plot_theme <- reactive({themes[[input$theme]]})
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      # output$colorvalues = reactive({c(input$tumor_col,input$normal_col)
      #   })
      output$gene_pancan_dist <- renderPlot({
        vis_toil_TvsN(
          Gene = input$Pancan_search,
          Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
          Show.P.value = input$pdist_show_p_value,
          Show.P.label = input$pdist_show_p_label,
          TCGA.only = input$pdist_dataset,
          values = colors(),
        ) + plot_theme()
      })
    }
  })
}
