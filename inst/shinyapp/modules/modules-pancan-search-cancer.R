choices <- c(
  "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", "LUAD", "LUSC"
)

ui.modules_cancer_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Cancer Expression Distribution"),
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
        materialSwitch(ns("pdist_mode"), "Show Box plot", inline = TRUE),
        materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = FALSE),
        materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = FALSE),
        materialSwitch(ns("pdist_dataset"), "TCGA Dataset only", inline = FALSE),
        colourpicker::colourInput(inputId = ns("tumor_col"), "Tumor sample color", "#DF2020"),
        colourpicker::colourInput(inputId = ns("normal_col"), "Normal sample color", "#DDDF21"),
        selectInput(inputId = ns("Cancer"), label = "Filter Cancer", choices = choices, selected = "ACC"),
        numericInput(inputId = ns("height"), label = "Height", value = 5),
        numericInput(inputId = ns("width"), label = "Width", value = 5),
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
        plotOutput(ns("gene_cancer_dist"), height = "600px"),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. The unit of gene expression is log2(tpm+0.001)"),
        p("3. You have to turn on both 'Show P value' and 'Show P label' to show significant labels"),
        p("4. If a void plot shows, please check your input"),
        width = 9
      )
    )
  )
}

server.modules_cancer_dist <- function(input, output, session) {
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
  
  colors <- reactive({
    c(input$tumor_col, input$normal_col)
  })

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_cancer_dist"), html = waiter::spin_hexdots(), color = "white")

  plot_func <- reactive({
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_toil_TvsN_cancer(
        Gene = input$Pancan_search,
        Cancer = input$Cancer,
        Mode = ifelse(input$pdist_mode, "Boxplot", "Violinplot"),
        Show.P.value = input$pdist_show_p_value,
        Show.P.label = input$pdist_show_p_label,
        TCGA.only = input$pdist_dataset,
        values = colors()
      )
      p = p + theme_cowplot()
    }
    return(p)
  })

  observeEvent(input$search_bttn, {
    output$gene_cancer_dist <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, " gene_cancer_dist.", input$device)
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
    }
  )
}
