choices <- c(
  "ACC","BLCA","BRCA","CESC","CHOL","COAD","DLBC","ESCA","GBM","HNSC","KICH","KIRC","KIRP","LAML","LGG","LIHC","LUAD","LUSC"
)

ui.modules_cancer_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module: Gene Cancer Expression Distribution"),
    fluidRow(
      tags$div(
        style = "margin-left: 30px;",
        fluidRow(
          shinyWidgets::searchInput(
            inputId = ns("pancan_search"),
            label = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            placeholder = "Enter a gene symbol, e.g. TP53",
            width = "40%"
          ),
          shinyBS::bsPopover(ns("pancan_search"),
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
            selectInput(inputId = ns("Cancer"), label = "Filter Cancer", choices = choices, selected = "ACC"),
            fluidRow(
              numericInput(inputId = ns("height"),label = "Height",value = 5),
              numericInput(inputId = ns("width"),label = "Width",value = 5),
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
        )
      )
      ,
      mainPanel = mainPanel(
        column(
          6,wellPanel(
            plotOutput(ns("gene_cancer_dist"),height = "600px")
          )
        ),
      )
    )
  )
}

server.modules_cancer_dist <- function(input, output, session) {
  ns <- session$ns
  
  colors <- reactive({c(input$tumor_col,input$normal_col)})
  
  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_cancer_dist"), html = waiter::spin_hexdots(), color = "white")
  
  plot_func <- reactive({
    if (nchar(input$pancan_search) >= 1) {
      p <- vis_toil_TvsN_cancer(
        Gene = input$pancan_search,
        Cancer = input$Cancer,
        Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
        Show.P.value = input$pdist_show_p_value,
        Show.P.label = input$pdist_show_p_label,
        TCGA.only = input$pdist_dataset,
        values = colors()
      ) 
    }
    return(p)
  })
  
  observeEvent(input$pancan_search, {
    output$gene_cancer_dist <- renderPlot({
      w$show() # Waiter add-ins
      plot_func()
    })
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$pancan_search," gene_cancer_dist.", input$device)
    },
    content = function(file) {
      p <- plot_func()
      if(input$device == "pdf"){
        pdf(file,width = input$width, height = input$height)
        print(p)
        dev.off()
      } else {
        png(file,width = input$width, height = input$height,res = 300,units = "in")
        print(p)
        dev.off()
      }
      
      #ggplot2::ggsave(filename = file, plot = print(p), device = input$device, width = input$width, height = input$height, dpi = 600)
    }
  )
}