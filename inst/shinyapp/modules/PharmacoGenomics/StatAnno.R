uiStatAnno <- function(id){
  ns <- NS(id)
  fluidPage(
    column(12,
           navlistPanel(
             tabPanel("Overall Drug Info",
                      tabsetPanel(
                        tabPanel("Drug and Cell Counts",
                                 plotOutput(ns("p_count_drugandcell"))
                        ),
                        tabPanel("Cell Subtypes",
                                 plotly::plotlyOutput(ns("p_count_subtype"))),
                        tabPanel("Drug and Cell Overlap Counts",
                                 plotOutput(ns("p_overlap_drug")),
                                 plotOutput(ns("p_overlap_cell"))
                        )
                      )
             ),
             tabPanel("Annotation",
                      tabsetPanel(
                        tabPanel("Cell",
                                 DT::dataTableOutput(ns("cell_anno"))),
                        tabPanel("Drug",
                                 DT::dataTableOutput(ns("drug_anno"))),
                      )),
           )),
  )
}

serverStatAnno <- function(input, output, session){
  ns <- session$ns
  # Plot ----
  output$p_count_drugandcell <- renderPlot({
    p_count_drugandcell
  })
  output$p_count_subtype <- plotly::renderPlotly({
    p_count_subtype
  })
  output$p_overlap_drug <- renderPlot({
    p_overlap_drug
  })
  output$p_overlap_cell <- renderPlot({
    p_overlap_cell
  })
  # Table ----
  output$drug_anno <- DT::renderDataTable({ 
    drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$cell_anno <- DT::renderDataTable({ 
    cell_anno
  }, options = list(scrollX = TRUE), selection = 'single')
}

# StatAnnoMerge <- function(){
#   uiStatAnno("StatAnno")
# }