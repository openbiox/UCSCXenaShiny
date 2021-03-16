ui.combo.sg.pancan.analysis <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Single gene pan-cancer transcriptome analysis"),
    ##input gene 
    fluidRow(
      shinyWidgets::searchInput(
        inputId = ns("Pancan_search"),
        label = NULL,
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        placeholder = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
        width = "40%"
    )
  ),
  ##pancan distribution
  fluidRow(
    column(
      12,
      plotOutput(ns("gene_pancan_dist")
      )
  ),
  ##uni-cox survival analysis
  fluidRow(
    column(
      6,
      plotOutput(ns("unicox_gene_tree")
      )
    ),
    column(
      6,
      ##heatmap: correlation between gene and immune signatures
      selectInput(inputId = ns("immune_sig"), "Select the immune signature source", selected = "Cibersort",
                  choices= c("Yasin","Wolf","Attractors","ICR","c7atoms","Bindea","Cibersort")),
      plotOutput(ns("hm_gene_immune_cor"))
      )
  )
 )
)
}

server.combo.sg.pancan.analysis <- function(input, output, session) {
  observeEvent(input$Pancan_search, {
    if (nchar(input$Pancan_search) >= 1) {
      output$gene_pancan_dist <- renderPlot({
        vis_toil_TvsN(
          Gene = input$Pancan_search,
          Mode = ("Boxplot"),
          Method = "wilcox.test",
          Show.P.value = TRUE,
          Show.P.label = TRUE
        )
      })
      
     output$unicox_gene_tree <- renderPlot({
       vis_unicox_tree(
         Gene = input$Pancan_search
       )
     })
     
     output$hm_gene_immune_cor <- renderPlot({
       vis_gene_immune_cor(
         Gene = input$Pancan_search,
         Immune_sig_type = input$immune_sig
       )
     })
     
    }
  })
}