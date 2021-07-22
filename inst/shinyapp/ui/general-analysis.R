ui.page_general_analysis <- function() {
  # ns <- NS(id)
  # navbarMenu(
  #   title = "General Analysis",
  #   icon = icon("angle-double-down"),
  #   "result"
  #   # tabPanel("Single Gene Pan-cancer Analysis",
  #   #          ui.sg.pancan.analysis("sg.pancan.analysis"))
  # )
  tabPanel(
    title = "General Analysis",
    icon = icon("angle-double-down"),
    fluidRow(
      column(
        2,
        dropdownButton(
          inputId = "ga_drop_button",
          label = "Pre-selected Datasets for Analysis",
          icon = icon("sliders"),
          status = "primary",
          circle = FALSE,
          DT::dataTableOutput("ga_dataset_table")
        ),
        shinyBS::bsPopover("ga_drop_button",
          title = "Tips",
          content = "Click to show pre-selected datasets including user selected datasets from Repository page and corresponding phenotype datasets.",
          placement = "right", options = list(container = "body")
        )
      ),
      column(
        1, offset = 1,
        dropdownButton(
          inputId = "ga_drop_button_custom_data",
          label = "Upload your data?",
          tags$h3("Upload your data file"),
          br(),
          
          fileInput("ga_input_feature_file", 
                    label = "Feature-by-sample file (*.csv/.tsv/.txt), e.g., a gene expression matrix", 
                    accept = c("text/plain", ".gz")),
          tags$a(href="https://tcga-xena-hub.s3.us-east-1.amazonaws.com/download/TCGA.LAML.sampleMap%2FHiSeqV2.gz", "Click to download example feature data"),
          br(),
          
          fileInput("ga_input_phenotype_file",
                    label = "Phenotype file (*.csv/.tsv/.txt)",
                    accept = c("text/plain", ".gz")),
          tags$a(href="https://tcga-xena-hub.s3.us-east-1.amazonaws.com/download/TCGA.LAML.sampleMap%2FLAML_clinicalMatrix", "Click to download example phenotype data"),
          
          tags$h5("Note: Not both files are required. Prepare your upload files based on your analysis plan.\nBesides, make sure upload files have correct file extensions."),
          
          br(),
          icon = icon("upload"),
          circle = FALSE
        ),
        shinyBS::bsPopover("ga_drop_button_custom_data",
                           title = "Tips",
                           content = "Click to upload custom data for analysis",
                           placement = "right", options = list(container = "body")
        )
      ),
      column(4,
        offset = 4,
        shinyWidgets::actionBttn(
          inputId = "use_ga_page",
          label = "How to use",
          icon = icon("question-circle"),
          style = "bordered",
          color = "primary",
          size = "sm"
        )
      )
    ),
    tags$br(),
    # navlistPanel is an alternative
    tabsetPanel(
      tabPanel(
        "Scatter-Correlation",
        ui.modules_ga_scatter_correlation("module_ga_scatter_correlation")
      ),
      tabPanel(
        "Matrix-Correlation",
        ui.modules_ga_matrix_correlation("module_ga_matrix_correlation")
      ),
      tabPanel(
        "Group-Comparison",
        ui.modules_ga_group_comparison("module_ga_group_comparison")
      ),
      tabPanel(
        "Survival-Analysis",
        ui.modules_ga_surv_analysis("module_ga_surv_analysis")
      )
    )
  )
}
