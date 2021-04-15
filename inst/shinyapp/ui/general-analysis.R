ui.page_general_analysis <- function() {
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
        5,
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
      column(3,
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
