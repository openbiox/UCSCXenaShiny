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
    ),
    tags$br(),
    tabsetPanel(
      tabPanel(
        "Correlation Analysis"
      ),
      tabPanel(
        "Comparison Analysis"
      )
    )
  )
}
