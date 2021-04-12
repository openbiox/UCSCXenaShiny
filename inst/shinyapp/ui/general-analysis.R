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
        fluidPage(
          fluidRow(
            column(
              3,
              h4("Analysis Controls"),
              uiOutput("ga_data1_id"),
              selectizeInput(
                inputId = "ga_data1_mid", # molecule identifier
                label = "Dataset 1 molecule identifier:",
                choices = NULL,
                options = list(
                  create = TRUE,
                  maxOptions = 5,
                  placeholder = "e.g. TP53",
                  plugins = list("restore_on_backspace")
                )
              ),
              uiOutput("ga_data2_id"),
              selectizeInput(
                inputId = "ga_data2_mid", # molecule identifier
                label = "Dataset 2 molecule identifier:",
                choices = NULL,
                options = list(
                  create = TRUE,
                  maxOptions = 5,
                  placeholder = "e.g. TP53",
                  plugins = list("restore_on_backspace")
                )
              ),
              materialSwitch(
                inputId = "ga_use_ggstats",
                label = "Use ggstatsplot?",
                value = FALSE,
                status = "primary"
              ),
              actionBttn(
                inputId = "ga_go",
                label = "Go!",
                color = "primary",
                style = "bordered",
                size = "sm"
              )
            ),
            column(
              6,
              plotOutput("ga_output"),
              tags$br(),
              h6("NOTE: The data table is not available when use ggstatsplot."),
              tags$br(),
              DT::dataTableOutput("ga_output_data")
            ),
            column(
              3,
              h4("Sample Filters"),
              uiOutput("ga_data_filter1_id"),
              actionBttn(
                inputId = "ga_filter_button",
                label = "Click to filter!",
                color = "primary",
                style = "bordered",
                size = "sm"
              )
            )
          )
        )
      ),
      tabPanel(
        "Matrix-Correlation"
      ),
      tabPanel(
        "Group-Comparison"
      )
    )
  )
}
