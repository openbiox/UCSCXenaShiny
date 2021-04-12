ui.page_repository <- function() {
  tabPanel(
    title = "Repository",
    icon = icon("database"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        shinyWidgets::actionBttn(
          inputId = "use_repository",
          label = "How to use repository",
          icon = icon("question-circle"),
          style = "bordered",
          color = "primary",
          size = "sm"
        ),
        shinyWidgets::prettyCheckboxGroup("hubs_text", "Active Data Hub:",
          choiceNames = c(
            "UCSC Public", "TCGA",
            "GDC", "ICGC",
            "Pan-Cancer Atlas", "TOIL",
            "Treehouse", "PCAWG",
            "ATAC-seq", "Single Cell"
          ),
          choiceValues = c(
            "publicHub", "tcgaHub", "gdcHub", "icgcHub", "pancanAtlasHub",
            "toilHub", "treehouseHub", "pcawgHub", "atacseqHub", "singlecellHub"
          ),
          selected = "gdcHub",
          shape = "round",
          status = "success",
          animation = "tada"
        ),
        shinyBS::bsPopover("hubs_text",
          title = "Tips",
          content = "Data hub/host is an individual database for storing genomic data",
          placement = "right", options = list(container = "body")
        ),
        uiOutput("cohorts_text"),
        shinyBS::bsPopover("cohorts_text",
          title = "Tips",
          content = "Cohort is an independent study, it contains datasets with same patients",
          placement = "right", options = list(container = "body")
        ),
        shinyWidgets::prettyCheckboxGroup("type_text", "Data Type:",
          choiceNames = c(
            "Phenotype", "Feature by sample matrix",
            "Genomic segments", "Mutations"
          ),
          choiceValues = c("clinicalMatrix", "genomicMatrix", "genomicSegment", "mutationVector"),
          selected = c("clinicalMatrix", "genomicMatrix", "genomicSegment", "mutationVector"),
          status = "success",
          animation = "tada"
        ),
        shinyBS::bsPopover("type_text",
          title = "Tips",
          content = "Data type divide datasets into 4 basic categories: Phenotype for clinical or other phenotype data; Feature by sample matrix for gene/probe expression matrix; Genomic segments for copy number records; Mutations for mutation annotations",
          placement = "right", options = list(container = "body")
        ),
        uiOutput("subtype_text"),
        shinyBS::bsPopover("subtype_text",
          title = "Tips",
          content = "Subtype are categories based on biological meaning instead of storing format like Type",
          placement = "right", options = list(container = "body")
        )
      ),
      mainPanel(
        DT::dataTableOutput("xena_table"),
        useShinyalert(), # Set up shinyalert
        # use_waiter(),
        hr(),
        fluidRow(
          column(
            offset = 1,
            4,
            actionBttn(
              inputId = "show_met",
              label = "Show Metadata",
              style = "gradient",
              # color = "danger",
              icon = icon("database"),
              size = "sm"
            )
          ),
          column(
            4,
            actionBttn(
              inputId = "req_data",
              label = "Request Data",
              style = "gradient",
              icon = icon("file"),
              size = "sm"
            )
          ),
          column(
            3,
            actionBttn(
              inputId = "analyze_data",
              label = "Analyze Data",
              style = "gradient",
              icon = icon("chart-bar"),
              size = "sm"
            )
          )
        ),
        hr(),
        shinyjs::hidden(
          tags$div(
            id = "show_data",
            h4("Selected Data:", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
            h6("Wait until a table shows..."),
            tableOutput(
              "table"
            )
          )
        )
      )
    )
  )
}
