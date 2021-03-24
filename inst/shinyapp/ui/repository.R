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
        hr(),
        shinyWidgets::prettyRadioButtons("hubs_text", "Active Data Hub:", 
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
          animation = "jelly"
        ),
        shinyBS::bsPopover("hubs_text",
          title = "Tips",
          content = "Data hub/host is an individual database for storing genomic data",
          placement = "right", options = list(container = "body")
        ),
        shinyWidgets::textInputAddon("cohorts_text", "Cohort Name:",
          placeholder = "e.g. Breast (separator is ;)",
          value = NULL, width = "100%", addon = icon("search")
        ),
        shinyBS::bsPopover("cohorts_text",
          title = "Tips",
          content = "Cohort is a dataset set from independent study/site",
          placement = "right", options = list(container = "body")
        ),
        checkboxGroupInput("type_text", "Data Type:", 
          choiceNames = c(
            "Phenotype", "Feature by sample matrix",
            "Genomic segments", "Mutations"
          ),
          choiceValues = c("clinicalMatrix", "genomicMatrix", "genomicSegment", "mutationVector")
        ),
        shinyBS::bsPopover("type_text",
          title = "Tips",
          content = "Data type divide datasets into 4 basic categories: Phenotype for clinical or other phenotype data; Feature by sample matrix for gene/probe expression matrix; Genomic segments for copy number records; Mutations for mutation annotations",
          placement = "right", options = list(container = "body")
        ),
        shinyWidgets::textInputAddon("subtype_text", "Data Subtype:", 
          value = NULL, width = "100%", addon = icon("search")
        ),
        shinyBS::bsPopover("subtype_text",
          title = "Tips",
          content = paste0(
            "Available options: ",
            unique(XenaData$DataSubtype) %>% paste(collapse = ";")
          ),
          placement = "right", options = list(container = "body")
        )
      ),
      mainPanel(
        DT::dataTableOutput("xena_table"),
        useShinyalert(), # Set up shinyalert
        # use_waiter(),
        hr(),
        fluidRow(
          column(4,
            offset = 1,
            actionButton(
              inputId = "show_met",
              label = "Show Metadata",
              icon = icon("database")
              #style = "gradient",
              #color = "default",
              #size = "sm", block = T
            )
          ),
          column(4,
            offset = 2,
            actionButton(
              inputId = "req_data",
              label = "Request Data",
              icon = icon("file")
            )
          )
        ),
        hr(),
        shinyjs::hidden(
          tags$div(
            id = "show_data",
            h4("Selected Data:", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
            h6("Wait until a table shows..."),
            h6("Of note, single dataset file can be downloaded by clicking URL"),
            tableOutput(
              "table"
            )
          )
        )
      )
    )
  )
}
