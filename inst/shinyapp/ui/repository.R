ui.page_repository <- function() {
  tabPanel(
    title = "Repository",
    icon = icon("database"),

    # Set checkbox css
    tags$head(
      tags$style(
        HTML(
          ".checkbox-inline { 
          margin-left: 0px;
          margin-right: 10px;
}
.checkbox-inline+.checkbox-inline {
margin-left: 0px;
margin-right: 10px;
}
.shiny-notification {
position:fixed;
top: calc(50% + 120px);;
left: calc(50% + 250px);;
}
"
        )
      )
    ),

    fluidPage(
      br(),

      column(
        width = 3,
        tags$div(
          id = "hubs_info",
          # style = "padding: 0px 5px 1px;border: 1px solid #EEE;border-radius: 3px;margin-bottom: 20px;",
          style = "padding: 0px 5px 1px;margin-bottom: 0px;",
          checkboxGroupInput("hubs_text", h4("Active Data Hub :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
            # inline = TRUE,
            width = "80%",
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
            )
          ),
          # add popover on UI
          shinyBS::bsPopover("hubs_text",
            title = "Tips",
            content = "Data hub/host is an individual database for storing genomic data",
            placement = "right", options = list(container = "body")
          )
          # bsTooltip("hubs_text", "The wait times will be broken into this many equally spaced bins",
          #           "right", options = list(container = "body"))
        ),

        tags$div(
          id = "cohorts_info",
          style = "padding: 8px 1px 0px;margin-bottom: 25px;",
          tags$style(type = "text/css", "#cohorts_text {height: 35px;}"),
          textInput("cohorts_text", h4("Cohort Name :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
            width = "80%", placeholder = "e.g. Breast (separator is ;)", value = NULL
          ),
          shinyBS::bsPopover("cohorts_text",
            title = "Tips",
            content = "Cohort is a dataset set from independent study/site",
            placement = "right", options = list(container = "body")
          )
        ),

        tags$div(
          id = "type_info",
          style = "padding: 0px 5px 1px;margin-bottom: 0px;",
          checkboxGroupInput("type_text", h4("Data Type :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
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
          )
        ),

        tags$div(
          id = "subtype_info",
          style = "padding: 8px 1px 0px;margin-bottom: 25px;",
          tags$style(type = "text/css", "#subtype_text {height: 35px;}"),
          textInput("subtype_text", h4("Data Subtype :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
            width = "80%", placeholder = "e.g. gene expression (separator is ;)", value = NULL
          ),
          shinyBS::bsPopover("subtype_text",
            title = "Tips",
            content = paste0(
              "Available options: ",
              table(XenaData$DataSubtype) %>% names() %>% paste(collapse = ";")
            ),
            placement = "right", options = list(container = "body")
          )
        )
      ),

      column(
        width = 9,
        DT::dataTableOutput("xena_table"),

        hr(),

        actionButton(inputId = "show_met", label = "Show Metadata", icon = icon("database"), style = "margin-bottom: 10px; margin-left: 25px;"),
        actionButton(inputId = "req_data", label = "Request Data", icon = icon("file"), style = "margin-bottom: 10px; margin-left: 75px;"),

        br(),

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
