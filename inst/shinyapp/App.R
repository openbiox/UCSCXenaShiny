# Prepare -----------------------------------------------------------------
library(shiny)
library(UCSCXenaTools)
library(shinyBS)

# Here data goes
data("XenaData", package = "UCSCXenaTools", envir = environment())
xena_table <- XenaData[, c(
  "XenaDatasets", "XenaHostNames", "XenaCohorts",
  "SampleCount", "DataSubtype", "Label"
)]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")

data("dat_datasets", package = "UCSCXenaShiny", envir = environment())
data("dat_samples", package = "UCSCXenaShiny", envir = environment())
data("XenaInfo", package = "UCSCXenaShiny", envir = environment())

Data_hubs_number <- XenaInfo[["n_hubs"]]
Cohorts_number <- XenaInfo[["n_cohorts"]]
Datasets_number <- XenaInfo[["n_datasets"]]
Samples_number <- XenaInfo[["n_samples"]]
Primary_sites_number <- XenaInfo[["n_origin"]]
Data_subtypes_number <- XenaInfo[["n_subtypes"]]

# global color
mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))
# need at least 140 colors for summary plot
mycolor <- rep(mycolor, 15)


# UI part ----------------------------------------------------------------------
ui <- tagList(
  tags$head(tags$title("XenaShiny")),
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(
      img(src = "xena_shiny-logo_white.png", height = 49.6, style = "margin:-20px -15px -15px -15px")
    ),
    # Home page ==================================================
    tabPanel(
      title = "Home",
      icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
        ),
        fluidRow(
          column(
            6,
            tags$div(
              column(
                12,
                tags$h2("Data Portal Summary"),
                tags$b(paste0("XenaShiny version ", packageVersion("UCSCXenaShiny"))),
                tags$br(),
                tags$a(href = "https://xenabrowser.net/datapages/", "Data are controled by UCSC Xena"),
                tags$hr(),
                tags$div(
                  class = "card-deck text-center block-center",
                  tags$div(
                    class = "card",
                    tags$div(
                      class = "card-body",
                      tags$img(src = "host.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      tags$h5("DATA HUBS")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Data_hubs_number)
                    )
                  ),
                  tags$div(
                    class = "card",
                    tags$div(
                      tags$img(src = "cohort.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      class = "card-body",
                      tags$h5("COHORTS")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Cohorts_number)
                    )
                  ),
                  tags$div(
                    class = "card",
                    tags$div(
                      class = "card-body",
                      tags$img(src = "dataset.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      tags$h5("DATASETS")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Datasets_number)
                    )
                  )
                ),
                tags$br(),
                tags$div(
                  class = "card-deck text-center block-center",
                  tags$div(
                    class = "card",
                    tags$div(
                      class = "card-body",
                      tags$img(src = "sample.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      tags$h5("SAMPLES")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Samples_number)
                    )
                  ),
                  tags$div(
                    class = "card",
                    tags$div(
                      class = "card-body",
                      tags$img(src = "site.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      tags$h5("PRIMARY SITES")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Primary_sites_number)
                    )
                  ),
                  tags$div(
                    class = "card",
                    tags$div(
                      class = "card-body",
                      tags$img(src = "file.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                      tags$h5("DATA SUBTYPES")
                    ),
                    tags$div(
                      class = "card-footer",
                      tags$h4(Data_subtypes_number)
                    )
                  )
                )
              )
            )
          ),
          column(
            6,
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tabsetPanel(
              tabPanel(
                "Sample Distribution",
                tags$br(),
                plotly::plotlyOutput("Xenasummary1", height = "100%")
              ),
              tabPanel(
                "Dataset Distribution",
                tags$br(),
                plotly::plotlyOutput("Xenasummary", height = "100%")
              )
            )
          )
        ),
        tags$br(),
        tags$div(
          class = "text-center",
          tags$div(
            class = "bg-dark text-white",
            tags$p("The goal of XenaShiny is to provide a web app for downloading, analyzing and visulizing datasets from UCSC Xena, which is a collection of UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and others. Databases are normalized so they can be combined, linked, filtered, explored and downloaded.")
          )
        )
      )
    ),

    # Repository page ====================
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
    ),

    # Modules page ======================
    navbarMenu(
      title = "Modules",
      icon = icon("buromobelexperte"),
      tabPanel("Import"),
      tabPanel("Export"),
      tabPanel("module 3")
    ),

    # Pipelines page =====================
    navbarMenu(
      title = "Pipelines",
      icon = icon("angle-double-down"),
      tabPanel("RNAseq")
    ),

    navbarMenu(
      title = "Help",
      icon = icon("question-circle"),
      tabPanel("News", 
               fluidPage(
                 includeMarkdown(system.file("NEWS.md", package = "UCSCXenaShiny", mustWork = TRUE))
               )),
      tabPanel("Usages"),
      tabPanel("Term List")
    ),

    tabPanel(
      title = "Developers",
      icon = icon("user-friends"),
      fluidPage(
        # titlePanel("Developers"),
        fluidRow(
          column(
            12,
            tags$div(
              class = "card-deck text-center block-center",
              tags$div(
                class = "card bg-info",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars3.githubusercontent.com/u/25057508?s=400&v=4.png",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Shixiang Wang"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/ShixiangWang", class = "card-link", "See Profile")
                )
              ),
              tags$div(
                class = "card bg-warning",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars3.githubusercontent.com/u/17489298?s=400&v=4",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Fei Zhao"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/fei0810", class = "card-link", "See Profile")
                )
              ),
              tags$div(
                class = "card bg-info",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars0.githubusercontent.com/u/28949856?s=400&v=4",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Yi Xiong"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/Byronxy", class = "card-link", "See Profile")
                )
              )
            ),
            tags$br(),
            tags$div(
              class = "card-deck text-center block-center",
              tags$div(
                class = "card bg-warning",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars2.githubusercontent.com/u/37660840?s=400&v=4",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Longfei Zhao"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/longfei8533", class = "card-link", "See Profile")
                )
              ),
              tags$div(
                class = "card bg-info",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars0.githubusercontent.com/u/38618580?s=400&v=4",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Yin Li"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/yinlisssss", class = "card-link", "See Profile")
                )
              ),
              tags$div(
                class = "card bg-warning",
                tags$div(
                  class = "card-body",
                  tags$img(
                    src = "https://avatars2.githubusercontent.com/u/22772592?s=400&v=4",
                    class = "img-circle",
                    alt = "logo", style = "width: 30% ; margin: 0 auto "
                  ),
                  tags$h4(class = "card-title", "Kai Gu"),
                  tags$p(class = "card-text", "Core developer")
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(href = "https://github.com/kaigu1990", class = "card-link", "See Profile")
                )
              )
            )
          )
        )
      )
    ),
    footer = tags$footer(HTML("Copyright &copy; 2019&nbsp;&nbsp;&nbsp;&nbsp;"), tags$a(href = "https://github.com/openbiox", "Openbiox"),
      HTML("- A community-driven bioinformatics innovation collaboration group in China |"),
      tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "MIT License"),
      align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
    ),
    theme = shinythemes::shinytheme("cosmo")
  )
)

# Server Part ---------------------------------------------------------------
server <- function(input, output, session) {

  cat("Shiny app run successfully! Enjoy it!\n")
  cat("               --  Xena shiny team\n")
  # Home ===========================
  output$Xenasummary <- plotly::renderPlotly({
    p <- dat_datasets %>%
      #  filter(XenaHostNames == "gdcHub") %>%
      dplyr::rename(
        Hub = XenaHostNames, Percent = Sample_percent,
        Cohort = XenaCohorts, DatasetCount = N
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = DatasetCount)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
      ggplot2::coord_flip() +
      ggplot2::labs(y = "", x = "") +
      ggplot2::theme_bw(base_size = 15) + # 去除背景色
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
      ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      ) + # 去除x轴
      ggplot2::guides(fill = F) +
      ggplot2::guides(color = F) +
      ggplot2::scale_fill_manual(values = mycolor)

    plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
  })
  output$Xenasummary1 <- plotly::renderPlotly({
    p <- dat_samples %>%
      #  filter(XenaHostNames == "gdcHub") %>%
      dplyr::rename(
        Hub = XenaHostNames, Percent = SampleCount_percent,
        Cohort = XenaCohorts, SampleCount = SampleCount_sum
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = SampleCount)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
      ggplot2::coord_flip() +
      ggplot2::labs(y = "", x = "") +
      ggplot2::theme_bw(base_size = 15) + # 去除背景色
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
      ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      ) + # 去除x轴
      ggplot2::guides(fill = F) +
      ggplot2::guides(color = F) +
      ggplot2::scale_fill_manual(values = mycolor)
    plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
  })


  # Repository ======================

  # separator is ;
  dataset <- reactive({
    res <- XenaData

    if (!is.null(input$hubs_text)) {
      res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
    }
    if (!is.null(input$cohorts_text) & input$cohorts_text != "") {
      ids <- sub(";", "|", input$cohorts_text)
      res <- dplyr::filter(res, grepl(ids, XenaCohorts, ignore.case = TRUE))
    }
    if (!is.null(input$subtype_text) & input$subtype_text != "") {
      ids <- sub(";", "|", input$subtype_text)
      res <- dplyr::filter(res, grepl(ids, DataSubtype, ignore.case = TRUE))
    }
    if (!is.null(input$type_text)) {
      res <- dplyr::filter(res, Type %in% input$type_text)
    }

    return(res)
  })

  # Show database information in repository page
  output$xena_table <- DT::renderDataTable(dataset()[, c(
    "XenaDatasets",
    "XenaHostNames",
    "XenaCohorts",
    "SampleCount",
    "DataSubtype",
    "Label"
  )],
  colnames = c("Dataset", "Hub", "Cohort", "Samples", "Subtype", "Label")
  )

  # Keep selected database
  selected_database <- reactive({
    s <- input$xena_table_rows_selected
    if (length(s)) {
      return(dataset()[s, ])
    }
  })

  query_url <- reactive({
    s <- input$xena_table_rows_selected
    if (!is.null(s)) {
      data <- selected_database()
      xe <- UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)
      xe_query <- UCSCXenaTools::XenaQuery(xe)
      return(xe_query)
    }
  })

  # Show download url for selected database
  observe({
    s <- input$xena_table_rows_selected
    if (!is.null(s)) {
      data <- selected_database()

      output$table <- renderTable({
        data.frame(
          ID = 1:length(s),
          XenaCohorts = data$XenaCohorts,
          Label = data$Label,
          URL = unlist(lapply(query_url()$url, function(x) {
            as.character(tags$a(href = x, x))
          }))
        )
      }, sanitize.text.function = function(x) x)

      shinyjs::show("show_data")
    } else {
      shinyjs::hide("show_data")
    }
  })

  # Dialog for showing selected data information
  observeEvent(input$show_met, {
    s <- input$xena_table_rows_selected
    if (length(s)) {
      showModal(
        modalDialog(
          title = "Detail information...",
          size = "l",
          DT::DTOutput("detail_info")
        )
      )
    }
  })

  # Dialog for some operations of request data
  observeEvent(input$req_data, {
    s <- input$xena_table_rows_selected
    if (length(s)) {
      showModal(
        modalDialog(
          title = "Submitted database...",
          size = "l",
          DT::DTOutput(
            "table_query"
          ),

          hr(),

          actionButton(inputId = "load", label = "Load Data", icon = icon("upload"), style = "margin-bottom: 10px; margin-right: 75px;"),
          shinyBS::bsPopover("load",
            title = "Tips",
            content = "Directly load data into R for analyses provided by modules or pipelines",
            placement = "bottom", options = list(container = "body")
          ),
          downloadButton(outputId = "download", label = "Download Data", icon = icon("download"), style = "margin-bottom: 10px;"),
          shinyBS::bsPopover("download",
            title = "Tips",
            content = "Download zipped data to local",
            placement = "bottom", options = list(container = "body")
          )
        )
      )

      output$table_query <- DT::renderDT(query_url(), options = list(dom = "t", scrollX = TRUE))
    }
  })

  # Show metadata
  observeEvent(input$show_met, {
    s <- input$xena_table_rows_selected
    if (length(s)) {
      m <- matrix(NA, nrow = length(s), ncol = 11)

      j <- 0
      for (i in s) {
        j <- j + 1
        temp <- .p_dataset_metadata(as.character(dataset()[i, "XenaHosts"]), as.character(dataset()[i, "XenaDatasets"]))

        title <- temp$longtitle
        dataset_ID <- temp$name
        samples <- temp$count

        if (is.na(temp$pmtext)) {
          version <- NA
        } else {
          version <- jsonlite::parse_json(temp$pmtext)$version
        }

        cohort <- jsonlite::parse_json(temp$text)$cohort
        type_of_data <- jsonlite::parse_json(temp$text)$dataSubType

        pm <- jsonlite::parse_json(temp$text)$probeMap
        ID_Mapping <- ifelse(is.null(pm), NA, file.path(as.character(dataset()[i, "XenaHosts"]), "download", pm))

        publication <- jsonlite::parse_json(temp$text)$articletitle
        if (is.null(publication)) publication <- NA

        citation <- jsonlite::parse_json(temp$text)$citation
        if (is.null(citation)) citation <- NA

        authorChin <- jsonlite::parse_json(temp$text)$dataproducer
        if (is.null(authorChin)) authorChin <- NA

        raw_data <- jsonlite::parse_json(temp$text)$url
        if (is.null(raw_data)) raw_data <- NA

        m[j, ] <- c(cohort, title, dataset_ID, samples, version, type_of_data, ID_Mapping, publication, citation, authorChin, raw_data)
      }

      m <- as.data.frame(m)
      names(m) <- c(
        "Cohort", "Title", "Dataset ID", "Samples", "Version", "Type of data",
        "ID/Gene Mapping", "Publication", "Citation", "Data producer", "Raw data"
      )
    }
    output$detail_info <- DT::renderDT(m, options = list(dom = "t", scrollX = TRUE))
  })


  # Download request data by XenaDownload function
  observeEvent(input$load, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Begin to download files, Please wait...", value = 0)

    UCSCXenaTools::XenaDownload(query_url())

    progress$set(message = "Over...", value = 1)
  })

  # Prepare request data by XenaPrepare function
  request_data <- eventReactive(input$load, {
    xe_download <- UCSCXenaTools::XenaDownload(query_url())
    return(UCSCXenaTools::XenaPrepare(xe_download))
  })

  # observeEvent(input$load, {
  #   print(request_data())
  # })

  # Download buttom of request data with zip compress
  output$download <- downloadHandler(
    filename = "database.zip",
    contentType = "application/zip",
    content = function(file) {
      xe_download <- UCSCXenaTools::XenaDownload(query_url())
      zip::zipr(zipfile = paste0(tempdir(), "/target_database.zip"), files = xe_download$destfiles, recurse = F)
      file.copy(paste0(tempdir(), "/target_database.zip"), file)
      file.remove(paste0(tempdir(), "/target_database.zip"))
    }
  )

  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste("www", r)
  })
}

# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)