# Load necessary packages ----------------------------------
library(shiny)
library(UCSCXenaTools)
library(shinyBS)

# Put data here -----------------------------------------------------------
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


# Put modules here --------------------------------------------------------
modules_path <- system.file("inst", "shinyapp", "modules", package = "UCSCXenaShiny", mustWork = TRUE)
modules_file <- dir(modules_path, pattern = "\\.R$", full.names = TRUE)
sapply(modules_file, function(x, y) source(x, local = y), y = environment())


# Put pages here ----------------------------------------------------------
pages_path <- system.file("inst", "shinyapp", "pages", package = "UCSCXenaShiny", mustWork = TRUE)
pages_file <- dir(pages_path, pattern = "\\.R$", full.names = TRUE)
sapply(pages_file, function(x, y) source(x, local = y), y = environment())



# UI part ----------------------------------------------------------------------
ui <- tagList(
  tags$head(tags$title("XenaShiny")),
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(
      img(src = "xena_shiny-logo_white.png", height = 49.6, style = "margin:-20px -15px -15px -15px")
    ),
    ui.page_home(),
    ui.page_repository(),
    ui.page_modules(),
    ui.page_pipelines(),
    ui.page_help(),
    ui.page_developers(),
    footer = ui.footer(),
    theme = shinythemes::shinytheme("cosmo")
  )
)

# Server Part ---------------------------------------------------------------
server <- function(input, output, session) {
  cat("Shiny app run successfully! Enjoy it!\n")
  cat("               --  Xena shiny team\n")
  # Home ===========================
  res <- callModule(server.home_search_box, "homepage_pancan_search")

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
          ),
          downloadButton(outputId = "total_url", label = "URLs List", icon = icon("download"), style = "margin-bottom: 10px; margin-left: 75px;"),
          shinyBS::bsPopover("total_url",
            title = "Tips",
            content = "Download list of target urls",
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

  # Download list of urls
  output$total_url <- downloadHandler(
    filename = "urls.txt",
    contentType = "text/txt",
    content = function(file) {
      write.table(file = file, paste0("wget ", query_url()$url), row.names = F, col.names = F, quote = F)
    }
  )


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


  # Modules -----------------------------------------------------------------
  # toil_df = ope_toil_gene()
  # output$vis_toil_gene = renderPlot({
  #   vis_toil_gene(toil_df)
  # })

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
