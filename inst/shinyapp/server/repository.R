# Render cohorts_text and subtype_text from server

update_repo_data <- function(input) {
  res <- XenaData

  if (!is.null(input$hubs_text)) {
    res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
  }

  if (!is.null(input$cohorts_text) && input$cohorts_text != "") {
    if (!"ALL" %in% input$cohorts_text) {
      res <- dplyr::filter(res, XenaCohorts %in% input$cohorts_text)
    }
  }

  if (!is.null(input$subtype_text) && input$subtype_text != "") {
    if (!"ALL" %in% input$subtype_text) {
      res <- dplyr::filter(res, DataSubtype %in% input$subtype_text)
    }
  }

  if (!is.null(input$type_text)) {
    res <- dplyr::filter(res, Type %in% input$type_text)
  }
  return(res)
}

repo_filters <- reactive({
  list(
    hub = input$hubs_text,
    cohort = input$cohorts_text,
    type = input$type_text,
    subtype = input$subtype_text
  )
})
dataset <- eventReactive(repo_filters(), update_repo_data(input))
# df <- dataset

output$cohorts_text <- renderUI({
  cohorts_text <- isolate(input$cohorts_text)
  selectInput(
    inputId = "cohorts_text",
    label = "Cohort Name:",
    choices = c("ALL", unique(xena_table$Cohort[xena_table$Hub %in% input$hubs_text])),
    selected = cohorts_text,
    multiple = TRUE
  )
})

output$subtype_text <- renderUI({
  subtype_text <- isolate(input$subtype_text)
  selectInput(
    inputId = "subtype_text",
    label = "Data Subtype:",
    choices = c("ALL", unique(xena_table$DataSubtype[xena_table$Hub %in% input$hubs_text])),
    selected = subtype_text,
    multiple = TRUE
  )
})

output$xena_table <- DT::renderDataTable({
  dataset()[, c(
    "XenaDatasets",
    "XenaHostNames",
    "XenaCohorts",
    "SampleCount",
    "DataSubtype",
    "Label",
    "Unit"
  )] %>%
    DT::datatable(
      rownames = FALSE,
      colnames = c(
        "Dataset",
        "Hub",
        "Cohort",
        "Samples",
        "Subtype",
        "Label",
        "Unit"
      ),
      options = list(
        language = list(search = "Filter with keyword:"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#4a5a6a', 'color': '#fff'});",
          "$(this.api().table().header()).css({'font-size': '95%'});",
          "$(this.api().table().body()).css({'font-size': '90%'});",
          "}"
        )
      )
    )
})


# Keep selected database
selected_database <- reactive({
  s <- input$xena_table_rows_selected
  if (length(s)) {
    return(dataset()[s, ])
  }
})

selected_database_add_url <- reactive({
  data <- selected_database()
  if (!is.null(data)) {
    data$download <- unlist(lapply(query_url()$url, function(x) {
      as.character(tags$a(href = x, "download link"))
    }))
    data$browse <- unlist(lapply(query_url()$browse, function(x) {
      as.character(tags$a(href = x, "browse Xena dataset page"))
    }))
  }
  data
})


query_url <- reactive({
  s <- input$xena_table_rows_selected
  if (!is.null(s)) {
    xe_query <- xe_query_url(selected_database())
    return(xe_query)
  }
})

# Show download url for selected database
w <- waiter::Waiter$new(id = "table", html = waiter::spin_wobblebar(), color = "white")

observe({
  s <- input$xena_table_rows_selected

  if (length(s) > 0) {
    data <- selected_database_add_url()

    w$show() # Waiter add-ins
    Sys.sleep(0.2)

    if (length(data$download) > 0) {
      output$table <- renderTable(
        {
          tryCatch(
            data.frame(
              No = seq_along(s),
              CohortName = data$XenaCohorts,
              Label = data$Label,
              Download = data$download,
              Browse = data$browse
            ),
            error = function(e) {
              message("Detect error from data.frame construction, no pain.")
            }
          )
        },
        sanitize.text.function = function(x) x
      )
    }
    shinyjs::show("show_data")
  } else {
    shinyjs::hide("show_data")
  }
})


# Button to show metadata -------------------------------------------------

# Dialog for showing selected data information
observeEvent(input$show_met, {
  s <- input$xena_table_rows_selected
  if (length(s)) {
    showModal(modalDialog(
      title = "Detail information...",
      size = "l",
      DT::DTOutput("detail_info")
    ))
  } else {
    sendSweetAlert(
      session,
      "Alert",
      "Please select at least one dataset by clicking the rows in the data table firstly.",
      type = "info"
    )
  }
})

# Show metadata
observeEvent(input$show_met, {
  s <- input$xena_table_rows_selected
  if (length(s)) {
    m <-
      purrr::map2(dataset()$XenaHosts[s], dataset()$XenaDatasets[s], function(x, y) {
        temp <- .p_dataset_metadata(x, y)
        json_data <- jsonlite::parse_json(temp$text)
        message("Metadata for ", y, " queried.")
        json_data <-
          purrr::map(json_data, ~ ifelse(length(.) > 0, paste(., collapse = ","), .))
        json_data <- tibble::enframe(json_data)
        json_data$value <- unlist(json_data$value)
        json_data
      })
    m <- purrr::reduce(m, dplyr::full_join, by = "name")
    message("Metadata are loaded!")
    colnames(m) <- c("Metadata", paste0("dataset", 1:(ncol(m) - 1L)))
  }
  output$detail_info <-
    DT::renderDT(m, options = list(dom = "t", scrollX = TRUE))
})


# Button to request (download) data ---------------------------------------

observeEvent(input$req_data, {
  s <- input$xena_table_rows_selected
  if (length(s)) {
    showModal(
      modalDialog(
        title = "Submitted datasets:",
        size = "l",
        DT::DTOutput("table_query"),
        hr(),
        if (xena.runMode == "client") {
          shinyDirButton(
            id = "download",
            label = "Download data directly",
            title = "Please select a folder",
            icon = icon("download"),
            style = "margin-bottom: 10px;"
          )
        } else {
          downloadButton(
            outputId = "download",
            label = "Download data directly",
            icon = icon("download"),
            style = "margin-bottom: 10px;"
          )
        },
        downloadButton(
          outputId = "total_url",
          label = "Batch download in terminal",
          icon = icon("download"),
          style = "margin-bottom: 10px; margin-left: 40px;"
        ),
        shinyBS::bsPopover(
          "total_url",
          title = "Tips",
          content = "Download wget commands to download requested datasets.",
          placement = "bottom",
          options = list(container = "body")
        ),
        actionButton(
          "show_R_code",
          label = "Copy R download code",
          icon = icon("file"),
          style = "margin-bottom: 10px; margin-left: 65px;"
        ),
        hr(),
        verbatimTextOutput("R_download_code")
      )
    )

    output$table_query <-
      DT::renderDT(query_url(), options = list(dom = "t", scrollX = TRUE))
  } else {
    sendSweetAlert(
      session,
      "Alert",
      "Please select at least one dataset by clicking the rows in the data table firstly.",
      type = "info"
    )
  }
})

if (xena.runMode == "client") {
  observeEvent(input$download, {
    volumes <- c(home = fs::path_home(), root = "/")
    shinyDirChoose(input, "download", roots = volumes, session = session)

    if (is.integer(input$download)) {
      message("No directory has been selected.")
    } else {
      message("Download datasets from client mode.")
      withProgress(
        message = "Download data directly from remote server with R",
        detail = "This may take a while...",
        value = 0,
        {
          for (i in seq_len(nrow(query_url()))) {
            UCSCXenaTools::XenaDownload(
              query_url()[i, c("hosts", "datasets", "url")],
              destdir = parseDirPath(volumes, input$download),
              download_probeMap = TRUE,
              trans_slash = TRUE
            )
            Sys.sleep(0.05)
            incProgress(1 / nrow(query_url()))
          }
        }
      )
    }
  })
} else {
  # Download buttom of request data with zip compress
  message("Download datasets from server mode.")

  output$download <- downloadHandler(
    filename = paste(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), "xena-datasets.zip", sep = "-"),
    contentType = "application/zip",
    content = function(file) {
      withProgress(
        message = "Query data from remote server",
        detail = "This may take a while...",
        value = 0,
        {
          xe_download <- dplyr::tibble()
          for (i in seq_len(nrow(query_url()))) {
            xe_download <- dplyr::bind_rows(
              xe_download,
              UCSCXenaTools::XenaDownload(
                query_url()[i, c("hosts", "datasets", "url")],
                destdir = XENA_DEST,
                download_probeMap = TRUE,
                trans_slash = TRUE
              )
            )
            incProgress(1 / (2 * nrow(query_url())))
            Sys.sleep(0.05)
          }
          zip::zipr(
            zipfile = file.path(XENA_DEST, "xena-datasets.zip"),
            files = xe_download$destfiles, recurse = FALSE
          )
          incProgress(1 / 4)
          file.copy(file.path(XENA_DEST, "xena-datasets.zip"), file)
          file.remove(file.path(XENA_DEST, "xena-datasets.zip"))
          incProgress(1 / 4)
        }
      )
    }
  )
}

# Download list of urls
output$total_url <- downloadHandler(
  filename = paste0(Sys.Date(), "-commands.sh"),
  contentType = "text/txt",
  content = function(file) {
    write.table(
      file = file,
      c(
        "#!/usr/bin/env bash",
        paste(
          "#Usage: run bash",
          paste0(Sys.Date(), "-commands.sh"),
          "in your terminal under a desired directory"
        ),
        paste0("wget -c ", query_url()$url)
      ),
      row.names = F,
      col.names = F,
      quote = F
    )
  }
)

## Show download code for reproducible research
observeEvent(input$show_R_code, {
  f <- tempfile()
  dput(query_url(), f)
  R_code <- paste(
    "# Load R package",
    "library('UCSCXenaTools')",
    "",
    "# Generate dataset(s) information",
    paste0("dataset_query <- ", paste(readLines(f), collapse = "")),
    "",
    "# Download dataset(s)",
    "dl <- XenaDownload(dataset_query,
                        destdir = './', # At default, download to working directory
                        download_probeMap = TRUE,
                        trans_slash = TRUE)",
    "",
    "# Load dataset(s) into R",
    "datasets <- XenaPrepare(dl)",
    "# Check data",
    "datasets",
    sep = "\n"
  )
  file.remove(f)

  output$R_download_code <- shiny::renderText(R_code)
})


# Button to Analysis ------------------------------------------------------

# ref: https://stackoverflow.com/questions/38706965/is-there-any-way-for-an-actionbutton-to-navigate-to-another-tab-within-a-r-shi
observeEvent(input$analyze_data, {
  updateNavbarPage(session = session, inputId = "navbar", selected = "General Analysis")
})

# Show alert info when select rows from table -----------------------------

observeEvent(input$use_repository, {
  # Show a modal when the button is pressed
  shinyalert(
    title = "Repository Usage",
    text = paste(
      "Firstly, filter dataset table based on left filters or right search bar.",
      "Secondly, select datasets by clicking corresponding rows in dataset table.",
      "Lastly, click the button under the dataset table to check metadata or download datasets.",
      sep = "\n\n"
    ),
    type = "info",
    timer = 0,
    confirmButtonCol = "#202324"
  )
})
