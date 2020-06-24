# separator is ;
dataset <- reactive({
  res <- XenaData
  
  if (!is.null(input$hubs_text)) {
    res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
  }
  if (!is.null(input$cohorts_text) & input$cohorts_text != "") {
    ids <- sub(";", "|", input$cohorts_text)
    res <-
      dplyr::filter(res, grepl(ids, XenaCohorts, ignore.case = TRUE))
  }
  if (!is.null(input$subtype_text) & input$subtype_text != "") {
    ids <- sub(";", "|", input$subtype_text)
    res <-
      dplyr::filter(res, grepl(ids, DataSubtype, ignore.case = TRUE))
  }
  if (!is.null(input$type_text)) {
    res <- dplyr::filter(res, Type %in% input$type_text)
  }
  
  return(res)
})

# Show database information in repository page
df <- eventReactive(input$hubs_text,{
  res <- XenaData
  
  if (!is.null(input$hubs_text)) {
    res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
  }
  if (!is.null(input$cohorts_text) & input$cohorts_text != "") {
    ids <- sub(";", "|", input$cohorts_text)
    res <-
      dplyr::filter(res, grepl(ids, XenaCohorts, ignore.case = TRUE))
  }
  if (!is.null(input$subtype_text) & input$subtype_text != "") {
    ids <- sub(";", "|", input$subtype_text)
    res <-
      dplyr::filter(res, grepl(ids, DataSubtype, ignore.case = TRUE))
  }
  if (!is.null(input$type_text)) {
    res <- dplyr::filter(res, Type %in% input$type_text)
  }
  
  return(res)
})


  output$xena_table <- DT::renderDataTable(
    df()[, c(
      "XenaDatasets",
      "XenaHostNames",
      "XenaCohorts",
      "SampleCount",
      "DataSubtype",
      "Label",
      "Unit"
    )],
    colnames = c(
      "Dataset",
      "Hub",
      "Cohort",
      "Samples",
      "Subtype",
      "Label",
      "Unit"
    )
  )


# Keep selected database
selected_database <- reactive({
  s <- input$xena_table_rows_selected
  if (length(s)) {
    return(dataset()[s,])
  }
})

query_url <- reactive({
  s <- input$xena_table_rows_selected
  if (!is.null(s)) {
    data <- selected_database()
    xe <-
      UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)
    xe_query <- UCSCXenaTools::XenaQuery(xe)
    return(xe_query)
  }
})

# Show download url for selected database
w <- waiter::Waiter$new(id = "table", html = waiter::spin_wobblebar(), color = "white")
# w <-
#   waiter::Waiter$new(
#     id = "table",
#     html = waiter::spin_loaders(id = 1, color = "black", style = NULL),
#     color = "white"
#   )
observe({
  s <- input$xena_table_rows_selected
  if (!is.null(s)) {
    data <- selected_database()
    
    w$show() # Waiter add-ins
    Sys.sleep(3)
    output$table <- renderTable({
      data.frame(
        ID = 1:length(s),
        XenaCohorts = data$XenaCohorts,
        Label = data$Label,
        URL = unlist(lapply(query_url()$url, function(x) {
          as.character(tags$a(href = x, x))
        }))
      )
    },
    sanitize.text.function = function(x)
      x)
    
    shinyjs::show("show_data")
  } else {
    shinyjs::hide("show_data")
  }
})

# Dialog for showing selected data information
observeEvent(input$show_met, {
  s <- input$xena_table_rows_selected
  if (length(s)) {
    showModal(modalDialog(
      title = "Detail information...",
      size = "l",
      DT::DTOutput("detail_info")
    ))
  }
})

# Dialog for some operations of request data
observeEvent(input$req_data, {
  s <- input$xena_table_rows_selected
  if (length(s)) {
    showModal(
      modalDialog(
        title = "Submitted datasets:",
        size = "l",
        DT::DTOutput("table_query"),
        hr(),
        # actionButton(inputId = "load", label = "Load Data", icon = icon("upload"), style = "margin-bottom: 10px; margin-right: 75px;"),
        # shinyBS::bsPopover("load",
        #   title = "Tips",
        #   content = "Directly load data into R for analyses provided by modules or pipelines",
        #   placement = "bottom", options = list(container = "body")
        # ),
        # downloadButton(outputId = "download", label = "Download data directly", icon = icon("download"), style = "margin-bottom: 10px;"),
        shinyDirButton(
          id = "download",
          label = "Download data directly",
          title = "Please select a folder",
          icon = icon("download"),
          style = "margin-bottom: 10px;"
        ),
        downloadButton(
          outputId = "total_url",
          label = "Batch download in terminal",
          icon = icon("download"),
          style = "margin-bottom: 10px; margin-left: 50px;"
        ),
        shinyBS::bsPopover(
          "total_url",
          title = "Tips",
          content = "Download wget commands to download requested datasets.",
          placement = "bottom",
          options = list(container = "body")
        )
      )
    )
    
    output$table_query <-
      DT::renderDT(query_url(), options = list(dom = "t", scrollX = TRUE))
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
        message("Metadata for ", y, " is queried.")
        json_data <-
          purrr::map(json_data, ~ ifelse(length(.) > 0, paste(., collapse = ","), .))
        json_data <- tibble::enframe(json_data)
        json_data$value = unlist(json_data$value)
        json_data
      })
    m <- purrr::reduce(m, dplyr::full_join, by = "name")
    message("Metadata are loaded!")
    colnames(m) <- c("Metadata", paste0("dataset", 1:(ncol(m) - 1L)))
    
  }
  output$detail_info <-
    DT::renderDT(m, options = list(dom = "t", scrollX = TRUE))
})

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


# Download request data by XenaDownload function
# observeEvent(input$load, {
#   progress <- shiny::Progress$new()
#   on.exit(progress$close())
#   progress$set(message = "Begin to download files, Please wait...", value = 0)
#
#   UCSCXenaTools::XenaDownload(query_url())
#
#   progress$set(message = "Over...", value = 1)
# })

# Prepare request data by XenaPrepare function
# request_data <- eventReactive(input$load, {
#   xe_download <- UCSCXenaTools::XenaDownload(query_url(), download_probeMap = TRUE, trans_slash = TRUE)
#   return(UCSCXenaTools::XenaPrepare(xe_download))
# })

# Download buttom of request data with zip compress
# output$download <- downloadHandler(
#   filename = "database.zip",
#   contentType = "application/zip",
#   content = function(file) {
#     xe_download <- UCSCXenaTools::XenaDownload(query_url())
#     zip::zipr(zipfile = paste0(tempdir(), "/target_database.zip"), files = xe_download$destfiles, recurse = F)
#     file.copy(paste0(tempdir(), "/target_database.zip"), file)
#     file.remove(paste0(tempdir(), "/target_database.zip"))
#   }
# )
volumes <- c(home = fs::path_home(), root = "/")
shinyDirChoose(input, "download", roots = volumes, session = session)
observeEvent(input$download, {
  if (is.integer(input$download)) {
    print("No directory has been selected.")
  } else {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Begin to download files, Please wait...", value = 0)
    UCSCXenaTools::XenaDownload(
      query_url(),
      destdir = parseDirPath(volumes, input$download),
      download_probeMap = TRUE,
      trans_slash = TRUE
    )
    progress$set(message = "Over...", value = 1)
  }
})

# Show alert info when select rows from table
observeEvent(input$use_repository, {
  # Show a modal when the button is pressed
  shinyalert(
    title = "Repository Usage",
    text = paste(
      "Firstly, filter dataset table based on left filters or right search bar.",
      "Secondly, select datasets by clicking corresponding rows in dataset table.",
      "Lastly, click the button under the dataset table to check metadata or download datasets.",
      sep = "\n"
    ),
    type = "info",
    timer = 0,
    confirmButtonCol = "#202324"
  )
})
