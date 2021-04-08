selected_database_add_url_and_phenotype <- reactive({
  data <- selected_database_add_url()
  if (!is.null(data)) {
    # Add phenotype datasets
    # Type == "clinicalMatrix"
    add_datasets <- dplyr::filter(
      XenaData, 
      XenaCohorts %in% unique(data$XenaCohorts),
      Type == "clinicalMatrix",
      !XenaDatasets %in% data$XenaDatasets)
    
    if (nrow(add_datasets) > 0) {
      message("Querying extra Phenotype datasets for pre-selected datasets.")
      add_query <- xe_query_url(add_datasets)
      
      add_datasets$download <- unlist(lapply(add_query$url, function(x) {
        as.character(tags$a(href = x, "download link"))
      }))
      add_datasets$browse <- unlist(lapply(add_query$browse, function(x) {
        as.character(tags$a(href = x, "browse Xena dataset page"))
      })) 
      
      data <- dplyr::bind_rows(data, add_datasets) %>% 
        dplyr::arrange(XenaCohorts)
    }
  }
  data
})

selected_database_rm_phenotype <- reactive({
  data <- selected_database()
  if (!is.null(data)) {
    # Remove phenotype datasets
    # Type != "clinicalMatrix"
    data <- subset(data, Type != "clinicalMatrix")
  }
  data
})

output$ga_dataset_table <- DT::renderDataTable({
  show_table <- selected_database_add_url_and_phenotype() 
  if (!is.null(show_table)) {
    # 同时载入同队列的 phenotype (and probemap?)
    show_table <- show_table %>% 
      dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype", "Label", "Type", "download", "browse"))
    colnames(show_table)[1:4] <- c("Cohort", "Dataset", "N", "Subtype")
  } else {
    sendSweetAlert(session, title = "Warning!", text = "Please select datasets from Repository page firstly",
                   type = "warning")
  }
  
  show_table
}, escape = FALSE)


# Scatter and Correlation -------------------------------------------------------------

output$ga_data1_id <- renderUI({
  show_table <- selected_database_rm_phenotype()
  selectInput(
    inputId = "ga_data1_id",
    label = "Select dataset 1:",
    choices = c("NONE", unique(show_table$XenaDatasets)),
    selected = "NONE",
    multiple = FALSE
  )
})

observe({
  updateSelectizeInput(
    session,
    "ga_data1_mid",
    choices = all_preload_identifiers,
    selected = "TP53",
    server = TRUE
  )
})

output$ga_data2_id <- renderUI({
  show_table <- selected_database_rm_phenotype()
  selectInput(
    inputId = "ga_data2_id",
    label = "Select dataset 2:",
    choices = c("NONE", unique(show_table$XenaDatasets)),
    selected = "NONE",
    multiple = FALSE
  )
})

observe({
  updateSelectizeInput(
    session,
    "ga_data2_mid",
    choices = all_preload_identifiers,
    selected = "TP53",
    server = TRUE
  )
})

p_scatter <- eventReactive(input$ga_go, {
  tryCatch(
    vis_identifier_cor(
      isolate(input$ga_data1_id),
      isolate(input$ga_data1_mid),
      isolate(input$ga_data2_id), 
      isolate(input$ga_data2_mid)),
    error = function(e) {
      "Error"
    }
  )
})

observeEvent(input$ga_go, {
  # Analyze correlation with 2 input datasets and identifiers
  output$ga_output <- renderPlot(
    if (inherits(p_scatter(), "ggplot")) {
      p_scatter()
    } else {
      sendSweetAlert(
        session,
        title = "Error",
        text = "Error to query data and plot. Please make sure the two selected datasets are 'genomicMatrix' type.",
        type = "error")
    }
  )
  
  output$ga_output_data <- DT::renderDataTable(server = FALSE, {
    if (inherits(p_scatter(), "ggplot")) {
      DT::datatable(
        p_scatter()$data,
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "csv", text = "Download Current Page", filename = "page",
                 exportOptions = list(
                   modifier = list(page = "current")
                 )
            ),
            list(extend = "csv", text = "Download Full Results", filename = "data",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            )
          )
        )
      )
    }
  })
})

output$ga_data_filter1_id <- renderUI({
  show_table <- selected_database_add_url_and_phenotype()
  selectInput(
    inputId = "ga_data_filter1_id",
    label = "Select phenotype dataset:",
    choices = c("NONE", unique(show_table$XenaDatasets[show_table$XenaDatasets %in% phenotype_datasets])),
    selected = "NONE",
    multiple = FALSE
  )
})

observeEvent(input$ga_filter_button, {
  message("Sample filter button is clicked by user.")
  pdataset <- setdiff(input$ga_data_filter1_id, "NONE")

  if (length(pdataset)) {
    showModal(
      modalDialog(
        title = "Filter samples for analysis",
        size = "l",
        fluidPage(
          h4("Select available columns on the left to right"),
          h6("NOTE: some useless columns are automatically filtered out by us."),
          fluidRow(
            uiOutput("ga_col_chooser")
          ),
          actionBttn(
            inputId = "show_or_update_ptable",
            label = "Show/Update Phenotype Table",
            color = "primary",
            style = "bordered"
          ),
          h4("Filter rows by searchPanel.\nFinally specify sample column and hit button."),
          fluidRow(
            DT::dataTableOutput("ga_phenotype_data")
          )
        )
      )
    )
    
    phenotype_table <- XenaGenerate(subset = XenaDatasets == pdataset) %>% 
      XenaQuery() %>% 
      XenaDownload(destdir = XENA_DEST) %>% 
      XenaPrepare()
    
    output$ga_col_chooser <- renderUI({
      chooserInput("ga_col_chooser", "Available columns", "Selected columns",
                   colnames(phenotype_table)[-c(1:4)], colnames(phenotype_table)[1:4], size = 5, multiple = TRUE
      )
    })
    
    observeEvent(input$show_or_update_ptable, {
      selected_cols <- isolate(input$ga_col_chooser$right)
      if (length(selected_cols)) {
        print(selected_cols)
        
        output$ga_phenotype_data <- DT::renderDataTable(server = FALSE, {
          DT::datatable(
            phenotype_table %>% 
              dplyr::select(all_of(selected_cols)),
            rownames = FALSE,
            extensions = c("Buttons", "Select", "SearchPanes", "Scroller"),
            options = list(
              dom = 'Bfrtip', # P
              buttons = list(
                "searchPanes",
                list(extend = "csv", text = "Download Current Page", filename = "page",
                     exportOptions = list(
                       modifier = list(page = "current")
                     )
                ),
                list(extend = "csv", text = "Download Full Results", filename = "data",
                     exportOptions = list(
                       modifier = list(page = "all")
                     )
                )
              ),
              scrollY = 350,
              scrollX = 300,
              deferRender = TRUE,
              scroller = TRUE,
              stateSave = TRUE
            ),
            selection = 'none'
          )
        })
      } else {
        sendSweetAlert(session, title = "Warning", type = "warn", text = "Please select at least 1 column!")
      }
    })
    

    observeEvent(input$ga_phenotype_data_rows_all, {
      rows_filtered <- input$ga_phenotype_data_rows_all
      print(rows_filtered)
    })
    
  } else {
    sendSweetAlert(session, title = "Warning", type = "warn", text = "Please select a dataset!")
  }
})

# Show use alert ----------------------------------------------------------

observeEvent(input$use_ga_page, {
  shinyalert(
    title = "General Analysis Usage",
    text = paste(
      "Firstly, select datasets from Repository page, the datasets and corresonding clinical datasets will be automatically loaded here.",
      "Secondly, use any analysis feature below by clicking the tab.",
      "Lastly, control how to analyze from left panel and filter samples from right panel. The result plot should be shown at the middle.",
      sep = "\n\n"
    ),
    type = "info",
    timer = 0,
    confirmButtonCol = "#202324"
  )
})
