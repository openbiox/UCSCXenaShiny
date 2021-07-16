custom_file <- reactiveValues()
custom_file$fData <- NULL
custom_file$pData <- NULL

selected_database_add_url_and_phenotype <- reactive({
  data <- selected_database_add_url()
  
  if (!is.null(data)) {
    # Add phenotype datasets
    # Type == "clinicalMatrix"
    add_datasets <- dplyr::filter(
      XenaData,
      XenaCohorts %in% unique(data$XenaCohorts),
      Type == "clinicalMatrix",
      !XenaDatasets %in% data$XenaDatasets
    )

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
    
    if (!is.null(custom_file$fData)) {
      data <- dplyr::add_row(data, 
                             XenaCohorts = "Custom", 
                             XenaDatasets = "custom_feature_dataset", 
                             Type = "genomicMatrix",
                             SampleCount = ncol(custom_file$fData) - 1L)
    }
    if (!is.null(custom_file$pData)) {
      data <- dplyr::add_row(data, 
                             XenaCohorts = "Custom", 
                             XenaDatasets = "custom_phenotype_dataset", 
                             Type = "clinicalMatrix",
                             SampleCount = nrow(custom_file$pData))
    }
  } else if (!is.null(custom_file$pData) | !is.null(custom_file$fData)) {
    data <- dplyr::tibble()
    if (!is.null(custom_file$fData)) {
      data <- dplyr::bind_rows(
        data, 
        dplyr::tibble(
          XenaCohorts = "Custom", 
          XenaDatasets = "custom_feature_dataset", 
          Type = "genomicMatrix",
          SampleCount = ncol(custom_file$fData) - 1L,
          DataSubtype = NA_character_
        ))
    }
    if (!is.null(custom_file$pData)) {
      data <- dplyr::bind_rows(
        data, 
        dplyr::tibble(
          XenaCohorts = "Custom", 
          XenaDatasets = "custom_phenotype_dataset", 
          Type = "clinicalMatrix",
          SampleCount = nrow(custom_file$pData),
          DataSubtype = NA_character_
        ))
    }
  }
  data
})

selected_database_rm_phenotype <- reactive({
  data <- selected_database_add_url_and_phenotype()
  if (!is.null(data)) {
    # Remove phenotype datasets
    # Type != "clinicalMatrix"
    data <- subset(data, Type != "clinicalMatrix")
  }
  data
})

# Upload custom feature/phenotype data
observeEvent(input$ga_input_feature_file,{
  req(input$ga_input_feature_file)
  
  # ext <- tools::file_ext(input$ga_input_feature_file$name)
  # shiny::validate(need(ext %in% c("csv","tsv", "gz"), "Please upload a csv/tsv file"))
  
  inFile <- input$ga_input_feature_file
  if (is.null(inFile))
    return(NULL)
  df <- data.table::fread(inFile$datapath, header = TRUE)
  message("Saving custom feature data to temp directory.")
  saveRDS(df, file = file.path(tempdir(), "custom_feature_data.rds"))
  
  custom_file$fData <- df
})

observeEvent(input$ga_input_phenotype_file,{
  req(input$ga_input_phenotype_file)
  
  # ext <- tools::file_ext(input$ga_input_phenotype_file$name)
  # shiny::validate(need(ext %in% c("csv","tsv", "gz"), "Please upload a csv/tsv file"))
  
  inFile <- input$ga_input_phenotype_file
  if (is.null(inFile))
    return(NULL)
  df <- data.table::fread(inFile$datapath, header = TRUE)
  message("Saving custom phenotype data to temp directory.")
  saveRDS(df, file = file.path(tempdir(), "custom_phenotype_data.rds"))

  custom_file$pData <- df
})

output$ga_dataset_table <- DT::renderDataTable(
  {
    show_table <- selected_database_add_url_and_phenotype()
    if (!is.null(show_table)) {
      # 同时载入同队列的 phenotype (and probemap?)
      if (!"Label" %in% colnames(show_table)) {
        show_table <- show_table %>%
          dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype"))
      } else {
        show_table <- show_table %>%
          dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype", "Label", "Type", "download", "browse"))
      }
      
      colnames(show_table)[1:4] <- c("Cohort", "Dataset", "N", "Subtype")
    } else {
      sendSweetAlert(session,
        title = "Warning!", text = "Please select datasets from Repository page or upload your own data firstly!",
        type = "warning"
      )
    }
    
    show_table
  },
  escape = FALSE
)


# Individual analysis pages -------------------------------------------------------------

callModule(
  server.modules_ga_scatter_correlation, "module_ga_scatter_correlation",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file
)
callModule(
  server.modules_ga_matrix_correlation, "module_ga_matrix_correlation",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file
)
callModule(
  server.modules_ga_group_comparison, "module_ga_group_comparison",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file
)
callModule(
  server.modules_ga_surv_analysis, "module_ga_surv_analysis",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file
)

# Show use alert ----------------------------------------------------------

observeEvent(input$use_ga_page, {
  shinyalert(
    title = "General Analysis Usage",
    text = paste(
      "Firstly, select datasets from Repository page, the datasets and corresonding clinical datasets will be automatically loaded here (You can also upload your own data with the upload button).",
      "Secondly, use any analysis feature below by clicking the tab.",
      "Lastly, control how to analyze from left panel and filter samples from right panel. The result plot should be shown at the middle.",
      sep = "\n\n"
    ),
    type = "info",
    timer = 0,
    confirmButtonCol = "#202324"
  )
})

observeEvent(input$ga_drop_button, {
  if (is.null(selected_database_add_url_and_phenotype())) {
    sendSweetAlert(session,
      title = "Warning!", text = "Please select datasets from Repository page firstly",
      type = "warning"
    )
  }
})
