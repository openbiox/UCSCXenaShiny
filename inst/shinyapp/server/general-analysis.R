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

output$ga_dataset_table <- DT::renderDataTable(
  {
    show_table <- selected_database_add_url_and_phenotype()
    if (!is.null(show_table)) {
      # 同时载入同队列的 phenotype (and probemap?)
      show_table <- show_table %>%
        dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype", "Label", "Type", "download", "browse"))
      colnames(show_table)[1:4] <- c("Cohort", "Dataset", "N", "Subtype")
    }
    # } else {
    #   sendSweetAlert(session,
    #     title = "Warning!", text = "Please select datasets from Repository page firstly",
    #     type = "warning"
    #   )
    # }

    show_table
  },
  escape = FALSE
)


# Individual analysis pages -------------------------------------------------------------

callModule(
  server.modules_ga_scatter_correlation, "module_ga_scatter_correlation",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype
)
callModule(
  server.modules_ga_matrix_correlation, "module_ga_matrix_correlation",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype
)
callModule(
  server.modules_ga_group_comparison, "module_ga_group_comparison",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype
)
callModule(
  server.modules_ga_surv_analysis, "module_ga_surv_analysis",
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype
)

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

observeEvent(input$ga_drop_button, {
  if (is.null(selected_database_add_url_and_phenotype())) {
    sendSweetAlert(session,
      title = "Warning!", text = "Please select datasets from Repository page firstly",
      type = "warning"
    )
  }
})
