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

output$ga_dataset_table <- DT::renderDataTable({
  show_table <- selected_database_add_url_and_phenotype() 
  if (!is.null(show_table)) {
    # 同时载入同队列的 phenotype (and probemap?)
    show_table <- show_table %>% 
      dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype", "Label", "Type", "download", "browse"))
    colnames(show_table)[1:4] <- c("Cohort", "Dataset", "N", "Subtype")
  } else {
    sendSweetAlert(session, title = "Warning!", text = "Please select datasets from Repository page firstly",
                   type = "warn")
  }
  
  show_table
}, escape = FALSE)