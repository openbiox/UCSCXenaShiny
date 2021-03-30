output$ga_dataset_table <- DT::renderDataTable({
  show_table <- selected_database_add_url() 
  if (!is.null(show_table)) {
    show_table <- show_table %>% 
      dplyr::select(c("XenaCohorts", "XenaDatasets", "SampleCount", "DataSubtype", "Label", "Type", "download", "browse"))
    colnames(show_table)[1:4] <- c("Cohort", "Dataset", "N", "Subtype")
  } else {
    sendSweetAlert(session, title = "Warning!", text = "Please select datasets from Repository page firstly",
                   type = "warn")
  }
  # 同时载入同队列的 phenotype (and probemap?)
  show_table
}, escape = FALSE)