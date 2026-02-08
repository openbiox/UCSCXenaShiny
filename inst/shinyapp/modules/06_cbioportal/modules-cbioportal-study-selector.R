# cBioPortal Study Selection Module UI
ui.modules_cbioportal_study_selector <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        12,
        h4(strong("Step 1: Select cBioPortal Study"), style = "color: #2c3e50;"),
        br(),
        
        # Study selection interface
        fluidRow(
          column(
            8,
            selectInput(
              inputId = ns("study_selection"),
              label = "Choose a study:",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              width = "100%"
            )
          ),
          column(
            4,
            br(),
            actionButton(
              inputId = ns("load_studies"),
              label = "Refresh Studies",
              icon = icon("refresh"),
              class = "btn-primary",
              style = "width: 100%; margin-top: 5px;"
            )
          )
        ),
        
        # Study information display
        br(),
        wellPanel(
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
          verbatimTextOutput(ns("study_info"))
        ),
        
        br(),
        h4(strong("Step 2: Select Data Type"), style = "color: #2c3e50;"),
        
        # Data type selection
        fluidRow(
          column(
            8,
            selectInput(
              inputId = ns("data_type_selection"),
              label = "Choose data type:",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              width = "100%"
            )
          ),
          column(
            4,
            br(),
            actionButton(
              inputId = ns("load_data"),
              label = "Load Data",
              icon = icon("download"),
              class = "btn-success",
              style = "width: 100%; margin-top: 5px;"
            )
          )
        ),
        
        # Data loading status
        br(),
        wellPanel(
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
          verbatimTextOutput(ns("data_status"))
        )
      )
    )
  )
}

# cBioPortal Study Selection Module Server
modules_cbioportal_study_selector_Server <- function(input, output, session, database = "cbioportal") {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    studies = NULL,
    selected_study_data = NULL,
    molecular_data = NULL,
    clinical_data = NULL
  )
  
  # Load studies on module initialization and refresh button
  observeEvent(c(input$load_studies, TRUE), {
    if (is.null(values$studies) || input$load_studies > 0) {
      withProgress(message = "Loading cBioPortal studies...", {
        tryCatch({
          studies <- get_cbioportal_studies(base_url = "public")
          values$studies <- studies
          
          if (nrow(studies) > 0) {
            study_choices <- setNames(studies$studyId, paste0(studies$name, " (", studies$studyId, ")"))
            updateSelectInput(session, "study_selection", choices = study_choices)
            showNotification("Studies loaded successfully!", type = "success")
          } else {
            showNotification("No studies found or connection failed.", type = "warning")
          }
        }, error = function(e) {
          showNotification(paste("Error loading studies:", e$message), type = "error")
        })
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Display study information when selected
  output$study_info <- renderText({
    if (!is.null(input$study_selection) && !is.null(values$studies)) {
      study_row <- values$studies[values$studies$studyId == input$study_selection, ]
      if (nrow(study_row) > 0) {
        paste0(
          "Study: ", study_row$name, "\n",
          "Description: ", if (!is.na(study_row$description)) study_row$description else "Not available", "\n",
          "Cancer Type: ", if (!is.na(study_row$cancerTypeId)) study_row$cancerTypeId else "Not specified", "\n",
          "Sample Count: ", if (!is.na(study_row$allSampleCount)) study_row$allSampleCount else "Not specified"
        )
      } else {
        "Select a study to see details."
      }
    } else {
      "Select a study to see details."
    }
  })
  
  # Load study data when study is selected
  observeEvent(input$study_selection, {
    if (!is.null(input$study_selection) && input$study_selection != "") {
      withProgress(message = paste("Loading metadata for study:", input$study_selection), {
        tryCatch({
          # Get study info
          study_info <- get_cbioportal_study_info(input$study_selection, base_url = "public")
          
          # Get available molecular profiles
          profiles <- get_cbioportal_profiles(input$study_selection, base_url = "public")
          
          if (nrow(profiles) > 0) {
            # Filter to expression/RNA profiles
            rna_profiles <- profiles[grepl("rna|mrna|expression", profiles$name, ignore.case = TRUE), ]
            
            if (nrow(rna_profiles) > 0) {
              profile_choices <- setNames(
                rna_profiles$molecularProfileId,
                paste0(rna_profiles$name, " (", rna_profiles$datatype, ")")
              )
              updateSelectInput(session, "data_type_selection", choices = profile_choices)
              values$selected_study_data <- list(info = study_info, profiles = profiles)
              
              # Load clinical data
              values$clinical_data <- get_cbioportal_clinical_data(input$study_selection, base_url = "public")
              
              showNotification("Study metadata loaded successfully!", type = "success")
            } else {
              showNotification("No RNA expression profiles found for this study.", type = "warning")
              updateSelectInput(session, "data_type_selection", choices = NULL)
            }
          } else {
            showNotification("No molecular profiles found for this study.", type = "error")
          }
        }, error = function(e) {
          showNotification(paste("Error loading study data:", e$message), type = "error")
        })
      })
    }
  })
  
  # Load molecular data when data type is selected
  observeEvent(input$load_data, {
    if (!is.null(input$data_type_selection) && !is.null(input$study_selection)) {
      withProgress(message = paste("Loading molecular data..."), {
        tryCatch({
          # NOTE: Loading all genes without filtering could be memory intensive for large studies.
          # In production, consider:
          # 1. Adding a gene filter input field
          # 2. Implementing pagination
          # 3. Loading data on-demand for specific analyses
          # For now, setting genes = NULL loads all available data for the profile
          molecular_data <- get_cbioportal_molecular_data(
            study_id = input$study_selection,
            genes = NULL,  # TODO: Consider adding gene filter for large studies
            molecular_profile_id = input$data_type_selection,
            base_url = "public"
          )
          
          if (nrow(molecular_data) > 0) {
            values$molecular_data <- molecular_data
            showNotification(
              paste("Loaded", nrow(molecular_data), "data points"), 
              type = "success"
            )
          } else {
            showNotification("No molecular data found for selected profile.", type = "warning")
          }
        }, error = function(e) {
          showNotification(paste("Error loading molecular data:", e$message), type = "error")
        })
      })
    }
  })
  
  # Display data loading status
  output$data_status <- renderText({
    status_lines <- c()
    
    if (!is.null(values$selected_study_data)) {
      status_lines <- c(status_lines, paste("✓ Study data loaded"))
    }
    
    if (!is.null(values$clinical_data)) {
      status_lines <- c(status_lines, paste("✓ Clinical data:", nrow(values$clinical_data), "samples"))
    }
    
    if (!is.null(values$molecular_data)) {
      status_lines <- c(status_lines, paste("✓ Molecular data:", nrow(values$molecular_data), "data points"))
    }
    
    if (length(status_lines) == 0) {
      return("No data loaded yet.")
    }
    
    paste(status_lines, collapse = "\n")
  })
  
  # Return reactive values for use by other modules
  return(
    reactive({
      list(
        study_id = input$study_selection,
        study_data = values$selected_study_data,
        clinical_data = values$clinical_data,
        molecular_data = values$molecular_data,
        data_type = input$data_type_selection
      )
    })
  )
}