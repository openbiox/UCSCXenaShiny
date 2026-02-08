ui.modules_ga_custom_heatmap <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Heatmap Controls"),
          helpText("Create custom heatmaps with molecular data. Select features/genes and configure visualization options."),
          uiOutput(ns("ga_data1_id")),
          virtualSelectInput(
            inputId = ns("ga_data1_mid"), # molecule identifier
            label = "Gene/Feature identifiers:",
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          numericInput(
            inputId = ns("max_genes"), 
            label = "Maximum features to display:", 
            value = 50, 
            min = 5, 
            max = 500,
            step = 5
          ),
          selectInput(
            inputId = ns("clustering_method"), 
            label = "Clustering method:",
            choices = c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median", "centroid"),
            selected = "complete"
          ),
          materialSwitch(
            inputId = ns("cluster_rows"),
            label = "Cluster rows (features)?",
            value = TRUE,
            status = "primary"
          ),
          materialSwitch(
            inputId = ns("cluster_cols"),
            label = "Cluster columns (samples)?",
            value = TRUE,
            status = "primary"
          ),
          materialSwitch(
            inputId = ns("show_row_names"),
            label = "Show feature names?",
            value = TRUE,
            status = "primary"
          ),
          materialSwitch(
            inputId = ns("show_col_names"),
            label = "Show sample names?",
            value = FALSE,
            status = "primary"
          ),
          selectInput(
            inputId = ns("color_palette"), 
            label = "Color palette:",
            choices = c("viridis", "plasma", "inferno", "magma", "RdYlBu", "RdBu", "Spectral"),
            selected = "RdYlBu"
          ),
          actionBttn(
            inputId = ns("ga_go"),
            label = "Generate Heatmap",
            style = "gradient",
            icon = icon("fire"),
            color = "default",
            block = TRUE,
            size = "sm"
          )
        )
      ),
      column(
        6,
        plotOutput(ns("ga_output"), height = "600px"),
        DT::dataTableOutput(ns("ga_output_data"))
      ),
      column(
        3,
        wellPanel(
          h4("Sample Grouping"),
          helpText("Define custom sample groups or use phenotype data to group samples for visualization."),
          uiOutput(ns("ga_data_phenotype_id")),
          uiOutput(ns("ga_grouping_variable")),
          selectInput(
            inputId = ns("grouping_method"),
            label = "Grouping method:",
            choices = c("Phenotype variable" = "phenotype", "Custom definition" = "custom"),
            selected = "phenotype"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("grouping_method"), "'] == 'custom'"),
            textAreaInput(
              inputId = ns("custom_groups"),
              label = "Custom groups (one group per line, samples separated by commas):",
              placeholder = "Group1: TCGA-AA-3502,TCGA-AA-3506\nGroup2: TCGA-AA-3510,TCGA-AA-3514",
              rows = 4
            ),
            helpText("Format: GroupName: sample1,sample2,sample3")
          ),
          actionBttn(
            inputId = ns("ga_filter_button"),
            label = "Apply Grouping",
            color = "primary",
            style = "bordered",
            size = "sm"
          ),
          tags$br(),
          tags$br(),
          h4("Download"),
          numericInput(inputId = ns("height"), label = "Height", value = 8),
          numericInput(inputId = ns("width"), label = "Width", value = 10),
          column(
            width = 12, align = "center",
            prettyRadioButtons(
              inputId = ns("device"),
              label = "Choose plot format",
              choices = c("png", "pdf"),
              selected = "png",
              inline = TRUE,
              icon = icon("check"),
              animation = "jelly",
              fill = TRUE
            )
          ),
          downloadBttn(
            outputId = ns("download"),
            style = "gradient",
            color = "default",
            block = TRUE,
            size = "sm"
          )
        )
      )
    )
  )
}


server.modules_ga_custom_heatmap <- function(input, output, session,
                                           selected_database_rm_phenotype,
                                           selected_database_add_url_and_phenotype,
                                           custom_file) {
  ns <- session$ns
  
  # Data source selection - simplified to match other GA modules
  output$ga_data1_id <- renderUI({
    show_table <- selected_database_rm_phenotype()
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Select dataset for heatmap:",
      choices = c("NONE", unique(show_table$XenaDatasets)),
      selected = "NONE",
      multiple = FALSE
    )
  })
  
  # Phenotype data selection for grouping
  phenotype_data_choices <- reactive({
    req(selected_database_add_url_and_phenotype())
    
    phenotype_data <- selected_database_add_url_and_phenotype() %>%
      dplyr::filter(Type == "clinicalMatrix")
    
    if (nrow(phenotype_data) > 0) {
      choices <- as.list(phenotype_data$XenaDatasets)
      names(choices) <- paste(phenotype_data$XenaCohorts,
                            phenotype_data$XenaDatasets,
                            sep = " - ")
      choices
    } else {
      list("No phenotype data available" = "none")
    }
  })
  
  output$ga_data_phenotype_id <- renderUI({
    selectInput(
      inputId = ns("ga_data_phenotype_id"),
      label = "Phenotype data for grouping:",
      choices = phenotype_data_choices(),
      selected = phenotype_data_choices()[[1]],
      multiple = FALSE
    )
  })
  
  observe({
    updateVirtualSelect(
      "ga_data1_mid",
      choices = if (is.null(custom_file$fData)) all_preload_identifiers else
        unique(c(colnames(custom_file$fData)[-1], all_preload_identifiers)),
      selected = c("TP53", "KRAS", "PTEN")
    )
  })
  
  # Dynamic phenotype variable selection
  phenotype_variables <- reactive({
    req(input$ga_data_phenotype_id)
    
    if (input$ga_data_phenotype_id == "custom_phenotype_dataset") {
      req(custom_file$pData)
      vars <- colnames(custom_file$pData)[-1] # Exclude first column (sample ID)
      return(vars)
    } else if (input$ga_data_phenotype_id != "none") {
      tryCatch({
        phenotype_data <- query_xena_phenotype(input$ga_data_phenotype_id)
        vars <- colnames(phenotype_data)[-1] # Exclude first column (sample ID)
        return(vars)
      }, error = function(e) {
        return(character(0))
      })
    }
    character(0)
  })
  
  output$ga_grouping_variable <- renderUI({
    vars <- phenotype_variables()
    if (length(vars) > 0) {
      selectInput(
        inputId = ns("ga_grouping_variable"),
        label = "Phenotype variable for grouping:",
        choices = vars,
        selected = vars[1]
      )
    } else {
      p("No phenotype variables available")
    }
  })
  
  # Data loading and processing - returns tidy long format
  heatmap_data_long <- eventReactive(input$ga_go, {
    req(input$ga_data1_id, input$ga_data1_mid)
    
    # Validate dataset selection
    if (input$ga_data1_id == "NONE") {
      showNotification("Please select a dataset first", type = "warning")
      return(NULL)
    }
    
    withProgress(message = "Loading data...", value = 0, {
      incProgress(0.3, detail = "Fetching molecular data")
      
      # Query data for multiple genes/features
      df <- purrr::map(input$ga_data1_mid, function(x) {
        message("Querying data of identifier ", x, " from dataset: ", input$ga_data1_id)
        
        if (input$ga_data1_id == "custom_feature_dataset") {
          req(custom_file$fData)
          custom_data <- custom_file$fData
          
          if (x %in% colnames(custom_data)[-1]) {
            sample_col <- colnames(custom_data)[1]
            data_vector <- custom_data[[x]]
            names(data_vector) <- custom_data[[sample_col]]
            data_vector
          } else {
            showNotification(paste("Feature", x, "not found in custom data"), type = "warning")
            return(NULL)
          }
        } else {
          data <- query_molecule_value(input$ga_data1_id, x)
          data
        }
      }) %>%
        purrr::map(function(data_vector) {
          if (is.null(data_vector) || length(data_vector) == 0) {
            return(NULL)
          }
          
          data_df <- dplyr::tibble(
            sample = names(data_vector),
            value = as.numeric(data_vector)
          )
          data_df
        }) %>%
        purrr::keep(~ !is.null(.))
      
      # Add feature identifiers to each data frame (stored in 'gene' column for consistency)
      for (i in seq_along(df)) {
        df[[i]]$gene <- input$ga_data1_mid[i]
      }
      
      # Combine all data
      if (length(df) == 0) {
        showNotification("No valid data found for selected features", type = "error")
        return(NULL)
      }
      
      final_df <- dplyr::bind_rows(df)
      
      if (is.null(final_df) || nrow(final_df) == 0) {
        showNotification("No data available for selected features", type = "error")
        return(NULL)
      }
      
      incProgress(0.3, detail = "Processing data")
      
      # Filter features if needed
      features_to_include <- input$ga_data1_mid
      if (length(features_to_include) > input$max_genes) {
        features_to_include <- features_to_include[1:input$max_genes]
        final_df <- final_df %>% dplyr::filter(gene %in% features_to_include)
        showNotification(paste("Limited to", input$max_genes, "features"), type = "message")
      }
      
      # Remove rows with NA values
      final_df <- final_df %>% dplyr::filter(!is.na(value))
      
      incProgress(0.4, detail = "Finalizing")
      
      return(final_df)
    })
  })
  
  # Sample grouping logic
  sample_groups <- eventReactive(input$ga_filter_button, {
    req(heatmap_data_long())
    
    data <- heatmap_data_long()
    available_samples <- unique(data$sample)
    
    if (input$grouping_method == "custom") {
      # Parse custom groups
      req(input$custom_groups)
      
      tryCatch({
        group_lines <- strsplit(input$custom_groups, "\n")[[1]]
        group_lines <- group_lines[nzchar(group_lines)]  # Remove empty lines
        
        group_list <- list()
        for (line in group_lines) {
          if (grepl(":", line)) {
            parts <- strsplit(line, ":")[[1]]
            group_name <- trimws(parts[1])
            samples <- trimws(strsplit(parts[2], ",")[[1]])
            
            # Filter to only include available samples
            samples <- samples[samples %in% available_samples]
            
            if (length(samples) > 0) {
              group_list[[group_name]] <- samples
            }
          }
        }
        
        if (length(group_list) == 0) {
          showNotification("No valid groups found in custom input", type = "warning")
          return(NULL)
        }
        
        # Create a data frame with sample-group mapping
        group_df <- purrr::map2_dfr(group_list, names(group_list), function(samples, group_name) {
          data.frame(sample = samples, group = group_name, stringsAsFactors = FALSE)
        })
        
        return(group_df)
        
      }, error = function(e) {
        showNotification(paste("Error parsing custom groups:", e$message), type = "error")
        return(NULL)
      })
      
    } else if (input$grouping_method == "phenotype") {
      # Use phenotype variable for grouping
      req(input$ga_data_phenotype_id, input$ga_grouping_variable)
      
      tryCatch({
        if (input$ga_data_phenotype_id == "custom_phenotype_dataset") {
          req(custom_file$pData)
          phenotype_data <- custom_file$pData
        } else {
          showNotification("Phenotype-based grouping not yet fully implemented for Xena data", type = "info")
          return(NULL)
        }
        
        # Create grouping based on phenotype variable
        sample_col <- colnames(phenotype_data)[1]
        group_col <- input$ga_grouping_variable
        
        group_df <- phenotype_data[, c(sample_col, group_col)]
        colnames(group_df) <- c("sample", "group")
        
        # Filter to only include samples present in expression data
        group_df <- group_df[group_df$sample %in% available_samples, ]
        
        # Remove NA values
        group_df <- group_df[!is.na(group_df$group), ]
        
        return(group_df)
        
      }, error = function(e) {
        showNotification(paste("Error processing phenotype groups:", e$message), type = "error")
        return(NULL)
      })
    }
    
    return(NULL)
  })
  
  # Generate heatmap using tidyHeatmap
  heatmap_plot <- reactive({
    req(heatmap_data_long())
    
    if (!requireNamespace("tidyHeatmap", quietly = TRUE)) {
      showNotification("tidyHeatmap package is required but not installed", type = "error")
      return(NULL)
    }
    
    data <- heatmap_data_long()
    groups <- sample_groups()
    
    withProgress(message = "Generating heatmap...", value = 0, {
      incProgress(0.5, detail = "Creating heatmap")
      
      tryCatch({
        # Check if grouping is applied (sample_groups() is only populated when filter button is clicked)
        has_groups <- !is.null(groups) && nrow(groups) > 0
        
        # Merge group information if available
        if (has_groups) {
          data <- data %>%
            dplyr::left_join(groups, by = "sample") %>%
            dplyr::mutate(group = ifelse(is.na(group), "Ungrouped", group))
        }
        
        # Set up color palette
        # tidyHeatmap natively accepts:
        # - viridis palette names: "viridis", "plasma", "inferno", "magma"
        # - RColorBrewer palette names: "RdYlBu", "RdBu", "Spectral", etc.
        palette_name <- input$color_palette
        
        # Create base heatmap
        # Note: scale = "row" applies z-score normalization per feature for better visualization
        # clustering_method is applied to both rows and columns
        p <- data %>%
          tidyHeatmap::heatmap(
            .row = gene,
            .column = sample,
            .value = value,
            scale = "row",
            clustering_method = input$clustering_method,
            cluster_rows = input$cluster_rows,
            cluster_columns = input$cluster_cols,
            show_row_names = input$show_row_names,
            show_column_names = input$show_col_names,
            palette_value = palette_name
          )
        
        # Add group annotation if groups are defined
        # annotation_tile automatically assigns distinct colors to each group
        if (has_groups) {
          p <- p %>% tidyHeatmap::annotation_tile(group)
        }
        
        return(p)
        
      }, error = function(e) {
        showNotification(paste("Error generating heatmap:", e$message), type = "error")
        message("Heatmap error details: ", e$message)
        return(NULL)
      })
    })
  })
  
  output$ga_output <- renderPlot({
    heatmap_plot()
  })
  
  # Data table output
  output$ga_output_data <- DT::renderDataTable({
    req(heatmap_data_long())
    
    data <- heatmap_data_long()
    # Convert to wide format for display
    data_wide <- data %>%
      tidyr::pivot_wider(names_from = gene, values_from = value)
    
    DT::datatable(
      data_wide,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_custom_heatmap.", input$device)
    },
    content = function(file) {
      p <- heatmap_plot()
      
      if (!is.null(p)) {
        if (input$device == "png") {
          png(file, width = input$width, height = input$height, units = "in", res = 300)
        } else {
          pdf(file, width = input$width, height = input$height)
        }
        
        # Print the heatmap (tidyHeatmap handles rendering internally)
        print(p)
        
        dev.off()
      }
    }
  )
}
