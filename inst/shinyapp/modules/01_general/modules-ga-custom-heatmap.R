ui.modules_ga_custom_heatmap <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Heatmap Controls"),
          helpText("Create custom heatmaps with expression data. Select genes and configure visualization options."),
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
            label = "Maximum genes to display:", 
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
            label = "Cluster rows (genes)?",
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
            label = "Show gene names?",
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
  
  # Data source selection
  data1_choices <- reactive({
    req(selected_database_rm_phenotype())
    
    choices <- as.list(selected_database_rm_phenotype()$XenaDatasets)
    names(choices) <- paste(selected_database_rm_phenotype()$XenaCohorts,
                          selected_database_rm_phenotype()$XenaDatasets,
                          sep = " - ")
    choices
  })
  
  output$ga_data1_id <- renderUI({
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Data for heatmap:",
      choices = data1_choices(),
      selected = data1_choices()[[1]],
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
  
  # Data loading and processing
  heatmap_data <- eventReactive(input$ga_go, {
    req(input$ga_data1_id, input$ga_data1_mid)
    
    withProgress(message = "Loading data...", value = 0, {
      incProgress(0.3, detail = "Fetching expression data")
      
      # Query data for multiple genes using the same pattern as other modules
      df <- purrr::map(input$ga_data1_mid, function(x) {
        message("Querying data of identifier ", x, " from dataset: ", input$ga_data1_id)
        
        if (input$ga_data1_id == "custom_feature_dataset") {
          # For custom data, we need to get data differently
          req(custom_file$fData)
          custom_data <- custom_file$fData
          
          # Check if the gene exists in custom data
          if (x %in% colnames(custom_data)[-1]) {
            sample_col <- colnames(custom_data)[1]
            data_vector <- custom_data[[x]]
            names(data_vector) <- custom_data[[sample_col]]
            data_vector
          } else {
            showNotification(paste("Gene", x, "not found in custom data"), type = "warning")
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
            y = as.numeric(data_vector)
          )
          
          # Use the gene name as column name
          gene_name <- names(data_vector)[1]
          if (is.null(gene_name) || gene_name == "") {
            # If no name, use the vector itself to find the gene name
            # This is a bit tricky - we need to track which gene this is
            # For now, we'll use the iteration context
            gene_name <- "unknown"
          }
          
          colnames(data_df)[2] <- gene_name
          data_df
        }) %>%
        purrr::keep(~ !is.null(.))
      
      # Set correct gene names
      for (i in seq_along(df)) {
        colnames(df[[i]])[2] <- input$ga_data1_mid[i]
      }
      
      # Join all the data
      if (length(df) == 0) {
        showNotification("No valid data found for selected genes", type = "error")
        return(NULL)
      }
      
      final_df <- df %>%
        purrr::reduce(dplyr::full_join, by = "sample")
      
      if (is.null(final_df) || nrow(final_df) == 0) {
        showNotification("No data available for selected genes", type = "error")
        return(NULL)
      }
      
      incProgress(0.3, detail = "Processing data")
      
      # Filter genes if needed
      genes_to_include <- input$ga_data1_mid
      if (length(genes_to_include) > input$max_genes) {
        genes_to_include <- genes_to_include[1:input$max_genes]
        final_df <- final_df[, c("sample", genes_to_include)]
        showNotification(paste("Limited to", input$max_genes, "genes"), type = "message")
      }
      
      # Remove rows with all NA values
      final_df <- final_df[rowSums(is.na(final_df[, -1])) < (ncol(final_df) - 1), ]
      
      incProgress(0.4, detail = "Finalizing")
      
      return(final_df)
    })
  })
  
  # Sample grouping logic
  sample_groups <- eventReactive(input$ga_filter_button, {
    req(heatmap_data())
    
    data <- heatmap_data()
    available_samples <- data$sample
    
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
          # Query phenotype data from Xena
          # This would need to be implemented with proper Xena query functions
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
  
  # Enhanced heatmap generation with grouping
  heatmap_plot_with_groups <- reactive({
    req(heatmap_data())
    
    data <- heatmap_data()
    groups <- sample_groups()
    
    withProgress(message = "Generating heatmap with groupings...", value = 0, {
      incProgress(0.5, detail = "Creating heatmap")
      
      tryCatch({
        # Convert to matrix format (genes in rows, samples in columns)
        data_matrix <- as.matrix(data[, -1])  # Remove sample column
        rownames(data_matrix) <- data$sample
        data_matrix <- t(data_matrix)  # Transpose so genes are rows
        
        # Remove NA rows and columns
        data_matrix <- data_matrix[rowSums(is.na(data_matrix)) < ncol(data_matrix), ]
        data_matrix <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
        
        if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0) {
          showNotification("No valid data for heatmap after filtering", type = "error")
          return(NULL)
        }
        
        # Prepare column annotations if groups are available
        col_annotation <- NULL
        if (!is.null(groups) && nrow(groups) > 0) {
          # Create annotation data frame
          sample_order <- colnames(data_matrix)
          annotation_df <- data.frame(
            sample = sample_order,
            stringsAsFactors = FALSE
          )
          
          # Add group information
          annotation_df$group <- groups$group[match(annotation_df$sample, groups$sample)]
          annotation_df$group[is.na(annotation_df$group)] <- "Ungrouped"
          
          # Create column annotation for ComplexHeatmap
          if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
            # Generate distinct colors for groups
            unique_groups <- unique(annotation_df$group)
            group_colors <- RColorBrewer::brewer.pal(min(length(unique_groups), 11), "Set3")
            names(group_colors) <- unique_groups
            
            col_annotation <- ComplexHeatmap::HeatmapAnnotation(
              Group = annotation_df$group,
              col = list(Group = group_colors)
            )
          }
        }
        
        # Generate heatmap based on available packages
        if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
          # Create color function
          if (input$color_palette %in% c("viridis", "plasma", "inferno", "magma")) {
            if (requireNamespace("viridis", quietly = TRUE)) {
              colors <- switch(input$color_palette,
                viridis = viridis::viridis(100),
                plasma = viridis::plasma(100),
                inferno = viridis::inferno(100),
                magma = viridis::magma(100)
              )
            } else {
              colors <- RColorBrewer::brewer.pal(11, "RdYlBu")
            }
          } else {
            colors <- RColorBrewer::brewer.pal(11, input$color_palette)
          }
          
          col_fun <- circlize::colorRamp2(
            seq(min(data_matrix, na.rm = TRUE), max(data_matrix, na.rm = TRUE), length = length(colors)),
            colors
          )
          
          # Generate ComplexHeatmap
          p <- ComplexHeatmap::Heatmap(
            data_matrix,
            col = col_fun,
            cluster_rows = input$cluster_rows,
            cluster_columns = input$cluster_cols,
            show_row_names = input$show_row_names,
            show_column_names = input$show_col_names,
            clustering_method_rows = input$clustering_method,
            clustering_method_columns = input$clustering_method,
            heatmap_legend_param = list(title = "Expression"),
            top_annotation = col_annotation
          )
          
        } else if (requireNamespace("pheatmap", quietly = TRUE)) {
          # Create color palette for pheatmap
          if (input$color_palette %in% c("viridis", "plasma", "inferno", "magma")) {
            if (requireNamespace("viridis", quietly = TRUE)) {
              colors <- switch(input$color_palette,
                viridis = viridis::viridis(100),
                plasma = viridis::plasma(100),
                inferno = viridis::inferno(100),
                magma = viridis::magma(100)
              )
            } else {
              colors <- RColorBrewer::brewer.pal(11, "RdYlBu")
            }
          } else {
            colors <- RColorBrewer::brewer.pal(11, input$color_palette)
          }
          
          # Prepare annotation for pheatmap
          annotation_col <- NULL
          annotation_colors <- NULL
          if (!is.null(groups) && nrow(groups) > 0) {
            sample_order <- colnames(data_matrix)
            annotation_df <- data.frame(
              Group = groups$group[match(sample_order, groups$sample)],
              row.names = sample_order,
              stringsAsFactors = FALSE
            )
            annotation_df$Group[is.na(annotation_df$Group)] <- "Ungrouped"
            annotation_col <- annotation_df
            
            # Generate colors for groups
            unique_groups <- unique(annotation_df$Group)
            group_colors <- RColorBrewer::brewer.pal(min(length(unique_groups), 11), "Set3")
            names(group_colors) <- unique_groups
            annotation_colors <- list(Group = group_colors)
          }
          
          # Generate pheatmap
          p <- pheatmap::pheatmap(
            data_matrix,
            cluster_rows = input$cluster_rows,
            cluster_cols = input$cluster_cols,
            show_rownames = input$show_row_names,
            show_colnames = input$show_col_names,
            clustering_method = input$clustering_method,
            color = colors,
            annotation_col = annotation_col,
            annotation_colors = annotation_colors,
            silent = TRUE
          )
          
        } else {
          # Fallback to basic heatmap
          colors <- RColorBrewer::brewer.pal(11, "RdYlBu")
          p <- heatmap(data_matrix, 
                      Rowv = if(input$cluster_rows) NULL else NA,
                      Colv = if(input$cluster_cols) NULL else NA,
                      col = colors,
                      labRow = if(input$show_row_names) NULL else rep("", nrow(data_matrix)),
                      labCol = if(input$show_col_names) NULL else rep("", ncol(data_matrix)))
        }
        
        return(p)
        
      }, error = function(e) {
        showNotification(paste("Error generating heatmap:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  output$ga_output <- renderPlot({
    # Check if grouping is applied
    if (!is.null(sample_groups()) && input$ga_filter_button > 0) {
      heatmap_plot_with_groups()
    } else {
      # Use basic heatmap without grouping
      req(heatmap_data())
      
      data <- heatmap_data()
      
      tryCatch({
        # Convert to matrix format (genes in rows, samples in columns)
        data_matrix <- as.matrix(data[, -1])  # Remove sample column
        rownames(data_matrix) <- data$sample
        data_matrix <- t(data_matrix)  # Transpose so genes are rows
        
        # Remove NA rows and columns
        data_matrix <- data_matrix[rowSums(is.na(data_matrix)) < ncol(data_matrix), ]
        data_matrix <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
        
        if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0) {
          return(NULL)
        }
        
        # Generate basic heatmap
        if (requireNamespace("pheatmap", quietly = TRUE)) {
          colors <- RColorBrewer::brewer.pal(11, input$color_palette)
          pheatmap::pheatmap(
            data_matrix,
            cluster_rows = input$cluster_rows,
            cluster_cols = input$cluster_cols,
            show_rownames = input$show_row_names,
            show_colnames = input$show_col_names,
            clustering_method = input$clustering_method,
            color = colors,
            silent = TRUE
          )
        } else {
          NULL
        }
      }, error = function(e) {
        NULL
      })
    }
  })
  
  # Data table output
  output$ga_output_data <- DT::renderDataTable({
    req(heatmap_data())
    
    data <- heatmap_data()
    DT::datatable(
      data,
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
      # Get the appropriate plot
      p <- if (!is.null(sample_groups()) && input$ga_filter_button > 0) {
        heatmap_plot_with_groups()
      } else if (!is.null(heatmap_data())) {
        # Generate basic plot for download
        data <- heatmap_data()
        data_matrix <- as.matrix(data[, -1])
        rownames(data_matrix) <- data$sample
        data_matrix <- t(data_matrix)
        
        if (requireNamespace("pheatmap", quietly = TRUE)) {
          colors <- RColorBrewer::brewer.pal(11, input$color_palette)
          pheatmap::pheatmap(
            data_matrix,
            cluster_rows = input$cluster_rows,
            cluster_cols = input$cluster_cols,
            show_rownames = input$show_row_names,
            show_colnames = input$show_col_names,
            clustering_method = input$clustering_method,
            color = colors,
            silent = TRUE
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
      
      if (!is.null(p)) {
        if (input$device == "png") {
          png(file, width = input$width, height = input$height, units = "in", res = 300)
        } else {
          pdf(file, width = input$width, height = input$height)
        }
        
        # Handle different plot types
        if (inherits(p, "Heatmap")) {
          # ComplexHeatmap object
          ComplexHeatmap::draw(p)
        } else if (inherits(p, "pheatmap")) {
          # pheatmap object
          grid::grid.newpage()
          grid::grid.draw(p$gtable)
        } else {
          # Regular plot
          print(p)
        }
        
        dev.off()
      }
    }
  )
}

