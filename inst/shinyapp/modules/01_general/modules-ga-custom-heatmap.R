ui.modules_ga_custom_heatmap <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Heatmap Controls"),
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
          uiOutput(ns("ga_data_phenotype_id")),
          uiOutput(ns("ga_grouping_variable")),
          conditionalPanel(
            condition = paste0("input['", ns("grouping_method"), "'] == 'custom'"),
            textAreaInput(
              inputId = ns("custom_groups"),
              label = "Custom groups (one group per line, samples separated by commas):",
              placeholder = "Group1: TCGA-AA-3502,TCGA-AA-3506\nGroup2: TCGA-AA-3510,TCGA-AA-3514",
              rows = 4
            )
          ),
          selectInput(
            inputId = ns("grouping_method"),
            label = "Grouping method:",
            choices = c("Phenotype variable" = "phenotype", "Custom definition" = "custom"),
            selected = "phenotype"
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
  
  # Generate heatmap
  heatmap_plot <- reactive({
    req(heatmap_data())
    
    withProgress(message = "Generating heatmap...", value = 0, {
      data <- heatmap_data()
      
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
        
        # Try to use ComplexHeatmap first, fall back to pheatmap
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
            heatmap_legend_param = list(title = "Expression")
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
          
          # Generate pheatmap
          p <- pheatmap::pheatmap(
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
    heatmap_plot()
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
      p <- heatmap_plot()
      if (!is.null(p)) {
        if (input$device == "png") {
          png(file, width = input$width, height = input$height, units = "in", res = 300)
        } else {
          pdf(file, width = input$width, height = input$height)
        }
        print(p)
        dev.off()
      }
    }
  )
}

