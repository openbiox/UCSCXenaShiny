# cBioPortal Correlation Analysis Module

# UI
ui.modules_cbioportal_correlation <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("cBioPortal: Molecular Correlation Analysis", align = "center"),
    br(),
    
    fluidRow(
      column(
        3,
        wellPanel(
          # Study selection module
          ui.modules_cbioportal_study_selector(ns("study_selector")),
          
          hr(),
          
          # Gene selection
          h4(strong("Step 3: Select Genes"), style = "color: #2c3e50;"),
          
          fluidRow(
            column(
              12,
              textInput(
                inputId = ns("gene_x"),
                label = "Gene X:",
                value = "TP53",
                placeholder = "Enter gene symbol"
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              textInput(
                inputId = ns("gene_y"),
                label = "Gene Y:",
                value = "MDM2",
                placeholder = "Enter gene symbol"
              )
            )
          ),
          
          br(),
          actionButton(
            inputId = ns("run_correlation"),
            label = "Run Correlation Analysis",
            icon = icon("play"),
            class = "btn-warning",
            style = "width: 100%;"
          )
        )
      ),
      
      column(
        9,
        tabsetPanel(
          tabPanel(
            "Correlation Plot",
            br(),
            plotOutput(ns("correlation_plot"), height = "600px")
          ),
          tabPanel(
            "Results Table",
            br(),
            DT::dataTableOutput(ns("correlation_table"))
          ),
          tabPanel(
            "Study Info",
            br(),
            verbatimTextOutput(ns("study_summary"))
          )
        )
      )
    )
  )
}

# Server
modules_cbioportal_correlation_Server <- function(input, output, session) {
  ns <- session$ns
  
  # Get study data from the selector module
  study_data <- callModule(modules_cbioportal_study_selector_Server, "study_selector")
  
  # Reactive values for analysis
  values <- reactiveValues(
    correlation_data = NULL,
    correlation_result = NULL
  )
  
  # Run correlation analysis
  observeEvent(input$run_correlation, {
    req(study_data()$molecular_data, input$gene_x, input$gene_y)
    
    withProgress(message = "Running correlation analysis...", {
      tryCatch({
        molecular_data <- study_data()$molecular_data
        
        # Filter data for selected genes
        gene_x_data <- molecular_data[molecular_data$id == input$gene_x, ]
        gene_y_data <- molecular_data[molecular_data$id == input$gene_y, ]
        
        if (nrow(gene_x_data) == 0) {
          showNotification(paste("Gene", input$gene_x, "not found in the data"), type = "warning")
          return()
        }
        
        if (nrow(gene_y_data) == 0) {
          showNotification(paste("Gene", input$gene_y, "not found in the data"), type = "warning")
          return()
        }
        
        # Merge data for correlation
        correlation_data <- merge(
          gene_x_data[, c("Sample", "value")],
          gene_y_data[, c("Sample", "value")],
          by = "Sample",
          suffixes = c("_x", "_y")
        )
        
        # Remove NA values
        correlation_data <- correlation_data[complete.cases(correlation_data), ]
        
        if (nrow(correlation_data) < 3) {
          showNotification("Insufficient data points for correlation analysis", type = "warning")
          return()
        }
        
        # Calculate correlation
        cor_result <- cor.test(correlation_data$value_x, correlation_data$value_y)
        
        values$correlation_data <- correlation_data
        values$correlation_result <- cor_result
        
        showNotification(
          paste("Correlation analysis completed. R =", round(cor_result$estimate, 3)), 
          type = "success"
        )
        
      }, error = function(e) {
        showNotification(paste("Error in correlation analysis:", e$message), type = "error")
      })
    })
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    req(values$correlation_data, values$correlation_result)
    
    cor_data <- values$correlation_data
    cor_result <- values$correlation_result
    
    p <- ggplot(cor_data, aes(x = value_x, y = value_y)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", color = "red", se = TRUE) +
      labs(
        title = paste("Correlation:", input$gene_x, "vs", input$gene_y),
        subtitle = paste(
          "Study:", study_data()$study_id, 
          "| R =", round(cor_result$estimate, 3),
          "| p =", format.pval(cor_result$p.value, digits = 3)
        ),
        x = paste(input$gene_x, "Expression"),
        y = paste(input$gene_y, "Expression")
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    print(p)
  })
  
  # Results table
  output$correlation_table <- DT::renderDataTable({
    req(values$correlation_data)
    
    cor_data <- values$correlation_data
    colnames(cor_data) <- c("Sample", paste(input$gene_x, "Expression"), paste(input$gene_y, "Expression"))
    
    DT::datatable(
      cor_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    ) %>%
      DT::formatRound(columns = 2:3, digits = 3)
  })
  
  # Study summary
  output$study_summary <- renderText({
    req(study_data()$study_id)
    
    lines <- c()
    lines <- c(lines, paste("Study ID:", study_data()$study_id))
    
    if (!is.null(study_data()$data_type)) {
      lines <- c(lines, paste("Data Type:", study_data()$data_type))
    }
    
    if (!is.null(study_data()$clinical_data)) {
      lines <- c(lines, paste("Clinical Samples:", nrow(study_data()$clinical_data)))
    }
    
    if (!is.null(study_data()$molecular_data)) {
      unique_genes <- length(unique(study_data()$molecular_data$id))
      unique_samples <- length(unique(study_data()$molecular_data$Sample))
      lines <- c(lines, paste("Molecular Data: ", unique_genes, "genes x", unique_samples, "samples"))
    }
    
    if (!is.null(values$correlation_result)) {
      lines <- c(lines, "")
      lines <- c(lines, "Correlation Analysis Results:")
      lines <- c(lines, paste("  Correlation coefficient:", round(values$correlation_result$estimate, 4)))
      lines <- c(lines, paste("  p-value:", format.pval(values$correlation_result$p.value)))
      lines <- c(lines, paste("  95% CI: [", 
                              round(values$correlation_result$conf.int[1], 4), ", ",
                              round(values$correlation_result$conf.int[2], 4), "]"))
      lines <- c(lines, paste("  Sample size:", nrow(values$correlation_data)))
    }
    
    paste(lines, collapse = "\n")
  })
}