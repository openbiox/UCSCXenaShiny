ui.modules_ga_matrix_correlation <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Analysis Controls"),
          uiOutput(ns("ga_data1_id")),
          selectizeInput(
            inputId = ns("ga_data1_mid"), # molecule identifier
            label = "Molecule identifiers:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              maxOptions = 5,
              placeholder = "e.g. TP53, PTEN, KRAS",
              plugins = list("restore_on_backspace")
            )
          ),
          selectInput(ns("ga_matrix_type"), "Matrix Type",
            choices = c("full", "upper", "lower"), selected = "full", multiple = FALSE
          ),
          selectInput(ns("ga_test_type"), "Test Type",
            choices = c("parametric", "nonparametric", "robust", "bayes"),
            selected = "parametric", multiple = FALSE
          ),
          selectInput(ns("ga_test_adjust"), "Adjust Method",
            choices = c(
              "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
              "none"
            ),
            selected = "holm", multiple = FALSE
          ),
          sliderInput(ns("ga_sig_level"), "Signif Level", 0, 1, 0.05, step = 0.01),
          materialSwitch(
            inputId = ns("ga_use_partial"),
            label = "Partial correlation?",
            value = FALSE,
            status = "primary"
          ),
          colourpicker::colourInput(inputId = ns("ga_lower_col"), "Color for negative", "#E69F00"),
          colourpicker::colourInput(inputId = ns("ga_higher_col"), "Color for positive", "#009E73"),
          actionBttn(
            inputId = ns("ga_go"),
            label = "Submit",
            style = "gradient",
            icon = icon("check"),
            color = "default",
            block = TRUE,
            size = "sm"
          )
        )
      ),
      column(
        6,
        plotOutput(ns("ga_output")),
        DT::dataTableOutput(ns("ga_output_data"))
      ),
      column(
        3,
        wellPanel(
          h4("Sample Filters"),
          uiOutput(ns("ga_data_filter1_id")),
          actionBttn(
            inputId = ns("ga_filter_button"),
            label = "Click to filter!",
            color = "primary",
            style = "bordered",
            size = "sm"
          ),
          tags$br(),
          tags$br(),
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


server.modules_ga_matrix_correlation <- function(input, output, session,
                                                 selected_database_rm_phenotype, selected_database_add_url_and_phenotype) {
  ns <- session$ns

  output$ga_data1_id <- renderUI({
    show_table <- selected_database_rm_phenotype()
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Select dataset:",
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
      selected = c("TP53", "KRAS", "PTEN"),
      server = TRUE
    )
  })

  selected_samps <- reactiveValues(id = NULL)
  p_scatter <- eventReactive(input$ga_go, {
    if (is.null(selected_samps$id)) {
      message("All samples selected for analysis.")
    } else {
      message(length(selected_samps$id), " samples selected for analysis.")
    }
    tryCatch(
      vis_identifier_multi_cor(
        isolate(input$ga_data1_id),
        isolate(input$ga_data1_mid),
        matrix.type = isolate(input$ga_matrix_type),
        type = isolate(input$ga_test_type),
        partial = isolate(input$ga_use_partial),
        sig.level = isolate(input$ga_sig_level),
        p.adjust.method = isolate(input$ga_test_adjust),
        color_low = isolate(input$ga_lower_col),
        color_high = isolate(input$ga_higher_col),
        samples = isolate(selected_samps$id)
      ),
      error = function(e) {
        message("General analysis plot error:")
        print(e$message)
        "Error"
      }
    )
  })
  output$download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_cormat_heatmap.", input$device)
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file, plot = p_scatter(), device = input$device,
        units = "cm", width = input$width, height = input$height, dpi = 600
      )
    }
  )

  observeEvent(input$ga_go, {
    # Analyze correlation with 2 input datasets and identifiers
    output$ga_output <- renderPlot(
      if (inherits(p_scatter(), c("ggplot", "grob"))) {
        print(p_scatter())
      } else {
        sendSweetAlert(
          session,
          title = "Error",
          text = "Error to query data and plot. Please make sure the two selected datasets are 'genomicMatrix' type.",
          type = "error"
        )
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
            dom = "Bfrtip",
            buttons = list(
              list(
                extend = "csv", text = "Download Current Page", filename = "page",
                exportOptions = list(
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = "csv", text = "Download Full Results", filename = "data",
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
      inputId = ns("ga_data_filter1_id"),
      label = "Select phenotype dataset:",
      choices = c("NONE", unique(show_table$XenaDatasets[show_table$XenaDatasets %in% phenotype_datasets])),
      selected = "NONE",
      multiple = FALSE
    )
  })

  observeEvent(input$ga_data_filter1_id, {
    # If phenotype dataset has been reset, we use all samples for plotting
    if (length(input$ga_data_filter1_id)) {
      if (input$ga_data_filter1_id == "NONE") {
        selected_samps$id <- NULL
      }
    }
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
            wellPanel(
              h4("1. Select available columns on the left to right"),
              uiOutput(ns("ga_col_chooser")),
              tags$br(),
              actionBttn(
                inputId = ns("show_or_update_ptable"),
                label = "Show/Update Phenotype Table",
                color = "primary",
                style = "bordered",
                size = "sm",
                block = F
              )
            ),
            wellPanel(
              h4("2. Filter rows by SearchPanels"),
              DT::dataTableOutput(ns("ga_phenotype_data"))
            ),
            wellPanel(
              h4("3. Specify sample column and hit button"),
              uiOutput(ns("ga_select_samp_col")),
              actionBttn(
                inputId = ns("ga_filter_submit_button"),
                label = "Submit to filter",
                color = "primary",
                style = "bordered", size = "sm",
                block = F
              )
            ),
            wellPanel(
              h4("4. After hitting button, dismiss this page and click submit button")
            )
          )
        )
      )

      phenotype_table <- XenaGenerate(subset = XenaDatasets == pdataset) %>%
        XenaQuery() %>%
        XenaDownload(destdir = XENA_DEST) %>%
        XenaPrepare()

      output$ga_col_chooser <- renderUI({
        all_cols <- colnames(phenotype_table)
        sel_idx <- seq_len(min(length(all_cols), 5))
        chooserInput(ns("ga_col_chooser"), "Available columns", "Selected columns",
          if (length(all_cols) == length(sel_idx)) c() else all_cols[-sel_idx],
          all_cols[sel_idx],
          size = 5, multiple = TRUE
        )
      })

      observeEvent(input$show_or_update_ptable, {
        selected_cols <- isolate(input$ga_col_chooser$right)
        if (length(selected_cols)) {
          message("Following columns selected by users from sample filter window.")
          print(selected_cols)

          output$ga_phenotype_data <- DT::renderDataTable(server = FALSE, {
            DT::datatable(
              phenotype_table %>%
                dplyr::select(all_of(selected_cols)),
              rownames = FALSE,
              extensions = c("Buttons", "Select", "SearchPanes"), # "Scroller" causes bug in searchPanel
              options = list(
                dom = "Bfrtip", # P
                buttons = list(
                  "searchPanes",
                  list(
                    extend = "csv", text = "Download Current Page", filename = "page",
                    exportOptions = list(
                      modifier = list(page = "current")
                    )
                  ),
                  list(
                    extend = "csv", text = "Download Full Results", filename = "data",
                    exportOptions = list(
                      modifier = list(page = "all")
                    )
                  )
                ),
                scrollY = 350,
                scrollX = 300,
                deferRender = TRUE,
                # scroller = TRUE,
                stateSave = FALSE
              ),
              selection = "none"
            )
          })
        } else {
          sendSweetAlert(session, title = "Warning", type = "warn", text = "Please select at least 1 column!")
        }
      })

      output$ga_select_samp_col <- renderUI({
        selectInput(
          inputId = ns("ga_select_samp_col"),
          label = "Select sample column:",
          choices = c("NONE", input$ga_col_chooser$right),
          selected = "NONE",
          multiple = FALSE
        )
      })
      observeEvent(input$ga_filter_submit_button, {
        rows_filtered <- input$ga_phenotype_data_rows_all
        col_sample <- input$ga_select_samp_col
        keep_samples <- phenotype_table[rows_filtered, , drop = FALSE][[col_sample]]
        message(length(keep_samples), " samples left after filtering.") # 为什么该代码块会被多次调用呢？
        if (length(keep_samples) < 1) {
          sendSweetAlert(session, title = "Error", text = "No samples left!", type = "error")
        } else {
          selected_samps$id <- isolate(keep_samples)
        }
      })
    } else {
      sendSweetAlert(session, title = "Warning", type = "warn", text = "Please select a dataset!")
    }
  })
}
