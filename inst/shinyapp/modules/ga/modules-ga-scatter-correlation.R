ui.modules_ga_scatter_correlation <- function(id) {
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
            label = "Dataset 1 molecule identifier:",
            choices = NULL,
            options = list(
              create = TRUE,
              maxOptions = 5,
              placeholder = "e.g. TP53",
              plugins = list("restore_on_backspace")
            )
          ),
          uiOutput(ns("ga_data2_id")),
          selectizeInput(
            inputId = ns("ga_data2_mid"), # molecule identifier
            label = "Dataset 2 molecule identifier:",
            choices = NULL,
            options = list(
              create = TRUE,
              maxOptions = 5,
              placeholder = "e.g. TP53",
              plugins = list("restore_on_backspace")
            )
          ),
          materialSwitch(
            inputId = ns("ga_use_ggstats"),
            label = "Use ggstatsplot?",
            value = FALSE,
            status = "primary"
          ),
          tags$p("NOTE: The data table is not available when use ggstatsplot."),
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


server.modules_ga_scatter_correlation <- function(
  input, output, session,
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file) {
  ns <- session$ns

  output$ga_data1_id <- renderUI({
    show_table <- selected_database_rm_phenotype()
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Select dataset for axis X:",
      choices = c("NONE", unique(show_table$XenaDatasets)),
      selected = "NONE",
      multiple = FALSE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "ga_data1_mid",
      choices = if (is.null(custom_file$fData)) all_preload_identifiers else
        unique(c(custom_file$fData[[1]], all_preload_identifiers)),
      selected = "TP53",
      server = TRUE
    )
  })

  output$ga_data2_id <- renderUI({
    show_table <- selected_database_rm_phenotype()
    selectInput(
      inputId = ns("ga_data2_id"),
      label = "Select dataset for axis Y:",
      choices = c("NONE", unique(show_table$XenaDatasets)),
      selected = "NONE",
      multiple = FALSE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "ga_data2_mid",
      choices = if (is.null(custom_file$fData)) all_preload_identifiers else
        unique(c(custom_file$fData[[1]], all_preload_identifiers)),
      selected = "KRAS",
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
      vis_identifier_cor(
        isolate(input$ga_data1_id),
        isolate(input$ga_data1_mid),
        isolate(input$ga_data2_id),
        isolate(input$ga_data2_mid),
        samples = isolate(selected_samps$id),
        use_ggstats = isolate(input$ga_use_ggstats)
      ),
      error = function(e) {
        message("General analysis plot error:")
        print(e$message)
        e$message
      }
    )
  })

  observeEvent(input$ga_go, {
    # Analyze correlation with 2 input datasets and identifiers
    output$ga_output <- renderPlot(
      if (inherits(p_scatter(), c("ggplot", "grob"))) {
        print(p_scatter())
      } else {
        sendSweetAlert(
          session,
          title = "Error",
          text = tags$span(
            tags$p(paste0("Error: ", p_scatter())),
            tags$p("Error to query data and plot. Please make sure the two selected datasets are 'genomicMatrix' type."),
            tags$p("'genomicMatrix' type means the dataset is stored in feature-by-sample format, e.g., gene-by-sample expression matrix."),
            tags$p("The type of datasets can be found at the dataset table by clicking 'Pre-selected Datasets for Analysis' on the 'General Analysis' Page."),
            tags$img(src = "https://gitee.com/ShixiangWang/ImageCollection/raw/master/png/20210708184045.png",
                     alt = "errorImg", width = "98%", height = "98%")
          ),
          type = "error"
        )
      }
    )
    output$download <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_corplot.", input$device)
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file, plot = p_scatter(), device = input$device,
          units = "cm", width = input$width, height = input$height, dpi = 600
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
