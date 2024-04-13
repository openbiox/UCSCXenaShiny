ui.modules_ga_surv_analysis <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Analysis Controls"),
          uiOutput(ns("ga_data1_id")),
          virtualSelectInput(
            inputId = ns("ga_data1_mid"), # molecule identifier
            label = "Dataset 1 molecule identifier:",
            choices = NULL,
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyBS::bsPopover(ns("ga_data1_mid"),
            title = "Note",
            content = "Ignore this option when you select a phenotype dataset",
            placement = "right", options = list(container = "body")
          ),
          uiOutput(ns("ga_data2_id")),
          selectInput(ns("color_palette"), "Color palette:",
            choices = c("npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons", "rickandmorty"),
            selected = "aaas"
          ),
          actionBttn(
            inputId = ns("ga_preprocess_data"),
            label = "Preprocess",
            style = "gradient",
            icon = icon("check"),
            color = "default",
            block = TRUE,
            size = "sm"
          ),
          tags$br(),
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
          numericInput(inputId = ns("height"), label = "Height", value = 25),
          numericInput(inputId = ns("width"), label = "Width", value = 20),
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


server.modules_ga_surv_analysis <- function(
  input, output, session,
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file) {
  ns <- session$ns

  output$ga_data1_id <- renderUI({
    show_table <- selected_database_add_url_and_phenotype()
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Select dataset for grouping:",
      choices = c("NONE", unique(show_table$XenaDatasets)),
      selected = "NONE",
      multiple = FALSE
    )
  })

  id1_choices <- eventReactive(input$ga_data1_id, {
    if (!identical(input$ga_data1_id, "NONE")) {
      if (input$ga_data1_id %in% phenotype_datasets || input$ga_data1_id == "custom_phenotype_dataset") {
        # A phenotype data is selected
        return(list(
          all = "NONE",
          selected = "NONE"
        ))
      } else {
        # !!Assume a dense matrix dataset is selected
        return(list(
          all = if (is.null(custom_file$fData)) all_preload_identifiers else
            unique(c(custom_file$fData[[1]], all_preload_identifiers)),
          selected = "TP53"
        ))
      }
    } else {
      return(list(
        all = "NONE",
        selected = "NONE"
      ))
    }
  })

  observe({
    updateVirtualSelect(
      "ga_data1_mid",
      choices = id1_choices()$all,
      selected = id1_choices()$selected
    )
  })

  output$ga_data2_id <- renderUI({
    show_table <- selected_database_add_url_and_phenotype()
    selectInput(
      inputId = ns("ga_data2_id"),
      label = "Select survival dataset:",
      choices = c("NONE", unique(show_table$XenaDatasets[show_table$XenaDatasets %in% c(phenotype_datasets, "custom_phenotype_dataset")])),
      selected = "NONE",
      multiple = FALSE
    )
  })

  # Preprocessing
  group_col_status <- reactiveValues(status = "Off") # Off, CO (连续值), CA（离散值）

  observeEvent(input$ga_preprocess_data, {
    message("Preprocess button is clicked by user.")

    # dataset1 is the value data
    dataset1 <- setdiff(input$ga_data1_id, "NONE")
    dataset1_id <- setdiff(input$ga_data1_mid, "NONE")
    dataset1_phenotype <- if (dataset1 %in% phenotype_datasets) "YES" else "NO"

    if (dataset1_phenotype == "YES") {
      data1 <- XenaGenerate(subset = XenaDatasets == dataset1) %>%
        XenaQuery() %>%
        XenaDownload(destdir = XENA_DEST) %>%
        XenaPrepare() %>%
        as.data.frame()
    } else {
      if (length(dataset1_id) || dataset1 == "custom_phenotype_dataset") {
        data1 <- get_data_df(dataset1, dataset1_id)
      } else {
        sendSweetAlert(session,
          title = "Warning",
          text = "Please input a ID (e.g., gene symbol) for axis X when you select a non-phenotype dataset"
        )
      }
    }
    print(head(data1))

    # dataset2 is the survival data
    dataset2 <- setdiff(input$ga_data2_id, "NONE")
    dataset2_id <- setdiff(input$ga_data2_mid, "NONE")

    if (dataset2 == "custom_phenotype_dataset") {
      data2 <- readRDS(file.path(tempdir(), "custom_phenotype_data.rds"))
    } else {
      data2 <- XenaGenerate(subset = XenaDatasets == dataset2) %>%
        XenaQuery() %>%
        XenaDownload(destdir = XENA_DEST) %>%
        XenaPrepare() %>%
        as.data.frame()
    }
    print(head(data2))

    if (length(dataset1) && length(dataset2)) {
      showModal(
        modalDialog(
          title = "Preprocessing",
          size = "l",
          fluidPage(
            wellPanel(
              h3("1. Process dataset 1 to determine the survival groups"),
              DT::dataTableOutput(ns("data1_table")),
              fluidRow(
                column(4, selectInput(ns("data1_sample_col"), "Sample column", choices = colnames(data1), selected = colnames(data1)[1])),
                shinyBS::bsPopover(ns("data1_sample_col"),
                  title = "Note",
                  content = "The sample IDs in this column must match with the second table",
                  placement = "top", options = list(container = "body")
                ),
                column(4, selectInput(ns("data1_group_col"), "Group column", choices = colnames(data1), selected = colnames(data1)[2]))
              ),
              shinyjs::hidden(
                prettyRadioButtons(
                  inputId = ns("group_col_cutoff_mode"),
                  label = "Cutoff mode",
                  icon = icon("check"),
                  choices = c("Auto", "Custom"),
                  selected = "Auto",
                  animation = "jelly",
                  status = "default",
                  inline = TRUE
                ),
                sliderInput(
                  inputId = ns("group_col_cutpoint"), label = "Select (min and max) percent cutoff (%) to generate (Low and High) groups:",
                  min = 10, max = 90, value = c(50, 50)
                )
              ),
              shinyjs::hidden(
                uiOutput(ns("group_col_groups"))
              )
            ),
            wellPanel(
              h3("2. Process dataset 2 to determine the time and status data"),
              DT::dataTableOutput(ns("data2_table")),
              fluidRow(
                column(3, selectInput(ns("data2_sample_col"), "Sample column", choices = colnames(data2), selected = colnames(data2)[1])),
                shinyBS::bsPopover(ns("data2_sample_col"),
                  title = "Note",
                  content = "The sample IDs in this column must match with the first table",
                  placement = "top", options = list(container = "body")
                ),
                column(3, selectInput(ns("data2_time_col"), "Time column", choices = c("NONE", colnames(data2)), selected = "NONE")),
                column(3, selectInput(ns("data2_status_col"), "Status column", choices = c("NONE", colnames(data2)), selected = "NONE")),
                column(3, selectInput(
                  inputId = ns("status_label"),
                  label = "Event label",
                  choices = NULL
                ))
              )
            ),
            wellPanel(
              h3("3. Join selected columns in the two tables"),
              actionBttn(
                inputId = ns("join_table_button"),
                label = "Click to join the data",
                color = "primary",
                style = "bordered", size = "sm",
                block = F
              ),
              h4("If the table below is right, dismiss this page and click submit button to plot"),
              markdown("- The first column refers to sample ID.
                       - The second column refers to groups (numeric type is accepted, it will be classified when plotting).
                       - The third column refers to survival time.
                       - The fourth column refers survival status."),
              DT::dataTableOutput(ns("joined_table"))
            )
          )
        )
      )

      observe({
        group_col <- setdiff(input$data1_group_col, "NONE")
        if (length(group_col) && group_col %in% colnames(data1)) {
          # A column has been selected by user
          data <- na.omit(data1[[group_col]])
          if (length(data)) {
            if (is.numeric(data) && length(unique(data)) > 5) {
              shinyjs::hide(id = "group_col_groups")
              shinyjs::show(id = "group_col_cutoff_mode")
              if (input$group_col_cutoff_mode == "Custom") {
                shinyjs::show(id = "group_col_cutpoint")
              } else {
                shinyjs::hide(id = "group_col_cutpoint")
              }
              group_col_status$status <- "CO"
            } else {
              choices <- unique(data)
              group_col_status$status <- "CA"
              output$group_col_groups <- renderUI({
                prettyCheckboxGroup(
                  inputId = ns("group_col_groups"),
                  label = "Select groups:",
                  choices = choices,
                  selected = choices,
                  animation = "jelly",
                  status = "info"
                )
              })
              shinyjs::hide(id = "group_col_cutoff_mode")
              shinyjs::hide(id = "group_col_cutpoint")
              shinyjs::show(id = "group_col_groups")
            }
            message("Observe:")
            print(group_col_status$status)
          } else {
            sendSweetAlert(session,
              title = "Warning",
              text = "The group column you selected seems have no valid data!",
              type = "warning"
            )
          }
        } else {
          shinyjs::hide(id = "group_col_cutoff_mode")
          shinyjs::hide(id = "group_col_cutpoint")
          shinyjs::hide(id = "group_col_groups")
          group_col_status$status <- "Off"
        }
      })

      output$data1_table <- DT::renderDataTable(server = TRUE, {
        DT::datatable(
          data1,
          rownames = FALSE,
          options = list(
            scrollY = 350,
            scrollX = 300,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'font-size': '90%'});",
              "$(this.api().table().body()).css({'font-size': '85%'});",
              "}"
            )
          )
        )
      })

      output$data2_table <- DT::renderDataTable(server = TRUE, {
        DT::datatable(
          data2,
          rownames = FALSE,
          options = list(
            scrollY = 350,
            scrollX = 300
          )
        )
      })

      event_choices <- reactive({
        data2_status_col <- setdiff(input$data2_status_col, "NONE")
        if (length(data2_status_col)) {
          names(table(data2[[data2_status_col]]))
        } else {
          NULL
        }
      })

      observe({
        if (length(event_choices())) {
          updateSelectInput(
            session,
            "status_label",
            choices = event_choices(),
            selected = if (length(event_choices) > 1) event_choices()[2] else event_choices()[2]
          )
        }
      })

      observeEvent(input$join_table_button, {
        data1_sample_col <- isolate(input$data1_sample_col)
        data2_sample_col <- isolate(input$data2_sample_col)
        data1_group_col <- isolate(input$data1_group_col)
        data2_time_col <- setdiff(isolate(input$data2_time_col), "NONE")
        data2_status_col <- setdiff(isolate(input$data2_status_col), "NONE")
        status_label <- isolate(input$status_label)

        if (length(data1_group_col) > 0 && length(data2_time_col) > 0 && length(data2_status_col) > 0 && length(status_label) > 0) {
          data1_cols <- c(data1_sample_col, data1_group_col)
          data2_cols <- c(data2_sample_col, data2_time_col, data2_status_col)
          col_order <- c("sample", data1_group_col, data2_time_col, data2_status_col)
          message("column order:")
          print(col_order)

          if (all(data1_cols %in% colnames(data1)) && all(data2_cols %in% colnames(data2))) {
            data1_copy <- dplyr::select(data1, dplyr::all_of(data1_cols))
            colnames(data1_copy)[1] <- "sample"
            message("data1 to join:")
            print(head(data1_copy))
            message("data1 columns:")
            print(colnames(data1_copy))

            data2_copy <- dplyr::select(data2, dplyr::all_of(data2_cols))
            colnames(data2_copy)[1] <- "sample"
            message("data2 to join:")
            print(head(data2_copy))
            message("data2 columns:")
            print(colnames(data2_copy))

            message("Joining:")
            joined_data <- tryCatch(
              {
                dplyr::inner_join(
                  data1_copy, data2_copy,
                  by = "sample"
                ) %>%
                  dplyr::select(dplyr::all_of(col_order)) %>%
                  as.data.frame() %>%
                  setNames(c("sample", "group", "time", "status"))
              },
              error = function(e) {
                message("Joining failed for group comparison analysis.")
                NULL
              }
            )
            print(head(joined_data))

            print(group_col_status$status)
            if (group_col_status$status == "CA") {
              # 作为离散值处理
              message(length(input$group_col_groups), " groups remained.")
              joined_data <- joined_data[joined_data[[2]] %in% isolate(input$group_col_groups), ]
            }

            # Make sure the status is 0 or 1
            message("Handling survival status...")
            joined_data$status <- as.character(joined_data$status)
            joined_data$status <- ifelse(joined_data$status == status_label, 1L, 0L)

            grp_df$data <- joined_data

            if (!is.null(joined_data)) {
              message("Displaying joining table")
              output$joined_table <- DT::renderDataTable(server = TRUE, {
                DT::datatable(
                  joined_data,
                  rownames = FALSE,
                  options = list(
                    scrollY = 350,
                    scrollX = 300
                  )
                )
              })
            } else {
              sendSweetAlert(session,
                title = "Warning",
                text = "Joining failed! Please check your selection, especially the sample ID column.", type = "warning"
              )
            }
          }
        } else {
          print(str(data2_time_col))
          print(str(data2_status_col))
          sendSweetAlert(session,
            title = "Warning",
            text = "Some options (e.g., time, status) have not been selected!", type = "warning"
          )
        }
      })
    } else {
      sendSweetAlert(session,
        title = "Warning",
        text = "Two datasets must be selected.",
        type = "warning"
      )
    }
  })

  selected_samps <- reactiveValues(id = NULL)
  grp_df <- reactiveValues(data = NULL)

  p_surv <- eventReactive(input$ga_go, {
    if (is.null(selected_samps$id)) {
      message("All samples selected for analysis.")
    } else {
      message(length(selected_samps$id), " samples selected for analysis.")
    }
    tryCatch(
      vis_identifier_grp_surv(
        surv_df = grp_df$data,
        samples = selected_samps$id,
        cutoff_mode = if (group_col_status$status == "CA") {
          "None"
        } else {
          input$group_col_cutoff_mode
        },
        cutpoint = input$group_col_cutpoint,
        palette = input$color_palette
      ),
      error = function(e) {
        message("General analysis survival plot error:")
        print(e$message)
        "Error"
      }
    )
  })

  observeEvent(input$ga_go, {
    # Analyze correlation with 2 input datasets and identifiers
    output$ga_output <- renderPlot(
      if (inherits(p_surv(), "ggsurvplot")) {
        print(p_surv())
      } else {
        sendSweetAlert(
          session,
          title = "Error",
          text = "Error to query data and plot. Please make sure you have go through the 'preprocess' step.",
          type = "error"
        )
      }
    )
    output$download <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_surv_plot.", input$device)
      },
      content = function(file) {
        if(input$device == "png"){
          png(filename = file, units = "cm", width = input$width, height = input$height, res = 600)
          print(p_surv())
          dev.off()
        }
        if(input$device == "pdf"){
          pdf(file = file, onefile = FALSE)
          print(p_surv())
          dev.off()
        }
        # ggplot2::ggsave(
        #   filename = file, plot = print(p_surv(), newpage = F), device = input$device,
        #   units = "cm", width = input$width, height = input$height, dpi = 600
        # )
      }
    )

    output$ga_output_data <- DT::renderDataTable(server = FALSE, {
      if (inherits(p_surv(), "ggsurvplot")) {
        DT::datatable(
          attr(p_surv(), "data"),
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
      choices = c("NONE", unique(show_table$XenaDatasets[show_table$XenaDatasets %in% c(phenotype_datasets, "custom_phenotype_dataset")])),
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
