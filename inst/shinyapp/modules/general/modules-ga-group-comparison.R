ui.modules_ga_group_comparison <- function(id) {
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
            label = "Molecule identifier",
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
          virtualSelectInput(
            inputId = ns("ga_data2_mid"), # molecule identifier
            label = "Molecule identifier",
            choices = NULL,
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyBS::bsPopover(ns("ga_data2_mid"),
            title = "Note",
            content = "Ignore this option when you select a phenotype dataset",
            placement = "right", options = list(container = "body")
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
          materialSwitch(
            inputId = ns("ga_use_pairwise"),
            label = "Display pairwise comparisons?",
            value = TRUE,
            status = "primary"
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


server.modules_ga_group_comparison <- function(
  input, output, session,
  selected_database_rm_phenotype, selected_database_add_url_and_phenotype,
  custom_file) {
  ns <- session$ns

  output$ga_data1_id <- renderUI({
    show_table <- selected_database_add_url_and_phenotype()
    selectInput(
      inputId = ns("ga_data1_id"),
      label = "Select dataset for groups:",
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
      label = "Select dataset for axis Y:",
      choices = c("NONE", unique(show_table$XenaDatasets)),
      selected = "NONE",
      multiple = FALSE
    )
  })

  id2_choices <- eventReactive(input$ga_data2_id, {
    if (!identical(input$ga_data2_id, "NONE")) {
      if (input$ga_data2_id %in% phenotype_datasets || input$ga_data2_id == "custom_phenotype_dataset") {
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
      "ga_data2_mid",
      choices = id2_choices()$all,
      selected = id2_choices()$selected
    )
  })

  # Preprocessing
  group_col_status <- reactiveValues(status = "Off") # Off, CO (连续值), CA（离散值）

  observeEvent(input$ga_preprocess_data, {
    message("Preprocess button is clicked by user.")

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

    dataset2 <- setdiff(input$ga_data2_id, "NONE")
    dataset2_id <- setdiff(input$ga_data2_mid, "NONE")
    dataset2_phenotype <- if (dataset2 %in% phenotype_datasets) "YES" else "NO"
    if (dataset2_phenotype == "YES") {
      data2 <- XenaGenerate(subset = XenaDatasets == dataset2) %>%
        XenaQuery() %>%
        XenaDownload(destdir = XENA_DEST) %>%
        XenaPrepare() %>%
        as.data.frame()
    } else {
      if (length(dataset2_id) || dataset2 == "custom_phenotype_dataset") {
        data2 <- get_data_df(dataset2, dataset2_id)
      } else {
        sendSweetAlert(session,
          title = "Warning",
          text = "Please input a ID (e.g., gene symbol) for axis Y when you select a non-phenotype dataset"
        )
      }
    }
    print(head(data2))

    if (length(dataset1) && length(dataset2)) {
      showModal(
        modalDialog(
          title = "Preprocessing",
          size = "l",
          fluidPage(
            wellPanel(
              h3("1. Process dataset 1 to determine the groups indicated in axis X"),
              DT::dataTableOutput(ns("data1_table")),
              fluidRow(
                column(4, selectInput(ns("data1_sample_col"), "Sample column", choices = colnames(data1), selected = colnames(data2)[1])),
                shinyBS::bsPopover(ns("data1_sample_col"),
                  title = "Note",
                  content = "The sample IDs in this column must match with the second table",
                  placement = "top", options = list(container = "body")
                ),
                column(4, selectInput(ns("data1_group_col"), "Group column", choices = c("NONE", colnames(data1)), selected = "NONE")),
                column(4, selectInput(ns("data1_facet_col"), "Facet column (optional)", choices = c("NONE", colnames(data1)), selected = "NONE")),
                shinyBS::bsPopover(ns("data1_facet_col"),
                  title = "Note",
                  content = "Two tables can only select 1 column for generating multiple sub-plots",
                  placement = "top", options = list(container = "body")
                )
              ),
              shinyjs::hidden(
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
              h3("2. Process dataset 2 to determine the values indicated in axis Y"),
              DT::dataTableOutput(ns("data2_table")),
              fluidRow(
                column(4, selectInput(ns("data2_sample_col"), "Sample column", choices = colnames(data2), selected = colnames(data2)[1])),
                shinyBS::bsPopover(ns("data2_sample_col"),
                  title = "Note",
                  content = "The sample IDs in this column must match with the first table",
                  placement = "top", options = list(container = "body")
                ),
                column(4, selectInput(ns("data2_value_col"), "Value column", choices = c("NONE", colnames(data2)), selected = colnames(data2)[2])),
                column(4, selectInput(ns("data2_facet_col"), "Facet column (optional)", choices = c("NONE", colnames(data2)), selected = "NONE")),
                shinyBS::bsPopover(ns("data2_facet_col"),
                  title = "Note",
                  content = "Two tables can only select 1 column for generating multiple sub-plots",
                  placement = "top", options = list(container = "body")
                )
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
                       - The second column refers to values indicated in axis Y.
                       - The third column refers to groups indicated in axis X.
                       - The fourth column is optional, which indicates facet variable."),
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
              shinyjs::show(id = "group_col_cutpoint")
              group_col_status$status <- "CO"
            } else {
              choices <- unique(data)
              group_col_status$status <- "CA"
              output$group_col_groups <- renderUI({
                prettyCheckboxGroup(
                  inputId = ns("group_col_groups"),
                  label = "Select groups shown in X axis:",
                  choices = choices,
                  selected = choices,
                  animation = "jelly",
                  status = "info"
                )
              })
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

      observeEvent(input$join_table_button, {
        data1_sample_col <- input$data1_sample_col
        data2_sample_col <- input$data2_sample_col
        data1_group_col <- setdiff(input$data1_group_col, "NONE")
        data2_value_col <- setdiff(input$data2_value_col, "NONE")
        data1_facet_col <- setdiff(input$data1_facet_col, "NONE")
        data2_facet_col <- setdiff(input$data2_facet_col, "NONE")

        if (length(data1_facet_col) && length(data2_facet_col)) {
          sendSweetAlert(session,
            title = "Warning",
            text = "Two tables can only select 1 column for generating multiple sub-plots!", type = "warning"
          )
        } else if (length(data1_group_col) > 0 && length(data2_value_col) > 0) {
          data1_cols <- c(data1_sample_col, data1_group_col, data1_facet_col)
          data2_cols <- c(data2_sample_col, data2_value_col, data2_facet_col)
          col_order <- c("sample", data2_value_col, data1_group_col, data1_facet_col, data2_facet_col)
          message("column order:")
          print(col_order)

          if (all(data1_cols %in% colnames(data1)) && all(data2_cols %in% colnames(data2))) {
            data1_copy <- dplyr::select(data1, dplyr::all_of(data1_cols))
            colnames(data1_copy)[1] <- "sample"
            data2_copy <- dplyr::select(data2, dplyr::all_of(data2_cols))
            colnames(data2_copy)[1] <- "sample"

            message("Joining:")
            joined_data <- tryCatch(
              {
                dplyr::inner_join(
                  data1_copy, data2_copy,
                  by = "sample"
                ) %>%
                  dplyr::select(dplyr::all_of(col_order)) %>%
                  as.data.frame()
              },
              error = function(e) {
                message("Joining failed for group comparison analysis.")
                NULL
              }
            )

            print(group_col_status$status)
            if (group_col_status$status != "Off") {
              if (group_col_status$status == "CO") {
                # 作为连续值处理，读取分割点
                data <- joined_data
                data$.group <- joined_data[[3]]
                data <- data %>%
                  dplyr::arrange(.data$.group) %>%
                  dplyr::mutate(per_rank = 100 / nrow(.) * (1:nrow(.))) %>%
                  dplyr::mutate(.group = dplyr::case_when(
                    .data$per_rank > !!input$group_col_cutpoint[2] ~ "High",
                    .data$per_rank <= !!input$group_col_cutpoint[1] ~ "Low",
                    TRUE ~ NA_character_
                  ))
                joined_data[[3]] <- data$.group
                joined_data <- na.omit(joined_data)
              } else {
                # 作为离散值处理
                message(length(input$group_col_groups), " groups remained.")
                joined_data <- joined_data[joined_data[[3]] %in% input$group_col_groups, ]
                print(nrow(joined_data))
              }
            }

            grp_df$data <- joined_data

            if (!is.null(joined_data)) {
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
          print(str(data1_group_col))
          print(str(data2_value_col))
          sendSweetAlert(session,
            title = "Warning",
            text = "Some options have not been selected!", type = "warning"
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

  p_grp_comp <- eventReactive(input$ga_go, {
    if (is.null(selected_samps$id)) {
      message("All samples selected for analysis.")
    } else {
      message(length(selected_samps$id), " samples selected for analysis.")
    }
    tryCatch(
      vis_identifier_grp_comparison(
        grp_df = grp_df$data,
        type = isolate(input$ga_test_type),
        pairwise.comparisons = isolate(input$ga_use_pairwise),
        p.adjust.method = isolate(input$ga_test_adjust),
        samples = isolate(selected_samps$id)
      ),
      error = function(e) {
        message("General analysis group comparison plot error:")
        print(e$message)
        "Error"
      }
    )
  })

  observeEvent(input$ga_go, {
    # Analyze correlation with 2 input datasets and identifiers
    output$ga_output <- renderPlot(
      if (inherits(p_grp_comp(), c("ggplot", "grob"))) {
        print(p_grp_comp())
      } else {
        sendSweetAlert(
          session,
          title = "Error",
          text = "Error to plot. Please make sure you have go through the 'preprocess' step.",
          type = "error"
        )
      }
    )
    output$download <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_group_comparison_plot.", input$device)
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file, plot = p_grp_comp(), newpage = F, device = input$device,
          units = "cm", width = input$width, height = input$height, dpi = 600
        )
      }
    )

    output$ga_output_data <- DT::renderDataTable(server = FALSE, {
      if (inherits(p_grp_comp(), "ggplot")) {
        DT::datatable(
          p_grp_comp()$data,
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
