ui.modules_pcawg_cross_gene_o2m <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # 初始设置
      column(
        3,
        wellPanel(
          style = "height:1100px",
          h2("S1: Preset", align = "center"),
          mol_origin_UI(ns("mol_origin2cor"), database = "pcawg"),
          h4(strong("S1.1 Choose projects")) %>%
            helper(
              type = "markdown", size = "l", fade = TRUE,
              title = "PCAWG projects",
              content = "pcawg_projects"
            ),
          pickerInput(
            ns("choose_cancers"), NULL,
            choices = pcawg_names,
            multiple = TRUE,
            selected = pcawg_names,
            options = list(`actions-box` = TRUE)
          ),
          br(),
          h4(strong("S1.2 Filter samples"), "[opt]") %>%
            helper(
              type = "markdown", size = "l", fade = TRUE,
              title = "Filter samples",
              content = "choose_samples"
            ),
          h5("Quick filter:"),
          pickerInput(
            ns("filter_by_code"), NULL,
            choices = NULL, selected = NULL,
            multiple = TRUE, options = list(`actions-box` = TRUE)
          ),
          h5("Exact filter:"),
          filter_samples_UI(ns("filter_samples2cor"), database = "pcawg"),
          br(),
          verbatimTextOutput(ns("filter_phe_id_info")),
          br()
        )
      ),
      # 下载X轴数据
      column(
        3,
        wellPanel(
          style = "height:1100px",
          h2("S2: Get data", align = "center"),
          # 调用下载模块UI
          h4(strong("S2.1 Select one gene")),
          virtualSelectInput(
            inputId = ns("gene_id"),
            label = NULL,
            choices = NULL,
            search = TRUE
          ),
          br(),
          h4(strong("S2.2 Load mRNA/Mutation/Fusion/APOBEC data")),
          shinyWidgets::actionBttn(
            ns("step2_2_load"), "Load",
            style = "gradient",
            icon = icon("box"),
            color = "primary",
            block = TRUE,
            size = "sm"
          ),
          br(),
          verbatimTextOutput(ns("step2_2_text")),
          br(),
          h4(strong("S2.3 Load promoter data")),
          checkboxInput(ns("add_mean_promoter"), "Add median promoter activity", value = TRUE),
          virtualSelectInput(
            inputId = ns("promoter_id"),
            label = "Select specific promoters:",
            choices = NULL,
            multiple = TRUE,
            search = TRUE
          ),
          selectInput(ns("promoter_type"), "Promoter type:", choices = c("relative", "raw", "outlier")),
          shinyWidgets::actionBttn(
            ns("step2_3_load"), "Load",
            style = "gradient",
            icon = icon("box"),
            color = "primary",
            block = TRUE,
            size = "sm"
          ),
          br(),
          verbatimTextOutput(ns("step2_3_text")),
          br()
        )
      ),
      # 分析/绘图/下载
      column(
        6,
        wellPanel(
          h2("S3: Analyze & Visualize", align = "center") %>%
            helper(
              type = "markdown", size = "l", fade = TRUE,
              title = "Analyze & Visualize",
              content = "pcawg_cross_gene"
            ),
          style = "height:1100px",
          shinyWidgets::actionBttn(
            ns("step3_plot"), "Run (Visualize)",
            style = "gradient",
            icon = icon("chart-line"),
            color = "primary",
            block = TRUE,
            size = "sm"
          ),
          br(),
          fluidRow(
            column(12,
              offset = 0,
              plotOutput(
                {
                  ns("funky_plot")
                },
                height = "700px"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              3,
              div(shinyjs::hidden(downloadButton(ns("save_plot_bt"), "Figure")), style = "display: inline-block;vertical-align:top;"),
              div(
                style = "display: inline-block;vertical-align:top",
                dropMenu(
                  actionBttn(ns("plot_opt"),
                    label = NULL, style = "material-circle",
                    color = "success", icon = icon("gear")
                  ),
                  div(h3("1. Height:"), style = "width:400px;"),
                  numericInput(ns("save_plot_H"), NULL, min = 1, max = 20, value = NULL, step = 0.1),
                  div(h3("2. Width:"), style = "width:400px;"),
                  numericInput(ns("save_plot_W"), NULL, min = 1, max = 20, value = NULL, step = 0.1),
                  div(h3("3. Format:"), style = "width:400px;"),
                  radioGroupButtons(
                    inputId = ns("save_plot_F"),
                    label = NULL,
                    status = "primary",
                    choices = c("PDF", "PNG"),
                    justified = TRUE
                  ),
                  placement = "top"
                )
              )
            ),
            column(3, offset = 0, shinyjs::hidden(downloadButton(ns("save_data_res"), "Statistical data(.csv)")))
          )
        )
      )
    )
  )
}

server.modules_pcawg_cross_gene_o2m <- function(input, output, session) {
  ns <- session$ns

  # 记录选择癌症
  cancer_choose <- reactiveValues(
    name = "BLCA-US", phe_primary = "",
    filter_phe_id = query_tcga_group(database = "pcawg", cancer = "BLCA-US", return_all = T)
  )
  observe({
    cancer_choose$name <- input$choose_cancers
    cancer_choose$phe_primary <- query_tcga_group(database = "pcawg", cancer = cancer_choose$name, return_all = T)
  })

  # 数据源设置
  opt_pancan <- callModule(mol_origin_Server, "mol_origin2cor", database = "pcawg")

  custom_meta_sig <- reactive(NULL)
  sig_dat <- reactive(NULL)

  ## 过滤样本
  # exact filter module
  filter_phe_id <- callModule(filter_samples_Server, "filter_samples2cor",
    database = "pcawg",
    cancers = reactive(cancer_choose$name),
    custom_metadata = reactive(custom_meta_sig()),
    opt_pancan = reactive(opt_pancan())
  )
  # quick filter widget
  observe({
    code_types_valid <- unique(cancer_choose$phe_primary$Type)
    updatePickerInput(
      session,
      "filter_by_code",
      choices = code_types_valid,
      selected = code_types_valid
    )
  })

  # 综合上述二者
  observe({
    # quick filter
    filter_phe_id2 <- cancer_choose$phe_primary %>%
      dplyr::filter(Type %in% input$filter_by_code) %>%
      dplyr::pull("Sample")

    # exact filter
    if (is.null(filter_phe_id())) {
      cancer_choose$filter_phe_id <- filter_phe_id2
    } else {
      cancer_choose$filter_phe_id <- intersect(filter_phe_id2, filter_phe_id())
    }

    output$filter_phe_id_info <- renderPrint({
      cat(paste0("Tip: ", length(cancer_choose$filter_phe_id), " samples are retained"))
    })
  })


  check_omics <- reactiveValues(mRNA = TRUE, mutation = TRUE, fusion = TRUE, APOBEC = TRUE, promoter = TRUE)

  updateVirtualSelect(
    "gene_id",
    choices = pcawg_id_option[["Molecular profile"]][["mRNA Expression"]]$all,
    selected = pcawg_id_option[["Molecular profile"]][["mRNA Expression"]]$default
  )

  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
  }
  observeEvent(input$step2_2_load, {
    check_omics$mRNA <- TRUE
    check_omics$mutation <- TRUE
    check_omics$fusion <- TRUE
    check_omics$APOBEC <- TRUE

    id <- notify(h3("[1/4] Caching mRNA data..."))
    on.exit(removeNotification(id), add = TRUE)

    dat_tmp <- query_pancan_value(input$gene_id, "mRNA", database = "pcawg")
    if (is.null(dat_tmp$data) || all(is.na(dat_tmp$data))) {
      check_omics$mRNA <- FALSE
    }
    Sys.sleep(0.5)

    notify(h3("[2/4] Caching mutation data..."), id = id)
    dat_tmp <- query_pancan_value(input$gene_id, "mutation", database = "pcawg")
    if (is.null(dat_tmp$sampleID) || all(is.na(dat_tmp$sampleID))) {
      check_omics$mutation <- FALSE
    }
    Sys.sleep(0.5)

    notify(h3("[3/4] Caching fusion data..."), id = id)
    dat_tmp <- query_pancan_value(input$gene_id, "fusion", database = "pcawg")
    if (is.null(dat_tmp$data) || all(is.na(dat_tmp$data))) {
      check_omics$fusion <- FALSE
    }
    Sys.sleep(0.5)

    notify(h3("[4/4] Caching APOBEC data..."), id = id)
    dat_tmp <- query_pancan_value("APOBECtCa_enrich", "APOBEC", database = "pcawg")
    if (is.null(dat_tmp$data) || all(is.na(dat_tmp$data))) {
      check_omics$APOBEC <- FALSE
    }
    Sys.sleep(0.5)

    output$step2_2_text <- renderPrint({
      cat(paste0(
        "Tips: \n(1) mRNA is ",
        ifelse(check_omics$mRNA, "OK; ", "missing; "),
        "\n(2) Mutation is ",
        ifelse(check_omics$mutation, "OK; ", "missing; "),
        "\n(3) Fusion is ",
        ifelse(check_omics$fusion, "OK; ", "missing; "),
        "\n(4) APOBEC is ",
        ifelse(check_omics$APOBEC, "OK.", "missing.")
      ))
    })
  })

  candi_promoter <- reactive({
  	load_data("v2_tpc_id_help")$pcawg$id_pro %>%
  		dplyr::filter(.data$gene == input$gene_id) %>%
  		dplyr::pull(.data$Level3) %>% sort()
  })
  observe({
  	updateVirtualSelect(
  		"promoter_id",
  		choices = candi_promoter(),
  		selected = candi_promoter()[1:2]
  	)
  })

  observeEvent(input$step2_3_load, {
  	check_omics$promoter <- TRUE
  	id <- notify(h3("Caching promoter data..."))
  	on.exit(removeNotification(id), add = TRUE)
  	
  	if (length(input$promoter_id) == 0 && !input$add_mean_promoter) {
  		check_omics$promoter <- FALSE
  	} else {
  		# Trigger cache for sampled or overall
  		# Just to ensure check_omics is updated
  		if (input$add_mean_promoter) {
  			dat_tmp <- get_pcawg_promoter_value(candi_promoter(), type = input$promoter_type)
  		}
  		if (length(input$promoter_id) > 0) {
  			dat_tmp <- get_pcawg_promoter_value(input$promoter_id, type = input$promoter_type)
  		}
  	}

  	output$step2_3_text <- renderPrint({
  		cat(paste0(
  			"Tips: \n(1) Promoter is ",
  			ifelse(check_omics$promoter, "OK.", "missing.")
  		))
  	})
  })

  plot_func <- eventReactive(input$step3_plot, {
    shiny::validate(
      need(
        any(check_omics$mRNA, check_omics$mutation, check_omics$fusion, check_omics$APOBEC, check_omics$promoter),
        "Please load valid data in Step2.2 or Step2.3 (At least one omics must be available)"
      ),
    )
    shinyjs::disable("step3_plot")
    res <- vis_pcawg_gene_cross_omics(input$gene_id,
      tumor_projects = cancer_choose$name,
      n_promoter = input$promoter_id,
      add_mean_promoter = input$add_mean_promoter,
      promoter_type = input$promoter_type,
      return_list = TRUE
    )
    shinyjs::enable("step3_plot")
    res
  })

  output$funky_plot <- renderPlot({
    plot_func()$plot
  })

  # three download buttons
  observeEvent(input$step3_plot, {
    shinyjs::show("save_plot_bt")
    shinyjs::show("save_data_res")
    updateNumericInput(session, "save_plot_H", value = plot_func()$plot$height)
    updateNumericInput(session, "save_plot_W", value = plot_func()$plot$width)
  })

  output$save_plot_bt <- downloadHandler(
    filename = function() {
      paste0("Plot", "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".", tolower(input$save_plot_F))
    },
    content = function(file) {
      p <- plot_func()$plot
      if (input$save_plot_F == "PDF") {
        pdf(file, width = input$save_plot_W, height = input$save_plot_H, onefile = FALSE)
        print(p)
        dev.off()
      } else if (input$save_plot_F == "PNG") {
        png(file, width = input$save_plot_W, height = input$save_plot_H, res = 600, units = "in")
        print(p)
        dev.off()
      }
    }
  )
  output$save_data_res <- downloadHandler(
    filename = function() {
      paste0("Statdata_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      p_stat <- plot_func()$data
      p_stat <- apply(p_stat, 2, as.character) # list column
      write.csv(p_stat, file, row.names = FALSE)
    }
  )
}
