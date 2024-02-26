ui.modules_pcawg_dist <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        3,
        wellPanel(
          h4("1. Data", align = "center"),
          div(actionButton(ns("toggleBtn"), "Modify datasets[opt]",icon = icon("folder-open")),
              style = "margin-bottom: 5px;"),
          conditionalPanel(
            ns = ns,
            condition = "input.toggleBtn % 2 == 1",
            mol_origin_UI(ns("mol_origin2quick"), database = "pcawg")
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("profile"), label = "Select a genomic profile:",
            choiceValues = c(
              "mRNA", "miRNA",
              "promoter", "fusion", "APOBEC"
            ),
            choiceNames = c(
              "mRNA Expression", "miRNA Expression",
              "Promoter Activity", "Gene Fusion",
              "APOBEC mutagenesis"
            ),
            animation = "jelly"
          ),
          virtualSelectInput(
            inputId = ns("Pancan_search"),
            label = "Input a gene or formula (as signature)",
            choices = NULL,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyBS::bsPopover(ns("Pancan_search"),
            title = "Tips",
            content = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
            placement = "right", options = list(container = "body")
          ),
        ),
      ),
      column(
        3,
        wellPanel(
          h4("2. Parameters", align = "center"),
          materialSwitch(ns("pdist_mode"), "Show violin plot", inline = FALSE),
          materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
          materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE),
          colourpicker::colourInput(inputId = ns("tumor_col"), "Tumor sample color", "#DF2020"),
          colourpicker::colourInput(inputId = ns("normal_col"), "Normal sample color", "#DDDF21"),
          selectInput(inputId = ns("theme"), label = "Select theme for plot", choices = names(themes_list), selected = "cowplot"),
          tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
          shinyWidgets::actionBttn(
            inputId = ns("search_bttn"),
            label = "Go!",
            style = "gradient",
            icon = icon("search"),
            color = "primary",
            block = TRUE,
            size = "sm"
          )
        ),
        wellPanel(
          h4("3. Download", align = "center"),
          numericInput(inputId = ns("height"), label = "Height", value = 5),
          numericInput(inputId = ns("width"), label = "Width", value = 12),
          prettyRadioButtons(
            inputId = ns("device"),
            label = "Choose plot format",
            choices = c("pdf", "png"),
            selected = "pdf",
            inline = TRUE,
            icon = icon("check"),
            animation = "jelly",
            fill = TRUE
          ),
          tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
          downloadBttn(
            outputId = ns("download"),
            style = "gradient",
            color = "primary",
            block = TRUE,
            size = "sm"
          )
        ),
      ),
      column(
        6,
        plotOutput(ns("gene_pancan_dist"), height = "500px"),
        hr(),
        h5("NOTEs:"),
        p("1. The data query may take some time based on your network. Wait until a plot shows"),
        p("2. You have to turn on both 'Show P value' and 'Show P label' to show significant labels"),
        p("3. If a void plot shows, please check your input"),
        p("4. ", tags$a(href = "https://xenabrowser.net/datapages/?cohort=PCAWG%20(specimen%20centric)&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443&removeHub=https%3A%2F%2Fatacseq.xenahubs.net", "Genomic profile data source")),
        tags$br(),
        DT::DTOutput(outputId = ns("tbl")),
        shinyjs::hidden(
          wellPanel(
            id = ns("save_csv"),
            downloadButton(ns("downloadTable"), "Save as csv")
          )
        )
      )
    )
  )
}

server.modules_pcawg_dist <- function(input, output, session) {
  ns <- session$ns

  profile_choices <- reactive({
    switch(input$profile,
      mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
      miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
      promoter = list(all = names(load_data("pcawg_promoter_id")), default = "1:169863093:SCYL3"),
      fusion = list(all = pancan_identifiers$gene, default = "DPM1"),
      APOBEC = list(all = c(
        "tCa_MutLoad_MinEstimate", "APOBECtCa_enrich",
        "A3A_or_A3B", "APOBEC_tCa_enrich_quartile", "APOBECrtCa_enrich",
        "APOBECytCa_enrich", "APOBECytCa_enrich-APOBECrtCa_enrich",
        "BH_Fisher_p-value_tCa", "ntca+tgan", "rtCa_to_G+rtCa_to_T",
        "rtca+tgay", "tCa_to_G+tCa_to_T",
        "ytCa_rtCa_BH_Fisher_p-value", "ytCa_rtCa_Fisher_p-value", "ytCa_to_G+ytCa_to_T",
        "ytca+tgar"
      ), default = "APOBECtCa_enrich"),
      list(all = "NONE", default = "NONE")
    )
  })

  observe({
    updateVirtualSelect(
      "Pancan_search",
      choices = profile_choices()$all,
      selected = profile_choices()$default
    )
  })

  colors <- reactive({
    c(input$tumor_col, input$normal_col)
  })

  plot_theme <- reactive({
    themes_list[[input$theme]]
  })

  opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database = "pcawg")

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_pancan_dist"), html = waiter::spin_hexdots(), color = "black")

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      p <- vis_pcawg_dist(
        Gene = input$Pancan_search,
        data_type = input$profile,
        Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
        Show.P.value = input$pdist_show_p_value,
        Show.P.label = input$pdist_show_p_label,
        values = colors(),
        opt_pancan = opt_pancan()
      ) + plot_theme() + ggplot2::theme(
        axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 15)
      )
    }
    return(p)
  })

  output$colorvalues <- reactive({
    c(input$tumor_col, input$normal_col)
  })

  output$gene_pancan_dist <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_pcawg_dist.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Project'='dcc_project_code','Sample'='icgc_specimen_id',
          'Group'='type2', 'Value'='tpm') %>%
        dplyr::select(Project, Sample, Group,Value)
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Pancan_search, "_", input$profile, "_pcawg_dist.", input$device)
    },
    content = function(file) {
      p <- plot_func()
      if (input$device == "pdf") {
        pdf(file, width = input$width, height = input$height)
        print(p)
        dev.off()
      } else {
        png(file, width = input$width, height = input$height, res = 600, units = "in")
        print(p)
        dev.off()
      }
    }
  )

  observeEvent(input$search_bttn, {
    if (nchar(input$Pancan_search) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })


  output$tbl <- renderDT(
    plot_func()$data %>%
      dplyr::rename('Project'='dcc_project_code','Sample'='icgc_specimen_id',
        'Group'='type2', 'Value'='tpm') %>%
      dplyr::select(Project, Sample, Group,Value),
    options = list(lengthChange = FALSE)
  )
}
