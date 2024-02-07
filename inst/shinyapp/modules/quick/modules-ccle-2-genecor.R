choices_primary_site <- c(
  "prostate", "stomach",
  "urinary_tract", "central_nervous_system",
  "ovary", "haematopoietic_and_lymphoid_tissue",
  "kidney", "thyroid",
  "skin", "soft_tissue",
  "salivary_gland", "lung",
  "bone", "pleura",
  "endometrium", "pancreas",
  "breast", "upper_aerodigestive_tract",
  "large_intestine", "autonomic_ganglia",
  "oesophagus", "liver",
  "biliary_tract", "small_intestine"
)


ui.modules_ccle_genecor <- function(id) {
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
            mol_origin_UI(ns("mol_origin2quick"), database = "ccle")
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("profile1"), label = "Select a genomic profile:",
            choiceValues = c("mRNA", "protein", "cnv"),
            choiceNames = c("mRNA Expression", "Protein Expression", "Copy Number Variation"),
            animation = "jelly"
          ),
          virtualSelectInput(
            inputId = ns("ccle_search1"),
            label = NULL,
            choices = NULL,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("profile2"), label = "Select a genomic profile:",
            choiceValues = c("mRNA", "protein", "cnv"),
            choiceNames = c("mRNA Expression", "Protein Expression", "Copy Number Variation"),
            animation = "jelly"
          ),
          virtualSelectInput(
            inputId = ns("ccle_search2"),
            label = NULL,
            choices = NULL,
            width = "100%",
            search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
          ),
          shinyBS::bsPopover(ns("ccle_search"),
            title = "Tips",
            content = "Enter a gene symbol to show its distribution, e.g. TP53",
            placement = "right", options = list(container = "body")
          ),
        )),
      column(
        3,
        wellPanel(
          h4("2. Parameters", align = "center"),   
          selectInput(
            inputId = ns("cor_method"),
            label = "Select Correlation method",
            choices = c("spearman", "pearson"),
            selected = "spearman"
          ),
          selectInput(inputId = ns("use_all"), label = "Use All Primary Sites", choices = c("TRUE", "FALSE"), selected = "FALSE"),
          selectInput(
            inputId = ns("SitePrimary"), label = "Filter Primary Site",
            choices = choices_primary_site, selected = "prostate", multiple = TRUE
          ),
          materialSwitch(ns("use_log_x"), "x axis log", inline = FALSE),
          materialSwitch(ns("use_log_y"), "y axis log", inline = FALSE),
          materialSwitch(ns("use_regline"), "Use regression line", inline = TRUE),
          sliderTextInput(
            inputId = ns("alpha"),
            label = "Choose a transparent value",
            choices = seq(
              from = 0,
              to = 1,
              by = 0.1
            ),
            selected = "0.5",
            grid = TRUE
          ),
          colourpicker::colourInput(inputId = ns("color"), "Point color", "#000000"),
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
          numericInput(inputId = ns("height"), label = "Height", value = 8),
          numericInput(inputId = ns("width"), label = "Width", value = 8),
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
          ),
          hr(),
          tags$a(href = "https://xenabrowser.net/datapages/?cohort=Cancer%20Cell%20Line%20Encyclopedia%20(CCLE)&removeHub=https%3A%2F%2Ficgc.xenahubs.net", "Genomic profile data source")
        )
      ),
      column(
        plotOutput(ns("gene_ccle_gene_cor"), height = "600px"),
        DT::DTOutput(outputId = ns("tbl")),
        shinyjs::hidden(
          wellPanel(
            id = ns("save_csv"),
            downloadButton(ns("downloadTable"), "Save as csv")
          )
        ),
        width = 6
      )
    )
  )
}

server.modules_ccle_genecor <- function(input, output, session) {
  ns <- session$ns

  profile_choices1 <- reactive({
    switch(input$profile1,
      mRNA = list(all = pancan_identifiers$gene, default = "CSF1R"),
      protein = list(all = pancan_identifiers$protein, default = "P53"),
      cnv = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
  })

  observe({
    updateVirtualSelect(
      "ccle_search1",
      choices = profile_choices1()$all,
      selected = profile_choices1()$default
    )
  })

  profile_choices2 <- reactive({
    switch(input$profile2,
      mRNA = list(all = pancan_identifiers$gene, default = "JAK3"),
      protein = list(all = UCSCXenaShiny:::.all_ccle_proteins, default = "p53_Caution"),
      cnv = list(all = pancan_identifiers$gene, default = "TP53"),
      list(all = "NONE", default = "NONE")
    )
  })

  opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database = "ccle")

  observe({
    updateVirtualSelect(
      "ccle_search2",
      choices = profile_choices2()$all,
      selected = profile_choices2()$default
    )
  })

  plot_func <- eventReactive(input$search_bttn, {
    if (nchar(input$ccle_search1) >= 1 & nchar(input$ccle_search2) >= 1) {
      p <- vis_ccle_gene_cor(
        Gene1 = input$ccle_search1,
        Gene2 = input$ccle_search2,
        data_type1 = input$profile1,
        data_type2 = input$profile2,
        cor_method = input$cor_method,
        use_log_x = input$use_log_x,
        use_log_y = input$use_log_y,
        use_regline = input$use_regline,
        color = input$color,
        alpha = input$alpha,
        SitePrimary = input$SitePrimary,
        use_all = as.logical(input$use_all),
        opt_pancan = opt_pancan()
      )
    }
    p <- p + theme_classic(base_size = 20) +
      ggplot2::theme(legend.position = "none")

    return(p)
  })

  # Show waiter for plot
  w <- waiter::Waiter$new(id = ns("gene_ccle_gene_cor"), html = waiter::spin_hexdots(), color = "white")

  output$gene_ccle_gene_cor <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search1, "_", input$profile1, "_", input$ccle_search2, "_", input$profile2, "_gene_ccle_genecor.", input$device)
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
    if (nchar(input$ccle_search1) >= 1 & nchar(input$ccle_search2) >= 1) {
      shinyjs::show(id = "save_csv")
    } else {
      shinyjs::hide(id = "save_csv")
    }
  })


  output$tbl <- renderDT(
    plot_func()$data %>%
      dplyr::rename('Cell_line'='sample',
        'Molecule1'='gene1', 'Molecule2'='gene2') %>%
      dplyr::select(Cell_line,Site_Primary,Molecule1,Molecule2),
    options = list(lengthChange = FALSE)
  )

  ## downloadTable
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$ccle_search1, "_", input$profile1, "_", input$ccle_search2, "_", input$profile2, "_gene_ccle_genecor.csv")
    },
    content = function(file) {
      data = plot_func()$data %>%
        dplyr::rename('Cell_line'='sample',
          'Molecule1'='gene1', 'Molecule2'='gene2') %>%
        dplyr::select(Cell_line,Site_Primary,Molecule1,Molecule2)
      write.csv(data, file, row.names = FALSE)
    }
  )
}
