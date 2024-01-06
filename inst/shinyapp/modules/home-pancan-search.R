ui.home_search_box <- function(id) {
  ns <- NS(id)

  # ref: https://shiny.rstudio.com/articles/selectize.html
  # https://stackoverflow.com/questions/51343552/dynamic-selectizeinput-in-shiny
  fluidRow(
    column(
      3,
      selectInput(
        inputId = ns("Pancan_search_type"),
        label = NULL,
        choices = c("mRNA", "transcript", "protein", "mutation",
                    "cnv",  "methylation", "miRNA")
      )

    ),
    column(
      5,
      selectizeInput(
        inputId = ns("Pancan_search"),
        label = NULL,
        choices = NULL,
        width = "100%",
        options = list(
          create = TRUE,
          maxOptions = 5,
          # placeholder = "Enter a gene symbol, e.g. TP53",
          plugins = list("restore_on_backspace")
        )
      )
    ),
    column(
      4,
      actionBttn(
        inputId = ns("search"),
        label = "Go!",
        color = "primary",
        style = "bordered",
        size = "sm"
      )
    )
  )
}

server.home_search_box <- function(input, output, session) {
  ns <- session$ns

  observe({
    mol_choices = switch(input$Pancan_search_type,
      "mRNA" = pancan_identifiers$gene,
      "transcript" = tcga_id_option$`Molecular profile`$`Transcript Expression`$all,
      "protein" = tcga_id_option$`Molecular profile`$`Protein Expression`$all,
      "mutation" = tcga_id_option$`Molecular profile`$`Mutation status`$all,
      "cnv" = tcga_id_option$`Molecular profile`$`Copy Number Variation`$all,
      "methylation" = tcga_id_option$`Molecular profile`$`DNA Methylation`$all,
      "miRNA" = tcga_id_option$`Molecular profile`$`miRNA Expression`$all
    )
    mol_selected = switch(input$Pancan_search_type,
      "mRNA" = "TP53",
      "transcript" = tcga_id_option$`Molecular profile`$`Transcript Expression`$default,
      "protein" = tcga_id_option$`Molecular profile`$`Protein Expression`$default,
      "mutation" = tcga_id_option$`Molecular profile`$`Mutation status`$default,
      "cnv" = tcga_id_option$`Molecular profile`$`Copy Number Variation`$default,
      "methylation" = tcga_id_option$`Molecular profile`$`DNA Methylation`$default,
      "miRNA" = tcga_id_option$`Molecular profile`$`miRNA Expression`$default
    )
    updateSelectizeInput(
      session,
      "Pancan_search",
      choices = mol_choices,
      selected = mol_selected,
      server = TRUE
    )
  })

  observeEvent(input$search, {
    message(input$Pancan_search, " is queried by user from home search box.")
    if (nchar(input$Pancan_search) >= 1) {
      showModal(
        modalDialog(
          title = paste("Pancan distribution of gene", input$Pancan_search),
          size = "l",
          fluidPage(
            fluidRow(
              materialSwitch(ns("pdist_mode"), "Show violin plot", inline = TRUE),
              materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
              materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE)
            ),
            column(
              12,
              plotOutput(ns("gene_pancan_dist"))
            ),

            br(),
            
            column(
              12,
              h4("NOTEs:"),
              h5("1. The data query may take some time based on your network. Wait until a plot shows"),
              h5("2. You have to turn on both 'Show P value' and 'Show P label' to show significant labels"),
              h5("3. If a void plot shows, please check your input"),
              h5("4. You can get more features for this plot in page 'Quick PanCan Analysis'"),
              h5("")
            ),
            br(),br(),

            fluidRow(
              column(
                3, offset =1,
                actionButton(ns("generate_report"), "Run more analysis!")
              ),
              column(
                8,
                uiOutput(ns("button.ui")),
              )
            ),
            verbatimTextOutput(ns("tip1")),
            column(
              12,
              h4("NOTEs:"),
              h5("1. More analysis such as molecule related (1) chinical phenotypes, (2) survival analysis, (3) tumor index,",
                 "(4) immune infiltration, (5) pathway activity are supported through the button"),
              h5("2. After clicking the analyze button, wait for about a minute until two download buttons appear on the right, indicating completion."),
              h5("3. You can render an analysis report in html format or download the original result in zip fromat.")

            ),
          )
        )
      )
      output$gene_pancan_dist <- renderPlot({
        p <- vis_toil_TvsN(
          Gene = input$Pancan_search,
          data_type = input$Pancan_search_type,
          Mode = ifelse(input$pdist_mode, "Violinplot", "Boxplot"),
          Show.P.value = input$pdist_show_p_value,
          Show.P.label = input$pdist_show_p_label
        )

        p + cowplot::theme_cowplot() + ggpubr::rotate_x_text(45)
      })
    }
  })


  w <- waiter::Waiter$new(id = ns("tip1"), html = waiter::spin_hexdots(), color = "grey95")
  observeEvent(input$generate_report, {
    w$show()
    output$tip1 = renderPrint({
      cat("Your analysis has been done!")
    })
  })


  observeEvent(input$generate_report, {

    time_stamp = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    res_dat = mol_quick_analysis(molecule = input$Pancan_search, 
                       data_type = input$Pancan_search_type, 
                       out_dir = tempdir(), out_report = FALSE)
    output$button.ui = renderUI({
      fluidRow(
        column(
          5, 
          downloadButton(ns("report"), "Generate report")
        ),
        column(
          5, 
          downloadButton(ns("rawdata"), "Get rawdata")
        )
      )
    })

    output$report = downloadHandler(
      filename = paste0(time_stamp,"_report.html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), paste0(time_stamp,"_report.Rmd"))
        file.copy(system.file("rmd","report_template.Rmd", package = "UCSCXenaShiny"), 
                  tempReport, overwrite = TRUE)
        params = list("sur_res" = res_dat$sur_res,
                      "cor_res" = res_dat$cor_res,
                      "phe_res" = res_dat$phe_res,
                      "id_name" = input$Pancan_search,
                      "id_type" = input$Pancan_search_type)
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv()))
      }
    )


    output$rawdata = downloadHandler(
      filename = paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_rawdata.zip"),
      content = function(file) {


        fs1 = file.path(tempdir(), paste0(time_stamp,"_molecule_clinical_result.csv"))
        fs2 = file.path(tempdir(), paste0(time_stamp,"_molecule_correlation_result.csv"))
        fs3 = file.path(tempdir(), paste0(time_stamp,"_molecule_survival_result.csv"))

        write.csv(res_dat$phe_res, row.names = FALSE, file=fs1)
        write.csv(res_dat$cor_res, row.names = FALSE, file=fs2)
        write.csv(res_dat$sur_res, row.names = FALSE, file=fs3)

        fs = c(fs1, fs2, fs3)

        zip(zipfile=file, files=fs, mode = "cherry-pick")
      },
      contentType = "application/zip"
    )

  })



}
