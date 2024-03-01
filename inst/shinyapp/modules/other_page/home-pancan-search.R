ui.home_search_box <- function(id) {
  ns <- NS(id)

  # ref: https://shiny.rstudio.com/articles/selectize.html
  # https://stackoverflow.com/questions/51343552/dynamic-selectizeinput-in-shiny
  tagList(
    fluidRow(
      column(10, offset = 1,
      #   5,
        selectInput(
          inputId = ns("Pancan_search_type"),
          # label = NULL,
          label = "1. Select one molecule:",
          choices = c("mRNA", "transcript", "protein", "mutation",
                      "cnv",  "methylation", "miRNA"),
          selected = "mRNA"
        )
      )
    ),
    fluidRow(
      column(10, offset = 1,
        virtualSelectInput(
          inputId = ns("Pancan_search"),
          label = NULL,
          choices = NULL,
          width = "100%",
          search = TRUE,
          allowNewOption = TRUE,
          dropboxWidth = "200%"
        )
      )
    ),
    br(),
    fluidRow(
      column(11, offset = 1,
        h5(strong("2.Run two explorations:"))
      )
    ),
    fluidRow(
      column(
        5, offset = 1,
        # align="center",
        actionBttn(
          inputId = ns("search_1"),
          label = "Tumor vs. Normal",
          color = "primary",
          style = "bordered",
          size = "sm"
        )
      ),
      column(
        5,
        # align="center",
        actionBttn(
          inputId = ns("search_2"),
          label = "Generate Report",
          color = "primary",
          style = "bordered",
          size = "sm"
        )
      )
    )
  )

}

server.home_search_box <- function(input, output, session) {
  ns <- session$ns

  observe({

    mol_choices = switch(ifelse(is.null(input$Pancan_search_type),"mRNA",input$Pancan_search_type),
      "mRNA" = pancan_identifiers$gene,
      "transcript" = tcga_id_option$`Molecular profile`$`Transcript Expression`$all,
      "protein" = tcga_id_option$`Molecular profile`$`Protein Expression`$all,
      "mutation" = tcga_id_option$`Molecular profile`$`Mutation status`$all,
      "cnv" = tcga_id_option$`Molecular profile`$`Copy Number Variation`$all,
      "methylation" = tcga_id_option$`Molecular profile`$`DNA Methylation`$all,
      "miRNA" = tcga_id_option$`Molecular profile`$`miRNA Expression`$all
    )
    mol_selected = switch(ifelse(is.null(input$Pancan_search_type),"mRNA",input$Pancan_search_type),
      "mRNA" = "TP53",
      "transcript" = tcga_id_option$`Molecular profile`$`Transcript Expression`$default,
      "protein" = tcga_id_option$`Molecular profile`$`Protein Expression`$default,
      "mutation" = tcga_id_option$`Molecular profile`$`Mutation status`$default,
      "cnv" = tcga_id_option$`Molecular profile`$`Copy Number Variation`$default,
      "methylation" = tcga_id_option$`Molecular profile`$`DNA Methylation`$default,
      "miRNA" = tcga_id_option$`Molecular profile`$`miRNA Expression`$default
    )
    updateVirtualSelect(
      "Pancan_search",
      choices = mol_choices,
      selected = mol_selected
    )
  })

  observeEvent(input$search_1, {
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
            )
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


  observeEvent(input$search_2, {
    message("Generate an analysis for report for ",input$Pancan_search)
    if (nchar(input$Pancan_search) >= 1) {
      showModal(
        modalDialog(
          title = paste("Generate an analysis report for ",
                        input$Pancan_search_type," ",input$Pancan_search),
          size = "l",
          fluidPage(
            fluidRow(
              column(
                4,
                wellPanel(
                  style = "height:180px",
                  h4("1. Run the analysis:"),
                  br(),
                  column(
                    12, align="center", 
                    actionBttn(
                      inputId = ns("report_1"),
                      label = "Go!",
                      color = "primary",
                      style = "pill",
                      size = "md",
                      icon = shiny::icon("gear")
                    )
                  ),
                  br(),
                  fluidRow(textOutput(ns("tip1"))),
                )
              ),
              column(
                4,
                wellPanel(
                  style = "height:180px",
                  h4("2. Knit the report:"),
                  br(),
                  column(
                    12, align="center", 
                    downloadBttn(
                      outputId = ns("report_2"),
                      label = "Download",
                      color = "primary",
                      style = "pill",
                      size = "md"
                    ),
                  ),
                  br(),
                  fluidRow(textOutput(ns("tip2"))),
                )
              ),
              column(
                4,
                wellPanel(
                  style = "height:180px",
                  h4("3. Obtain analyzed result:"),
                  br(),
                  column(
                    12, align="center", 
                    downloadBttn(
                      outputId = ns("report_3"),
                      label = "Download",
                      color = "primary",
                      style = "pill",
                      size = "md"
                    )
                  )
                )
              ),              
            ),
            fluidRow(
              br(),
              h3("NOTEs:"),
              h4("1. It will take about ",strong("one minute",style="color:red")," for the general analysis which includes the relationships between queried molecule and ",
                "(1) clinical phenotypes, (2) survival influence, (3) tumor index (4) immune infiltration, (5) pathway activity.",
                "You can see the ",a("example report", href = "https://lishensuo.github.io/book_test/UCSCXenaShiny_example_report.html")," while waiting."),
              h4("2. It will take about ", strong("10 seconds",style="color:red"), " for the generation of organized report in html format."),
              h4("3. Final, the analyzed result under the report can be directly downloaded in zip format.")
            )
          )
        )
      )
    }

  })



  w <- waiter::Waiter$new(id = ns("tip1"), html = waiter::spin_hexdots(), color = "grey95")
  observeEvent(input$report_1, {
    w$show()
    output$tip1 = renderText({
      paste0("Tip: You now can run step2 or step3. (",
        format(Sys.time(), "%H:%M:%S"), ")")
    })
  })


  observeEvent(input$report_1, {    
    time_stamp = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    res_dat = mol_quick_analysis(molecule = input$Pancan_search, 
                       data_type = input$Pancan_search_type, 
                       out_dir = tempdir(), out_report = FALSE)




    output$report_2 = downloadHandler(
      
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

    output$report_3 = downloadHandler(
      filename = paste0(time_stamp,"_rawdata.zip"),
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
