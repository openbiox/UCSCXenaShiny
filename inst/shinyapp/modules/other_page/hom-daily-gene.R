ui.home_daily_gene <- function(id) {
  ns <- NS(id)
  tagList(
    # h3("Daily Gene", icon = "dice", align = "center"),
    fluidRow(
      column(6, #offset = 1,
        uiOutput(ns("gene_tip")),

        fluidRow(
          column(12, align = "center",
            actionBttn(
               inputId = ns("change_gene"),
               label = "Sample another one", 
               style = "bordered",
               color = "primary",
               icon = icon("dice"),
               size = "sm"
            )
          )
        )
      ),
      column(6, #offset = 2,
        uiOutput(ns("gene_cancer"))
      )
    ),
    # fluidRow(
    #   column(8, offset = 2,
    #     uiOutput(ns("gene_cancer"))
    #   )
    # ),
  )
}





server.home_daily_gene <- function(input, output, session) {
  ns <- session$ns


  gene_sp = reactiveValues(gene="TP53")


  gene_sp = reactive({
    tcga_gene_all = tcga_id_referrence$id_molecule$id_gene$Level3
    tcga_gene = pancan_identifiers$gene[pancan_identifiers$gene %in% tcga_gene_all]

    if(input$change_gene==0){
      current_date <- Sys.Date()
      formatted_date <- as.integer(format(current_date, "%Y%m%d"))
      set.seed(formatted_date)
      gene_sp = sample(tcga_gene,size = 1)
    } else {
      current_time <- Sys.time()
      total_seconds <- as.integer(difftime(current_time, as.POSIXct("1970-01-01"), units = "secs"))
      set.seed(total_seconds)
      gene_sp = sample(tcga_gene,size = 1)
    } 
    gene_sp
  })

  gene_data = reactive({
    data.list = get_pancan_gene_value(gene_sp(), norm = "tpm")
    data = data.list$expression
    shiny::validate(
      need(try(!all(is.na(data))), 
        "Sorry, no valid value for the sampling gene. Please click the button again."),
    )   
    data = data.frame(value=data) %>% 
      tibble::rownames_to_column("sample") %>% 
      dplyr::inner_join(tcga_gtex) %>% 
      dplyr::mutate(tissue = as.character(tissue))
    data
  })



  stat_res = reactive({
    data = gene_data()
    stat_res = compare_means(value ~ type2, data = data,  
                             group.by = "tissue") %>% 
      dplyr::arrange(p)
    stat_res
  })


  output$gene_cancer = renderUI({
    output$box_plot = renderPlotly({
      data = gene_data()
      p = data %>% 
        dplyr::filter(tissue == stat_res()$tissue[1]) %>% 
        dplyr::filter(value > -9.966) %>% 
        ggboxplot(x = "type2", y = "value", 
                  fill = "type2", 
                  color = "black",
                  width = 0.5,
                  palette = "aaas") + 
        xlab(paste0("TCGA-",stat_res()$tissue[1])) + 
        ylab("TPM value (TCGA+GTEx)") +
        ggtitle(gene_sp(),
                subtitle = paste0("Wilcoxon, p=", 
                                  formatC(stat_res()$p[1], format = "e", digits = 2))) +
        theme(legend.position = "none",
              text = element_text(size = 12),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(size = 12, hjust = 0.5))
        subtitle_txt = paste0("Wilcoxon, p=", formatC(stat_res()$p[1], format = "e", digits = 2))
        title_text = 
      p = ggplotly(p) %>%
        layout(title = list(text = paste0(gene_sp(),'<br>','<sup>', subtitle_txt,'</sup>'),
                            font = list(size = 18)),
               xaxis = list(font=list(size = 15)),
               yaxis = list(font=list(size = 10)))
      p
    })
    plotlyOutput(ns("box_plot"), height = "220px")
  })


  output$gene_tip = renderUI({

    tcga_link = "https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/tcga-study-abbreviations"
    gc_link = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", gene_sp())
    pm_link = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=%28",gene_sp(),"%29+AND+%28cancer%29")
    tagList(
      tags$ul(
        tags$li(gene_sp()," is most differentially expressed in ",
                a(paste0("TCGA-",stat_res()$tissue[1]), href = tcga_link), ".",
                style = "font-size: 16px;"),
        tags$li("Explore its pan-cancer feature in right panel. ☞",style = "font-size: 16px;"),
        tags$li("Search more about the gene in ",
                a("GeneCards", href = gc_link), " or ",
                a("PubMed", href = pm_link), ".",
                style = "font-size: 16px;")
      )
    )
  })
}


