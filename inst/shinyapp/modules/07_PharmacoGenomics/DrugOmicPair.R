uiDrugOmicPair <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             selectInput(inputId = ns("select_omics"), 
                         "Please select the molecular type:", 
                         choices = c("Copy Number Data" = "cnv",
                                     "DNA Methylation" = "meth",
                                     "Gene Fusion" = "fusion",
                                     "Gene Mutation" = "mutation_gene",
                                     "Gene Site Mutation" = "mutation_site",
                                     "mRNA Expression" = "mRNA",
                                     "Protein Expression" = "protein"
                         ), selected = "mRNA"
             )),
      # Select omics ----
      column(4,
             selectizeInput(
               ns("select_specific_omic"), "Molecule Selection:", choices = NULL,
               options = list(
                 placeholder = 'Please select a molecular feature',
                 onInitialize = I('function() { this.setValue(""); }'), selected = "ABCC3"
               )),
             # DOPselectOmicsUI("DOPselectOmics"),
      ),
      # Select drugs ----
      column(4,
             selectizeInput(
               ns("select_specific_drug"), "Drug Selection:", choices = NULL,
               options = list(
                 placeholder = 'Please select a drug',
                 onInitialize = I('function() { this.setValue(""); }'), selected = "YM-155"
               ))
      ),
    ),
    # Plot results ----
    wellPanel(
      # textOutput("total")
      column(12, plotOutput(ns("p_search"), height="20cm")),
      h5("."),
    ),
    h4(strong("NOTEs:")),
    h5("The y axis is the drug sensitivity metric."),
    h5("The x-axis represents the molecular state or numerical data, including the boolean state for gene mutations or mRNA expression."),
    h5("Given that there are overlapping cells in different drug and omics datasets, we have utilized the common data to assess correlations, thereby maximizing the utilization of existing information. For instance, the designation 'gdsc_ctrp1' indicates that the omics data is sourced from the GDSC project, while the drug sensitivity data is derived from the CTRP1 project.")
    # Download plot ----
    # column(3,
    #        downloadLink(ns("dl_p_search"), "Save as PDF")
    # )
  )
}

serverDrugOmicPair <- function(input, output, session){
  ns <- session$ns
  # Select ----
  ## Omics ----
  omics_search_sel <- reactiveValues()
  observeEvent(input$select_omics, {
    omics_search_sel$omics <- switch(input$select_omics, 
                                     "mRNA" = omics_search[omics_search$type %in% "mRNA",]$omics,
                                     "meth" = omics_search[omics_search$type %in% "meth",]$omics,
                                     "protein" = omics_search[omics_search$type %in% "protein",]$omics,
                                     "cnv" = omics_search[omics_search$type %in% "cnv",]$omics,
                                     "mutation_gene" = omics_search[omics_search$type %in% "mutation_gene",]$omics,
                                     "mutation_site" = omics_search[omics_search$type %in% "mutation_site",]$omics,
                                     "fusion" = omics_search[omics_search$type %in% "fusion",]$omics)
    updateSelectizeInput(session = session, inputId = 'select_specific_omic',
                         label = 'Molecule Selection:', choices = omics_search_sel$omics, server = TRUE,
                         options = list(placeholder = 'Please select a molecular feature', onInitialize = I('function() { this.setValue(""); }')),
                         selected = "ABCC3"
    )
  })
  
  ## Drugs ----
  updateSelectizeInput(session = session, inputId = 'select_specific_drug',
                       label = 'Drug Selection:', choices = drugs_search$drugs, server = TRUE,
                       options = list(placeholder = 'Please select a drug', onInitialize = I('function() { this.setValue(""); }')),
                       selected = "YM-155"
  )
  # Plot ----
  # Select omics
  omics_search_list1_sel <- reactive({omics_search_list1[[input$select_omics]]}) # cor
  omics_search_list2_sel <- reactive({omics_search_list2[[input$select_omics]]}) # discrete
  output$p_search <- renderPlot({
    ## Cor plot ----
    if(input$select_omics %in% c("cnv",
                                 "protein",
                                 "meth",
                                 "mRNA")){
      p_list <- lapply(1:length(omics_search_list1_sel()), function(x){
        # TSPAN6 YM-155
        omics_sel <- omics_search_list1_sel()[[x]][[1]]
        drugs_sel <- omics_search_list1_sel()[[x]][[2]]
        sel_omics <- omics_sel[rownames(omics_sel) %in% input$select_specific_omic,] %>% as.numeric()
        sel_drugs <- drugs_sel[rownames(drugs_sel) %in% input$select_specific_drug,] %>% as.numeric()
        if(length(na.omit(sel_omics)) == 0 | length(na.omit(sel_drugs)) == 0){ return(NULL) }
        cor_df <- data.frame(
          genes = sel_omics,
          drugs = sel_drugs
        )
        p <- ggscatter(cor_df, x = "genes", y = "drugs",
                       alpha = 0.2) + 
          stat_cor(size = 6, method = "spearman") + stat_smooth(formula = y ~ x,method = "lm") + theme_bw() + 
          theme(
            axis.title = element_blank(),
            title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 12)
          ) + ggtitle(names(omics_search_list1_sel())[[x]])
      })
      # Box plot ----
    } else{
      p_list <- lapply(1:length(omics_search_list2_sel()), function(x){
        omics_sel <- omics_search_list2_sel()[[x]][[1]]
        drugs_sel <- omics_search_list2_sel()[[x]][[2]]
        sel_omics <- omics_sel$cells[omics_sel[[1]] %in% input$select_specific_omic] %>% unique()
        sel_drugs <- drugs_sel[rownames(drugs_sel) %in% input$select_specific_drug,] %>% as.numeric()
        yes_drugs <- sel_drugs[colnames(drugs_sel) %in% sel_omics] %>% na.omit()
        no_drugs <- sel_drugs[!colnames(drugs_sel) %in% sel_omics] %>% na.omit()
        if(length(yes_drugs) == 0 | length(no_drugs) == 0){ return(NULL) }
        box_df <- data.frame(
          drugs = c(no_drugs, yes_drugs),
          events = rep(c("no","yes"), times = c(length(no_drugs), length(yes_drugs))))
        p <- ggboxplot(data = box_df, x = "events", y = "drugs",
                       fill = "events", palette = c("#BEBADAFF", "#FB8072FF"),
                       add = "jitter", add.params = list(alpha = 0.2)) + 
          stat_compare_means(size = 6, label.x = 0.8) + theme_bw() + 
          theme(
            axis.title = element_blank(),
            title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 18),
            # axis.text.x = element_text(size = 17),
            legend.position = "none"
          ) + 
          coord_cartesian(ylim = c(0, max(box_df$drugs) + 
                                     max(box_df$drugs)/20)) + 
          ggtitle(names(omics_search_list2_sel())[[x]])
      })
    }
    p_list <- p_list[!sapply(p_list, is.null)]
    # Warning 
    shiny::validate(
      need(length(p_list) > 0, "You have not chosen yet, or there is no result for this drug-omic pair.")
    )
    p <- wrap_plots(p_list, ncol = 3)
    return(p)
  })
}
