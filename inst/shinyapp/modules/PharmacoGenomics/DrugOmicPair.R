uiDrugOmicPair <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             selectInput(inputId = ns("select_omics"), 
                         "Please select the omic type:", 
                         choices = c("mRNA", "meth",
                                     "protein", "cnv",
                                     "mutation_gene", "mutation_site",
                                     "fusion")
             )),
      # Select omics ----
      column(4,
             selectizeInput(
               ns("select_specific_omic"), "Omics Selection", choices = NULL,
               options = list(
                 placeholder = 'Please select an omic',
                 onInitialize = I('function() { this.setValue(""); }'), selected = "ABCC3"
               )),
             # DOPselectOmicsUI("DOPselectOmics"),
      ),
      # Select drugs ----
      column(4,
             selectizeInput(
               ns("select_specific_drug"), "Drugs Selection", choices = NULL,
               options = list(
                 placeholder = 'Please select a drug',
                 onInitialize = I('function() { this.setValue(""); }'), selected = "YM-155"
               ))
             # DOPselectDrugsUI("DOPselectDrugs"),
      ),
    ),
    # Plot results ----
    wellPanel(
      # textOutput("total")
      plotOutput(ns("p_search"), height="20cm"),
    ),
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
                         label = 'Omics Selection', choices = omics_search_sel$omics, server = TRUE,
                         options = list(placeholder = 'Please select an omic', onInitialize = I('function() { this.setValue(""); }')),
                         selected = "ABCC3"
    )
  })
  # message(
  #   paste0("omics_search_sel:", object.size(omics_search_sel))
  # )
  
  ## Drugs ----
  updateSelectizeInput(session = session, inputId = 'select_specific_drug',
                       label = 'Drugs Selection', choices = drugs_search$drugs, server = TRUE,
                       options = list(placeholder = 'Please select a drug', onInitialize = I('function() { this.setValue(""); }')),
                       selected = "YM-155"
  )
  # Plot ----
  # Select omics
  omics_search_list1_sel <- reactive({omics_search_list1[[input$select_omics]]}) # cor
  omics_search_list2_sel <- reactive({omics_search_list2[[input$select_omics]]}) # discrete
  # message(class(omics_search_list1_sel))
  output$p_search <- renderPlot({
    # names(omics_search_list1)
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
        # sel_omics <- omics_sel[rownames(omics_sel) %in% "TSPAN6",] %>% as.numeric()
        sel_drugs <- drugs_sel[rownames(drugs_sel) %in% input$select_specific_drug,] %>% as.numeric()
        # sel_drugs <- drugs_sel[rownames(drugs_sel) %in% "YM-155",] %>% as.numeric()
        if(length(na.omit(sel_omics)) == 0 | length(na.omit(sel_drugs)) == 0){ return(NULL) }
        cor_df <- data.frame(
          genes = sel_omics,
          drugs = sel_drugs
        )
        p <- ggscatter(cor_df, x = "genes", y = "drugs") + 
          stat_cor(size = 6) + stat_smooth(method = "lm") + theme_bw() + 
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
        # sel_omics <- omics_sel$cells[omics_sel[[1]] %in% "TP53"] %>% unique()
        sel_drugs <- drugs_sel[rownames(drugs_sel) %in% input$select_specific_drug,] %>% as.numeric()
        # sel_drugs <- drugs_sel[rownames(drugs_sel) %in% "YM-155",] %>% as.numeric()
        yes_drugs <- sel_drugs[colnames(drugs_sel) %in% sel_omics] %>% na.omit()
        no_drugs <- sel_drugs[!colnames(drugs_sel) %in% sel_omics] %>% na.omit()
        if(length(yes_drugs) == 0 | length(no_drugs) == 0){ return(NULL) }
        box_df <- data.frame(
          drugs = c(no_drugs, yes_drugs),
          events = rep(c("no","yes"), times = c(length(no_drugs), length(yes_drugs))))
        p <- ggboxplot(data = box_df, x = "events", y = "drugs",
                  fill = "events", palette = c("#BEBADAFF", "#FB8072FF"),
                  add = "jitter") + 
          stat_compare_means(size = 5,
                             label.y.npc = "bottom") + theme_bw() + 
          theme(
            axis.title = element_blank(),
            title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 12),
            legend.position = "none"
          ) + ggtitle(names(omics_search_list2_sel())[[x]])
      })
    }
    p_list <- p_list[!sapply(p_list, is.null)]
    # Warning 
    validate(
      need(length(p_list) > 0, "You have not chosen yet, or there is no result for this drug-omic pair.")
    )
    p <- wrap_plots(p_list, ncol = 3)
    return(p)
  })
}
