uiFeatureAcrossType <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # Select Features ----
      column(4,
             selectInput(inputId = ns("select_features"), 
                         "Please select the feature type:", 
                         choices = c("mRNA", "meth",
                                     "protein", "cnv",
                                     "mutation_gene", "mutation_site",
                                     "fusion", "drug")
             )),
      # Select specific feature ----
      column(4,
             selectizeInput(
               ns("select_specific_feature"), "Features Selection", choices = NULL,
               options = list(
                 placeholder = 'Please select a feature',
                 onInitialize = I('function() { this.setValue(""); }'), selected = "LAPATINIB"
               ))
             # DOPselectDrugsUI("DOPselectDrugs"),
      ),
      # Select trait to profile ----
      column(4,
             selectInput(inputId = ns("select_trait"), 
                         "Please select the trait type:", 
                         choices = c("Cell_Type_Source"), selected = "Cell_Type_Source"
             )),
    ),
    # Plot results ----
    wellPanel(
      # textOutput("total")
      plotOutput(ns("p_search2"), height="20cm"),
    ),
    # Download plot ----
    # column(3,
    #        downloadLink(ns("dl_p_search"), "Save as PDF")
    # )
  )
}

serverFeatureAcrossType <- function(input, output, session){
  ns <- session$ns
  # Select ----
  features_search_sel <- reactiveValues()
  observeEvent(input$select_features, {
    features_search_sel$features <- switch(input$select_features,
                                     "drug" = drugs_search$drugs, 
                                     "mRNA" = omics_search[omics_search$type %in% "mRNA",]$omics,
                                     "meth" = omics_search[omics_search$type %in% "meth",]$omics,
                                     "protein" = omics_search[omics_search$type %in% "protein",]$omics,
                                     "cnv" = omics_search[omics_search$type %in% "cnv",]$omics,
                                     "mutation_gene" = omics_search[omics_search$type %in% "mutation_gene",]$omics,
                                     "mutation_site" = omics_search[omics_search$type %in% "mutation_site",]$omics,
                                     "fusion" = omics_search[omics_search$type %in% "fusion",]$omics)
    updateSelectizeInput(session = session, inputId = 'select_specific_feature',
                         label = 'Features Selection', choices = features_search_sel$features, server = TRUE,
                         options = list(placeholder = 'Please select a feature', onInitialize = I('function() { this.setValue(""); }')),
                         selected = "ABCC3"
    )
  })
  # Plot ----
  output$p_search2 <- renderPlot({
    ## Box plot ----
    if(input$select_features %in% c("cnv",
                                    "drug",
                                 "protein",
                                 "meth",
                                 "mRNA")){
        # message(
        #   profile_vec_list[[input$select_features]]
        # )
      p_list <- lapply(profile_vec_list[[input$select_features]], function(x){
        # x = tmp$profile_vec[1]
        # Preprocess
        select_features <- input$select_features
        if(select_features == "mRNA") select_features <- "exp"
        profile <- base::get(paste0(x, "_", select_features), envir = env)
        intersected_cells <- intersect(cell_anno$Name, colnames(profile))
        sel_profile <- profile[rownames(profile) %in% input$select_specific_feature,
                               match(intersected_cells, colnames(profile))]
        cell_anno2 <- cell_anno[match(intersected_cells,cell_anno$Name),c(1,2)]
        sel_profile <- t(sel_profile) %>% as.numeric()
        if(length(na.omit(sel_profile)) == 0){ return(NULL) }
        # Plot
        profile_df <- data.frame(
          profile = sel_profile,
          anno = cell_anno2$Type)
        profile_df2 <- na.omit(profile_df)
        pval <- kruskal.test(split(profile_df2$profile, profile_df2$anno))$p.value %>% round(4)
        pval <- case_when(
          pval < 0.01 ~ "< 0.01",
          pval < 0.05 ~ "< 0.05",
          T ~ "> 0.05"
        )
        p <- ggboxplot(data = profile_df2, x = "anno", y = "profile",
                       add = "jitter") + 
          # stat_compare_means(size = 5,
          #                    # label.y.npc = "bottom"
          # ) 
          # geom_signif(annotations = pval) + 
          theme_bw() + 
          theme(
            axis.title = element_blank(), 
            plot.title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)
          ) + scale_x_discrete(limit = unique(cell_anno$Type)) +
          labs(title = x, 
               subtitle = paste0("Kruskal-Wallis, p ", pval))
        return(p)
      })
    # Bar plot ----    
    } else{
      p_list <- lapply(profile_vec_list[[input$select_features]], function(x){
        # x = tmp$profile_vec[1]
        # Preprocess
        profile <- base::get(paste0(x, "_", input$select_features))
        sel_profile <- profile[profile[[1]] %in% input$select_specific_feature,]
        if(nrow(sel_profile) == 0){ return(NULL) }
        ## If cells in sel_profile, label "yes"
        sel_profile_df1 <- data.frame(
          cells = sel_profile$cells,
          events = "yes"
        )
        sel_profile_df2 <- data.frame(
          cells = profile$cells[!profile$cells %in% sel_profile$cells],
          events = "no"
        ) %>% unique()
        sel_profile_df <- rbind(sel_profile_df1, sel_profile_df2)
        sel_profile_df <- base::merge(sel_profile_df, cell_anno[,1:2],
                                      by.x = "cells", by.y = "Name")
        sel_profile_fq <- as.data.frame(prop.table(table(sel_profile_df$events, sel_profile_df$Type), margin = 2))
        colnames(sel_profile_fq) <- c("events", "Type", "Freq")
        # Plot
        pval <- chisq.test(table(sel_profile_df$events, sel_profile_df$Type))$p.value %>% round(4)
        pval <- case_when(
          pval < 0.01 ~ "< 0.01",
          pval < 0.05 ~ "< 0.05",
          T ~ "> 0.05"
        )
        p <- ggplot(sel_profile_fq) + 
          geom_bar(aes(x = Type, y= Freq*100, fill = events), color = "white",stat = "identity",width = 0.7,linewidth = 0.5)+ 
          theme_bw() + scale_fill_manual(values = c("#BEBADAFF", "#FB8072FF")) + 
          theme(
            axis.title.x = element_blank(), 
            plot.title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)
          ) + scale_x_discrete(limit = unique(cell_anno$Type)) + 
          labs(x='',y = 'Percentage(%)',
               title = x, 
               subtitle = paste0("Chi-Squared, p ", pval)) 
      })
    }
    p_list <- p_list[!sapply(p_list, is.null)]
    # Warning 
    validate(
      need(length(p_list) > 0, "You have not chosen yet, or there is no result for this feature.")
    )
    if(length(p_list) > 1){
      for(i in 1:(length(p_list)-1)){
        p_list[[i]] <- p_list[[i]] + theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      }
    }
    p <- wrap_plots(p_list, ncol = 1)
    return(p)
  })
}
