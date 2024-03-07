uiFeatureDatabaseSig <- function(id){
  ns <- NS(id)
  fluidPage(
    useWaiter(),
    fluidRow(
      # Select Features ----
      column(3,
             selectInput(inputId = ns("select_features1"), 
                         "Please select the feature type:", 
                         choices = c("Copy Number Data" = "cnv",
                                     "DNA Methylation" = "meth",
                                     "Gene Fusion" = "fusion",
                                     "Gene Mutation" = "mutation_gene",
                                     "Gene Site Mutation" = "mutation_site",
                                     "mRNA Expression" = "mRNA",
                                     "Protein Expression" = "protein",
                                     "Drug Sensitivity" = "drug"
                         )
             )),
      # Select specific feature ----
      column(3,
             selectizeInput(
               ns("select_specific_feature"), "Features Selection:", choices = NULL,
               options = list(
                 placeholder = 'Please select a feature',
                 onInitialize = I('function() { this.setValue(""); }'), selected = ""
               ))
      ),
      # Select Feature to compare ----
      column(3,
             selectInput(inputId = ns("select_features2"), 
                         "Please select the feature type:", 
                         choices = c("Copy Number Data" = "cnv",
                                     "DNA Methylation" = "meth",
                                     "Gene Fusion" = "fusion",
                                     "Gene Mutation" = "mutation_gene",
                                     "Gene Site Mutation" = "mutation_site",
                                     "mRNA Expression" = "mRNA",
                                     "Protein Expression" = "protein",
                                     "Drug Sensitivity" = "drug"
                         )
             )),
      # Set the threshold value for setting significance ----
      column(3, 
             uiOutput(ns("threshold_value"))
      )  
    ),
    # Output results ----
    wellPanel(
      # plotOutput(ns("p_search2"), height="20cm"),
      column(12,
             tabsetPanel(
               tabPanel("Frequency Table",
                        DT::dataTableOutput(ns("freq_table"))),
               tabPanel("Result Table",
                        DT::dataTableOutput(ns("re_table"))),
             )),
      h5("."),
    ),
    # Download results ----
    downloadButton(ns("download_sig_re"), 'Download Significant Results'),
    h5("."),
    h4(strong("NOTEs:")),
    h5("This analysis can examine connections between a target feature and all features within all datasets."),
    h5("the determination of effect size and p-value varies based on the type of the features being compared:"),
    h5("1. For ", strong("continuous features"), "against ", strong("continuous datasets"),", the Spearman correlation coefficient R, which ranges from 0 to 1, is utilized;"),
    h5("2. When comparing", strong("discrete features"), "against ", strong("continuous datasets"), ", the log2 fold change (events/wildtype) serves as the measure of effect, with p-values obtained from the Wilcoxon test;"),
    h5("3. In the case of", strong(" discrete features"), " against ", strong("discrete datasets"), ", the effect size is quantified using the log2 odds ratio, with p-values calculated via the Chi-squared test;"),
    h5("The continuous features comprise Copy Number Data, DNA Methylation, mRNA Expression, Protein Expression, and drug sensitivity. On the other hand, the discrete features encompass Gene Fusion, Gene Mutation, and Gene Site Mutation."),
    h5("The Frequency table contains two columns: the count column, which records the number of significant pairs, and the proportion column, representing the ratio of significant pairs to total pairs."),
    h5("You can change the threshold to filter the effect size. The result table indicates a 'yes' in the Significance column when the p-value is below 0.05 and the effect size falls above the threshold defined by the user for the feature pair.")
  )
}

serverFeatureDatabaseSig <- function(input, output, session){
  ns <- session$ns
  # Select ----
  features_search_sel <- reactiveValues()
  observeEvent(input$select_features1, {
    features_search_sel$features <- switch(input$select_features1,
                                           "drug" = drugs_search$drugs, 
                                           "mRNA" = omics_search[omics_search$type %in% "mRNA",]$omics,
                                           "meth" = omics_search[omics_search$type %in% "meth",]$omics,
                                           "protein" = omics_search[omics_search$type %in% "protein",]$omics,
                                           "cnv" = omics_search[omics_search$type %in% "cnv",]$omics,
                                           "mutation_gene" = omics_search[omics_search$type %in% "mutation_gene",]$omics,
                                           "mutation_site" = omics_search[omics_search$type %in% "mutation_site",]$omics,
                                           "fusion" = omics_search[omics_search$type %in% "fusion",]$omics)
    updateSelectizeInput(session = session, inputId = 'select_specific_feature',
                         label = 'Features Selection:', choices = features_search_sel$features, server = TRUE,
                         options = list(placeholder = 'Please select a feature', onInitialize = I('function() { this.setValue(""); }')),
                         selected = ""
    )
  })
  # Set the threshold value for setting significance ----
  output$threshold_value <- renderUI({
    # Spearman correlation
    if (input$select_features1 %in% c("drug", "cnv",
                                      "protein",
                                      "meth",
                                      "mRNA") & 
        input$select_features2 %in% c("drug", "cnv",
                                      "protein",
                                      "meth",
                                      "mRNA")) {
      numericInput(inputId = ns("dynamic_numeric"), 
                   "Please set the threshold for effect size:", 
                   value = 0.2, min = 0)
      # Chi-squared test
    } else if(!input$select_features1 %in% c("drug", "cnv",
                                             "protein",
                                             "meth",
                                             "mRNA") & 
              !input$select_features2 %in% c("drug", "cnv",
                                             "protein",
                                             "meth",
                                             "mRNA")){
      numericInput(inputId = ns("dynamic_numeric"), 
                   "Please set the threshold for size effect:", 
                   value = 4, min = 0) 
    } else{
      # Wilcoxon test
      numericInput(inputId = ns("dynamic_numeric"), 
                   "Please set the threshold for size effect:", 
                   value = 4, min = 0)
    }
  })
  # Preparation ----
  re_list <- reactive({
    profile_vec1 <- profile_vec_list[[input$select_features1]]
    profile_vec2 <- profile_vec_list[[input$select_features2]]
    profile_comb <- expand.grid(profile_vec1, profile_vec2)
    re_list <- list()
    withProgress(message = "Calculation(Please be patient)", value = 0, {
      waiter_show( # show the waiter
        html = spin_3(), # use a spinner
        color = transparent(0.1)
      )
      for(index in 1:nrow(profile_comb)){
        # w$show()
        # index = 1
        # Prepare
        select_features1_2 <- input$select_features1
        if(select_features1_2 == "mRNA") select_features1_2 <- "exp"
        profile1 <- base::get(paste0(profile_comb[index,1], "_", select_features1_2), envir = parent.env(environment()))
        select_features2_2 <- input$select_features2
        if(select_features2_2 == "mRNA") select_features2_2 <- "exp"
        profile2 <- base::get(paste0(profile_comb[index,2], "_", select_features2_2), envir = parent.env(environment()))
        # Select specific feature and all features data
        # con vs con ----
        if(input$select_features1 %in% c("drug", "cnv",
                                         "protein",
                                         "meth",
                                         "mRNA") & 
           input$select_features2 %in% c("drug", "cnv",
                                         "protein",
                                         "meth",
                                         "mRNA")){
          intersected_cells <- intersect(colnames(profile1), colnames(profile2))
          fea <- profile1[rownames(profile1) %in% input$select_specific_feature,
                          match(intersected_cells, colnames(profile1))] %>% as.numeric()
          db <- profile2[,match(intersected_cells, colnames(profile2))]
          db <- db[!rownames(db) %in% input$select_specific_feature,]
          fea_nrow <- profile1[rownames(profile1) %in% input$select_specific_feature,match(intersected_cells, colnames(profile1))] %>% nrow()
          if(fea_nrow == 0 | length(intersected_cells) == 0){next}
          # Calculate
          re <- lapply(1:nrow(db), function(x){
            re2 <- tryCatch(cor.test(fea, as.numeric(db[x,]),
                                     method = "spearman"),
                            error = function(x){NA})
            if(all(is.na(re2))){
              re3 <- data.frame(
                p = NA,
                effect = NA
              )
            } else {
              re3 <- data.frame(
                p = re2$p.value,
                effect = re2$estimate)
            }
          })
          re <- do.call(rbind, re)
          re$fea <- rownames(db)
          re <- na.omit(re)
          # re <- re[order(re$R),]
          # quantiles_80 <- quantile(re$R, probs = c(0.1, 0.9))[2]
          # quantiles_20 <- quantile(re$R, probs = c(0.1, 0.9))[1]
          # dis vs dis ----
        } else if(!input$select_features1 %in% c("drug", "cnv",
                                                 "protein",
                                                 "meth",
                                                 "mRNA") & 
                  !input$select_features2 %in% c("drug", "cnv",
                                                 "protein",
                                                 "meth",
                                                 "mRNA")){
          intersected_cells <- intersect(profile1[[2]], profile2[[2]]) %>% unique()
          fea <- profile1[profile1[[1]] %in% input$select_specific_feature,]
          db <- profile2[profile2[[2]] %in% intersected_cells,]
          db <- db[!db[[1]] %in% input$select_specific_feature,]
          db_feas <- unique(db[[1]])
          if(nrow(fea) == 0 | length(intersected_cells) == 0){next}
          re <- lapply(1:length(db_feas), function(x){
            fea_cells <- unique(as.data.frame(fea)[,2])
            sel_cells <- unique(as.data.frame(db[db[[1]] %in% db_feas[x],])[,2])
            yes_yes <- length(intersected_cells[intersected_cells %in% intersect(fea_cells, sel_cells)])
            yes_no <- length(intersected_cells[intersected_cells %in% fea_cells & !(intersected_cells %in% sel_cells)])
            no_yes <- length(intersected_cells[intersected_cells %in% sel_cells & !(intersected_cells %in% fea_cells)])
            no_no <- length(intersected_cells[!intersected_cells %in% c(fea_cells, sel_cells)])
            chi_df <- t(data.frame(
              yes = c(yes_yes, yes_no),
              no = c(no_yes, no_no)
            ))
            re2 <- tryCatch(
              chisq.test(chi_df),
              error = function(x){NA}
            )
            if(all(is.na(re2))){
              re3 <- data.frame(
                p = NA,
                effect = NA
              )
            } else {
              re3 <- data.frame(
                p = re2$p.value,
                effect = log2(re2$statistic)
              )
            }
            re3
          })
          re <- do.call(rbind, re)
          re$fea <- db_feas
          re <- na.omit(re)
          # con vs dis ----
        } else if(input$select_features1 %in% c("drug", "cnv",
                                                "protein",
                                                "meth",
                                                "mRNA") & 
                  !input$select_features2 %in% c("drug", "cnv",
                                                 "protein",
                                                 "meth",
                                                 "mRNA")){
          intersected_cells <- intersect(colnames(profile1), profile2[[2]]) %>% unique()
          fea <- profile1[rownames(profile1) %in% input$select_specific_feature,
                          match(intersected_cells, colnames(profile1))]
          db <- profile2[profile2[[2]] %in% intersected_cells,]
          db_feas <- unique(db[[1]])
          if(nrow(fea) == 0 | length(intersected_cells) == 0){next}
          re <- lapply(1:length(db_feas), function(x){
            sel_cells <- as.data.frame(db[db[[1]] %in% db_feas[x],2])
            sel_cells <- sel_cells[,1]
            yes_drugs <- na.omit(as.numeric(fea[,colnames(fea) %in% sel_cells]))
            no_drugs <- na.omit(as.numeric(fea[,!colnames(fea) %in% sel_cells]))
            re2 <- tryCatch(
              wilcox.test(yes_drugs, no_drugs),
              error = function(x){NA}
            )
            if(all(is.na(re2))){
              re3 <- data.frame(
                p = NA,
                effect = NA
              )
            } else {
              re3 <- data.frame(
                p = re2$p.value,
                effect = log2(mean(yes_drugs)/mean(no_drugs))
              )
            }
            re3
          })
          re <- do.call(rbind, re)
          re$fea <- db_feas
          re <- na.omit(re)
          # dis vs con ----
        } else if(!input$select_features1 %in% c("drug", "cnv",
                                                 "protein",
                                                 "meth",
                                                 "mRNA") & 
                  input$select_features2 %in% c("drug", "cnv",
                                                "protein",
                                                "meth",
                                                "mRNA")){
          intersected_cells <- intersect(profile1[[2]], colnames(profile2)) %>% unique()
          db <- profile2[,colnames(profile2) %in% intersected_cells]
          sel_omics <- profile1$cells[profile1[[1]] %in% input$select_specific_feature] %>% unique()
          if(length(intersected_cells) == 0 | length(sel_omics) == 0){next}
          re <- lapply(1:nrow(db), function(x){
            # x = 1
            yes_drugs <- na.omit(as.numeric(db[x,colnames(db) %in% sel_omics]))
            no_drugs <- na.omit(as.numeric(db[x,!colnames(db) %in% sel_omics]))
            re2 <- tryCatch(
              wilcox.test(yes_drugs, no_drugs),
              error = function(x){NA}
            )
            if(all(is.na(re2))){
              re3 <- data.frame(
                p = NA,
                effect = NA
              )
            } else {
              re3 <- data.frame(
                p = re2$p.value,
                effect = log2(mean(yes_drugs)/mean(no_drugs))
              )
            }
          })
          re <- do.call(rbind, re)
          re$fea <- rownames(db)
          re <- na.omit(re)
        }
        if(nrow(re) == 0){ next }
        re$sig <- "no"
        # add db name
        re$database <- paste0(profile_comb$Var1[index], "_", profile_comb$Var2[index])
        rownames(re) <- NULL
        re_list[[index]] <- re
        incProgress(1/nrow(profile_comb), detail = paste0("Doing part ", index, "(Total ", nrow(profile_comb), ")"))
        # Warning 
        shiny::validate(
          need(length(profile_comb) > 0, "You have not chosen yet, or there is no result for this feature-database pair.")
        )
      }
      waiter_hide() # hide the waiter
    }
    )
    re_list <- re_list[!sapply(re_list, is.null)] 
    re_list
  })
  re_list2 <- reactive({
    lapply(re_list(), function(re){
      re$sig[abs(re$effect) > input$dynamic_numeric & re$p < .05] <- "yes"
      re
    })
  })
  # Freq Table ----
  output$freq_table <- DT::renderDataTable({ 
    # add freq
    re_name_list1 <- lapply(re_list2(), function(x){
      x$fea
    })
    # Warning 
    shiny::validate(
      need(length(re_name_list1) > 0, "You have not chosen yet, or there is no result for this feature-database pair.")
    )
    # message(class(re_list()))
    test1 <- unlist(re_name_list1, use.names = F)
    test1 <- as.data.frame(table(test1))
    colnames(test1)[1] <- "Name"
    # sig freq
    re_name_list2 <- lapply(re_list2(), function(x){
      x$fea[x$sig %in% "yes"]
    })
    test2 <- unlist(re_name_list2, use.names = F)
    test2 <- as.data.frame(table(test2))
    colnames(test2)[1] <- "Name"
    test2_2 <- data.frame(
      Name = test1$Name[!test1$Name %in% test2$Name],
      Freq = 0
    )
    test2 <- rbind(test2, test2_2)
    test2 <- test2[match(test1$Name,test2$Name),]
    freq_df <- test2
    freq_df$Prop <- test2$Freq/test1$Freq
    freq_df <- freq_df[order(freq_df$Prop, freq_df$Freq, decreasing = T),]
    colnames(freq_df) <- c("Feature", "Counts", "Proportion")
    freq_df
  }, options = list(scrollX = TRUE), selection = 'single')
  # Result Table ----
  output$re_table <- DT::renderDataTable({
    re_df <- do.call(rbind, re_list2())
    shiny::validate(
      need(length(re_df) > 0, "You have not chosen yet, or there is no result for this feature-database pair.")
    )
    colnames(re_df) <- c("P value", "Effect size", "Feature",
                         "Significance", "Dataset")
    rownames(re_df) <- NULL
    return(re_df)
  })
  # Download ----
  # input_list <- reactive({
  #   list(
  #     select_features1 = input$select_features1,
  #     select_specific_feature = input$select_specific_feature,
  #     select_features2 = input$select_features2
  #   )
  # })
  output$download_sig_re <- downloadHandler(
    filename =  function(){
      paste0(input$select_features1, "_", 
             input$select_specific_feature, "-",
             input$select_features2, "-",
             "Sig_result",
             ".csv")
    },
    content = function(filename) {
      re_df <- do.call(rbind, re_list2())
      data.table::fwrite(re_df, 
                         file = filename)
    })
}
