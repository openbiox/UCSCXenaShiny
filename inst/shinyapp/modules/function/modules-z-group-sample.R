group_samples_UI = function(id, button_name="Filter by multi-conditions"){
  ns = NS(id)
  tagList(
    h4("1. Add phenotypes[opt]"),
    
    fluidRow(
      column(
        6,
        selectInput(
          ns("data_L1"), "Data type:",
          choices = c("Molecular profile", "Tumor index", "Immune Infiltration", "Pathway activity","Custom metadata"),
          selected = "Molecular profile"
        )
      ),
      column(
        6,
        tabsetPanel(
          id = ns("data_L2_tab"),
          type = "hidden",
          # selected = "Molecular_profile",
          tabPanel("Molecular profile",
                   selectInput(
                     ns("genomic_profile"), "Data subtype:",
                     choices = c("mRNA Expression", "Transcript Expression", "DNA Methylation", 
                                 "Protein Expression", "miRNA Expression", "Mutation status","Copy Number Variation"),
                     selected = "mRNA Expression")
          ),
          tabPanel("Tumor index",
                   selectInput(
                     ns("tumor_index"), "Data subtype:",
                     choices = c("Tumor Purity","Tumor Stemness","Tumor Mutation Burden",
                                 "Microsatellite Instability","Genome Instability"),
                     selected = "Tumor Purity")
          ),
          tabPanel("Immune Infiltration",
                   selectInput(
                     ns("immune_infiltration"), "Data subtype:",
                     choices = sort(unique(TIL_meta$method)),
                     selected = "CIBERSORT")
          ),
          tabPanel("Pathway activity",
                   selectInput(
                     ns("pathway_activity"), "Data subtype:",
                     choices = c("HALLMARK","KEGG","IOBR"),
                     selected = "HALLMARK"),
          ),
          tabPanel("Custom metadata",
                    selectInput(
                      ns("custom_metadata"), "Data subtype:",
                      choices = c("custom_metadata"),
                      selected = "custom_metadata")
          )
        )		
      )
    ),
    tabsetPanel(
      id = ns("data_L3_tab"),
      type = "hidden",
      # selected = "Molecular_profile",
      tabPanel("Molecular profile",
               selectizeInput(
                 inputId = ns("genomic_profile_id"),
                 label = "Identifier:",
                 choices = NULL
               )
      ),
      tabPanel("Tumor index",
               selectizeInput(
                 inputId = ns("tumor_index_id"),
                 label = "Identifier:",
                 choices = NULL)
      ),
      tabPanel("Immune Infiltration",
               selectizeInput(
                 inputId = ns("immune_infiltration_id"),
                 label = "Identifier:",
                 choices = NULL)
      ),
      tabPanel("Pathway activity",
               selectizeInput(
                 inputId = ns("pathway_activity_id"),
                 label = "Identifier:",
                 choices = NULL)	
      ),
      tabPanel("Custom metadata",
               selectizeInput(
                 inputId = ns("custom_metadata_id"),
                 label = "Identifier:",
                 choices = NULL)  
      )
    ),
    fluidRow(
      actionBttn(
        inputId = ns("button_phe_add"),
        label = "Add",
        color = "primary",
        style = "bordered", size = "sm",
        block = F
      ),
      actionBttn(
        inputId = ns("button_phe_add_reset"),
        label = "Reset",
        color = "primary",
        style = "bordered", size = "sm",
        block = F
      )       
    ),
    verbatimTextOutput(ns("add_info")),
    
    h4("2. Select one phenotype"),
    uiOutput(ns("filter_phe_01_by.ui")),
    verbatimTextOutput(ns("tmp789")),
    verbatimTextOutput(ns("filter_phe_01_out_2")),
    
    h4("3. Set two groups") %>%
        helper(type = "markdown", size = "m", fade = TRUE, 
                   title = "Split above phenotype into 2 groups", 
                   content = "set_groups"),
		uiOutput(ns("set_quantile.ui")),
		uiOutput(ns("set_group1.ui")),
		uiOutput(ns("set_group2.ui")),


		fluidRow(
	    actionBttn(
	      inputId = ns("button_merge"),
	      label = "Run grouping",
	      color = "primary",
	      style = "bordered", size = "sm",
	      block = F
	    )
		),
		materialSwitch(ns("reverse_level"), "Whether reverse levels?"),

    verbatimTextOutput(ns("choose_group_2merge_out"))
    
    
  )
}


group_samples_Server = function(input, output, session, cancers=NULL, samples=NULL, custom_metadata=NULL, opt_pancan=NULL){
  ns <- session$ns
  observe({
    updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
  }) 
  observe({
    updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
  }) 
  genomic_profile_choices <- reactive({
    switch(input$genomic_profile,
           `mRNA Expression` = list(all = pancan_identifiers$gene, default = "TP53"),
           `Transcript Expression` = list(all = load_data("transcript_identifier"), default = "ENST00000000233"),
           `DNA Methylation` = list(all = pancan_identifiers$gene, default = "TP53"),
           `Protein Expression` = list(all = pancan_identifiers$protein, default = "P53"),
           `miRNA Expression` = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
           `Mutation status` = list(all = pancan_identifiers$gene, default = "TP53"),
           `Copy Number Variation` = list(all = pancan_identifiers$gene, default = "TP53"),
           list(all = "NONE", default = "NONE")
    )
  })
  
  tumor_index_choices <- reactive({
    switch(input$tumor_index,
           `Tumor Purity` = list(all = colnames(tumor_index_list$tcga_purity)[3:7], default = "ESTIMATE"),
           `Tumor Stemness` = list(all = colnames(tumor_index_list$tcga_stemness)[2:6], default = "RNAss"),
           `Tumor Mutation Burden` = list(all = colnames(tumor_index_list$tcga_tmb)[4:5], default = "Non_silent_per_Mb"),
           `Microsatellite Instability` = list(all = colnames(tumor_index_list$tcga_msi)[3:21], default = "Total_nb_MSI_events"),
           `Genome Instability` = list(all = colnames(tumor_index_list$tcga_genome_instability)[2:6], default = "ploidy"),
           list(all = "NONE", default = "NONE")
    )
  })
  
  immune_infiltration_choices <- reactive({
    switch(input$immune_infiltration,
           `CIBERSORT` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="CIBERSORT"]), default = "Monocyte"),
           `CIBERSORT-ABS` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="CIBERSORT-ABS"]), default = "Monocyte"),
           `EPIC` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="EPIC"]), default = "Macrophage"),
           `MCPCOUNTER` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="MCPCOUNTER"]), default = "Monocyte"),
           `QUANTISEQ` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="QUANTISEQ"]), default = "Monocyte"),
           `TIMER` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="TIMER"]), default = "Macrophage"),
           `XCELL` = list(all = sort(TIL_meta$celltype[TIL_meta$method=="XCELL"]), default = "Monocyte"),
           list(all = "NONE", default = "NONE")
    )
  })
  
  pathway_activity_choices <- reactive({
    switch(input$pathway_activity,
           `HALLMARK` = list(all = sort(PW_meta$Name[PW_meta$Type=="HALLMARK"]), default = "APOPTOSIS"),
           `KEGG` = list(all = sort(PW_meta$Name[PW_meta$Type=="KEGG"]), default = "CELL_CYCLE"),
           `IOBR` = list(all = sort(PW_meta$Name[PW_meta$Type=="IOBR"]), default = "Biotin_Metabolism"),
           list(all = "NONE", default = "NONE")
    )
  })


  custom_metadata_choices <- reactive({
    if(is.null(custom_metadata)){
      choice_all = "NULL"
      choice_default ="NULL"
    } else {
      choice_all = sort(colnames(custom_metadata()[-1]))
      choice_default = sort(colnames(custom_metadata()[-1]))[1]

    }

    switch(input$custom_metadata,
      `custom_metadata` = list(all = choice_all,  default = choice_default),
      list(all = "NONE", default = "NONE")
    )
  })

  
  observe({
    updateSelectizeInput(
      session,
      "genomic_profile_id",
      choices = genomic_profile_choices()$all,
      selected = genomic_profile_choices()$default,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "tumor_index_id",
      choices = tumor_index_choices()$all,
      selected = tumor_index_choices()$default,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "immune_infiltration_id",
      choices = immune_infiltration_choices()$all,
      selected = immune_infiltration_choices()$default,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "pathway_activity_id",
      choices = pathway_activity_choices()$all,
      selected = pathway_activity_choices()$default,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "custom_metadata_id",
      choices = custom_metadata_choices()$all,
      selected = custom_metadata_choices()$default,
      server = TRUE
    )
  })
  
  add_level2 = reactive({
    switch(input$data_L1,
           `Molecular profile` = input$genomic_profile,
           `Tumor index` = input$tumor_index,
           `Immune Infiltration` = input$immune_infiltration,
           `Pathway activity` = input$pathway_activity,
           `Custom metadata` = input$custom_metadata
    )
  })
  add_level3 = reactive({
    switch(input$data_L1,
           `Molecular profile` = input$genomic_profile_id,
           `Tumor index` = input$tumor_index_id,
           `Immune Infiltration` = input$immune_infiltration_id,
           `Pathway activity` = input$pathway_activity_id,
           `Custom metadata` = input$custom_metadata_id
    )
  })
  
  add_phes <- reactiveValues(cancers = "BRCA",click=0, name = list(), label=list(), show="")
  
  observe({
    if(!is.null(cancers)){
      add_phes$cancers = cancers()
    }
  })
  observeEvent(input$button_phe_add, {
    add_phes$click = add_phes$click+1	
    add_phes$name = c(add_phes$name, 
                      paste0(input$data_L1,"--",add_level2(),"--",add_level3()))
    add_phes$label = c(add_phes$label, 
                       paste0("Add_",add_phes$click))
    add_phes$show = paste0(add_phes$show,
                           "Add_",add_phes$click,": ", input$data_L1,"--",add_level2(),
                           "--",add_level3(),"\n")
  }) 
  observeEvent(input$button_phe_add_reset, {
    add_phes$click = 0	
    add_phes$name = list()
    add_phes$label=list()
    add_phes$show=""
  }) 
  output$add_info = renderPrint({cat(add_phes$show)})
  
  # 下载已添加的变量数据
  add_phes_dat = eventReactive(input$button_phe_add, {
    
    x_tmp = lapply(seq(add_phes$name), function(i){
      tmp_data_type = str_split(add_phes$name[[i]], "--")[[1]][1]
      tmp_data_sub = str_split(add_phes$name[[i]], "--")[[1]][2]
      tmp_data_target = str_split(add_phes$name[[i]], "--")[[1]][3]
      
      if(tmp_data_type == "Molecular profile"){
        if(is.null(opt_pancan)){
          opt_pancan = list(
              toil_mRNA = list(),
              toil_transcript = list(),
              toil_protein = list(),
              toil_mutation = list(),
              toil_cnv = list(use_thresholded_data = TRUE),
              toil_methylation = list(type = "450K", aggr = "Q25",rule_out = NULL),
              toil_miRNA = list()
          )
        } else {
          opt_pancan = opt_pancan()
        }
        x_genomic_profile = switch(tmp_data_sub,
                                   `mRNA Expression` = "mRNA",
                                   `Transcript Expression` = "transcript",
                                   `DNA Methylation` = "methylation",
                                   `Protein Expression` = "protein",
                                   `miRNA Expression` = "miRNA",
                                   `Mutation status` = "mutation",
                                   `Copy Number Variation` = "cnv"
        )
        x_data <- query_pancan_value(tmp_data_target, 
                                     data_type = x_genomic_profile,
                                     opt_pancan = opt_pancan)
        if (is.list(x_data)) x_data <- x_data[[1]]
        x_data <- data.frame(sample = names(x_data), value = as.numeric(x_data))
        
      } else if (tmp_data_type == "Tumor index"){
        x_tumor_index = switch(tmp_data_sub,
                               `Tumor Purity` = "tcga_purity",
                               `Tumor Stemness` = "tcga_stemness",
                               `Tumor Mutation Burden` = "tcga_tmb",
                               `Microsatellite Instability` = "tcga_msi",
                               `Genome Instability` = "tcga_genome_instability"
        )
        x_data = tumor_index_list[[x_tumor_index]][,c("sample", tmp_data_target)]
        colnames(x_data)[2] = "value"
        x_data = x_data %>% dplyr::filter(!is.na(value))
        
      } else if (tmp_data_type == "Immune Infiltration"){
        x_immune_infiltration = tmp_data_sub
        x_data = tcga_TIL[,c("cell_type",
                             paste0(tmp_data_target,"_",tmp_data_sub))]
        colnames(x_data) = c("sample","value")
        x_data = x_data %>% dplyr::filter(!is.na(value))
      } else if (tmp_data_type == "Pathway activity"){
        x_pathway_activity = tmp_data_sub
        x_data = tcga_PW[,paste0(x_pathway_activity,"_",tmp_data_target),drop=FALSE]
        colnames(x_data) = "value"
        x_data = x_data %>% as.data.frame() %>%
          tibble::rownames_to_column("sample") %>%
          dplyr::filter(!is.na(value))		
      } else if (tmp_data_type == "Custom metadata"){
        x_data = custom_metadata()[,c("Sample", tmp_data_target)]
        colnames(x_data) = c("sample","value")
        x_data = x_data %>% as.data.frame() %>%
          dplyr::filter(!is.na(value))    
      }
      colnames(x_data)[2] = add_phes$label[[i]]
      x_data = dplyr::left_join(
        dplyr::distinct(load_data("tcga_clinical")[,c("sample","type")]),x_data) %>%
        dplyr::select(!type) %>%
        tibble::column_to_rownames("sample")
      x_data
      
    }) %>% do.call(cbind, .)
    
    x_tmp %>% tibble::rownames_to_column("sample")
  })
  
  output$filter_phe_01_by.ui = renderUI({
    selectInput(ns("filter_phe_01_by_2"), NULL,
                choices = colnames(add_phes$phe_primary)[-1:-2], selected = "Code"
    )
  })
  observe({
    if(add_phes$click==0){
      add_phes$phe_primary = query_tcga_group(
        cancer = add_phes$cancers, 
        filter_id = samples(),
        return_all = T)
    } else {
      add_phes$phe_primary = query_tcga_group(
        cancer = add_phes$cancers, custom = add_phes_dat(),
        filter_id = samples(),
        # filter_id = ifelse(is.null(samples), samples, samples()),
        return_all = T)
    }
    add_phes$group = input$filter_phe_01_by_2
  })


  
  output$filter_phe_01_out_2 = renderPrint({
    filter_phe_01_out_2_tmp = add_phes$phe_primary %>%
      as.data.frame()
    
    filter_phe_01_out_2_tmp = filter_phe_01_out_2_tmp[,add_phes$group,drop=FALSE]
    
    filter_phe_01_out_2_tmp = filter_phe_01_out_2_tmp %>% 
      mutate(across(where(is.character), as.factor))
    summary(filter_phe_01_out_2_tmp)
  })

  # 创建两分组

  output$set_group1.ui = renderUI({
  	choice_chrs = add_phes$phe_primary[,ifelse(is.null(add_phes$group),"Code",add_phes$group),drop=T]
  	fluidRow(
  		if(class(choice_chrs)=="character"){
	  		column(
	  			8,
	  			selectInput(ns("group1_range"),"Group-1 Range",
	  				sort(unique(choice_chrs), ,na.last = T), multiple=T)
	  		)
  		} else {
	  		column(8,
	  			fluidRow(
	  				column(6,
	  					numericInput(ns("group1_min"),"Group-1 min",value=NA)),
	  				column(6,
	  					numericInput(ns("group1_max"),"max",
	  						value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T))))
	  			)
	  		)
  		}
  		,
  		column(
  			4,
  			textInput(ns("group1_name"),"Group-1 Name", "Group1")
  		)
  	)
  })

  output$set_group2.ui = renderUI({
  	choice_chrs = add_phes$phe_primary[,ifelse(is.null(add_phes$group),"Code",add_phes$group),drop=T]
  	fluidRow(
  		if(class(choice_chrs)=="character"){
	  		column(
	  			8,
	  			selectInput(ns("group2_range"),"Group-2 Range",
	  				sort(unique(choice_chrs), ,na.last = T), multiple=T)
	  		)
  		} else {
	  		column(8,
	  			fluidRow(
	  				column(6,
	  					numericInput(ns("group2_min"),"Group-2 min",
	  						value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T)))),
	  				column(6,
	  					numericInput(ns("group2_max"),"max",value=NA))
	  			)
	  		)
  		}
  		,
  		column(
  			4,
  			textInput(ns("group2_name"),"Group-2 Name", "Group2")
  		)
  	)
  })


  output$set_quantile.ui = renderUI({
  	choice_chrs = add_phes$phe_primary[,ifelse(is.null(add_phes$group),"Code",add_phes$group),drop=T]
  	if(class(choice_chrs)!="character"){
  		materialSwitch(ns("set_quantile"),"Whether group by percentile?",value = TRUE)
  	}
  })


  # 提取分组条件
  merge_by = eventReactive(input$button_merge, {
  	merge_by.list = list(NULL, NULL)
  	names(merge_by.list) = c(input$group1_name, input$group2_name)

  	choice_chrs = add_phes$phe_primary[,ifelse(is.null(add_phes$group),"Code",add_phes$group),drop=T]
  	if(class(choice_chrs)=="character"){
  		merge_by.list[[1]] = input$group1_range
  		merge_by.list[[2]] = input$group2_range
  	} else {
  		merge_by.list[[1]] = 
  			c(ifelse(is.null(input$group1_min),NA,input$group1_min),
  				ifelse(is.null(input$group1_max),NA,input$group1_max))
   		merge_by.list[[2]] = 
  			c(ifelse(is.null(input$group2_min),NA,input$group2_min),
  				ifelse(is.null(input$group2_max),NA,input$group2_max))
  	}
  	merge_by.list
  })

  # # 查看分组条件
  # output$tmp456 = renderPrint({merge_by()})

    
  # 根据提供条件进行分组
  merge_by_out = eventReactive(input$button_merge, {
    if(add_phes$click==0){
      choose_group_2merge_tmp = query_tcga_group(cancer=add_phes$cancers,
                                                 custom=NULL,group = ifelse(is.null(add_phes$group),"Code",add_phes$group),
                                                 filter_id = samples(),
                                                 merge_by = merge_by(),
                                                 merge_quantile = input$set_quantile)[['data']]
    } else {
      choose_group_2merge_tmp = query_tcga_group(cancer=add_phes$cancers,
                                                 custom=add_phes_dat(), group = ifelse(is.null(add_phes$group),"Code",add_phes$group),
                                                 filter_id = samples(),
                                                 merge_by = merge_by(),
                                                 merge_quantile = input$set_quantile)[['data']]
    }
    
    
    if(input$reverse_level){
    	group_levels = c(input$group2_name, input$group1_name)
    } else {
    	group_levels = c(input$group1_name, input$group2_name)
    }

    choose_group_2merge_tmp[,add_phes$group] = 
      factor(choose_group_2merge_tmp[,add_phes$group,drop=TRUE],
      				levels = group_levels)  



    choose_group_2merge_tmp$phenotype = colnames(choose_group_2merge_tmp)[4]
    colnames(choose_group_2merge_tmp)[4] = "group"
    choose_group_2merge_tmp$origin = add_phes$phe_primary[,add_phes$group,drop=TRUE][
      match(choose_group_2merge_tmp$Sample,add_phes$phe_primary$Sample)]

    choose_group_2merge_tmp
  })

  observeEvent(input$button_merge,{
    output$choose_group_2merge_out = renderPrint({
      shiny::validate(
        need(try(nrow(merge_by_out())>0), 
          "Please inspect whether to input valid grouping set."),
      )
      if(add_phes$group == unique(merge_by_out()$phenotype)){
        summary(merge_by_out()[,"group"])
      } else {
        cat("NULL, please update above steps.")
      }
    })
  })

  return(merge_by_out)

}


# # A tibble: 6 × 6
#   Sample          Patient      Cancer group phenotype origin
#   <chr>           <chr>        <chr>  <fct> <chr>     <chr> 
# 1 TCGA-A7-A0CE-11 TCGA-A7-A0CE BRCA   NT    Code      NT    
# 2 TCGA-A7-A0CH-11 TCGA-A7-A0CH BRCA   NT    Code      NT    
# 3 TCGA-A7-A0D9-11 TCGA-A7-A0D9 BRCA   NT    Code      NT    
# 4 TCGA-A7-A0DB-11 TCGA-A7-A0DB BRCA   NT    Code      NT    
# 5 TCGA-A7-A0DC-11 TCGA-A7-A0DC BRCA   NT    Code      NT    
# 6 TCGA-A7-A13E-11 TCGA-A7-A13E BRCA   NT    Code      NT 