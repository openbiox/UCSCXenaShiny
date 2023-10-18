group_samples_UI = function(id, button_name="Filter by multi-conditions"){
	ns = NS(id)
	tagList(
		h4("1. Add phenotypes(optional)"),

		fluidRow(
			column(
				6,
		        selectInput(
		        	ns("data_L1"), "Data type:",
		        	choices = c("Molecular profile", "Tumor index", "Immune Infiltration", "Pathway activity"),
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

	    h4("2. Select/Observe a phenotype"),
		uiOutput(ns("filter_phe_01_by.ui")),
		verbatimTextOutput(ns("filter_phe_01_out_2")),

	    h4("3. Merge/Split groups"),
		fluidRow(
			column(6,
				textInput(ns("spliting_item"),"(1) Threshold:", value = "NT||TM|TP")
			),
			column(6,
				textInput(ns("splitting_name"),"(2) Group names:", value = "Normal||Tumor")
			),
		),
        fluidRow(
            actionBttn(
              inputId = ns("button_merge"),
              label = "M/S",
              color = "primary",
              style = "bordered", size = "sm",
              block = F
            ),
            actionBttn(
              inputId = ns("button_merge_reset"),
              label = "Reset",
              color = "primary",
              style = "bordered", size = "sm",
              block = F
            ),
            actionBttn(
              inputId = ns("button_merge_help"),
              label = "Help",
              color = "primary",
              style = "bordered", size = "sm",
              block = F
            )
        ),
		verbatimTextOutput(ns("choose_group_2merge_out")),

		h4("4. Extract 2 groups"),
		uiOutput(ns("levels_orders.ui")),

        actionBttn(
          inputId = ns("button_extract"),
          label = "Run",
          color = "primary",
          style = "bordered", size = "sm",
          block = F
        ),
		textOutput(ns("choose_group_2levels_out"))


	)
}


group_samples_Server = function(input, output, session, cancers=NULL, samples=NULL){
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
	})

	add_level2 = reactive({
	  switch(input$data_L1,
	    `Molecular profile` = input$genomic_profile,
	    `Tumor index` = input$tumor_index,
	    `Immune Infiltration` = input$immune_infiltration,
	    `Pathway activity` = input$pathway_activity
	  )
	})
	add_level3 = reactive({
	  switch(input$data_L1,
	    `Molecular profile` = input$genomic_profile_id,
	    `Tumor index` = input$tumor_index_id,
	    `Immune Infiltration` = input$immune_infiltration_id,
	    `Pathway activity` = input$pathway_activity_id
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
			paste0(input$data_L1,"-",add_level2(),"-",add_level3()))
		add_phes$label = c(add_phes$label, 
			paste0("Add_",add_phes$click))
		add_phes$show = paste0(add_phes$show,
			"Add_",add_phes$click,": ", input$data_L1,"-",add_level2(),
			"-",add_level3(),"\n")
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
			tmp_data_type = str_split(add_phes$name[[i]], "-")[[1]][1]
			tmp_data_sub = str_split(add_phes$name[[i]], "-")[[1]][2]
			tmp_data_target = str_split(add_phes$name[[i]], "-")[[1]][3]

			if(tmp_data_type == "Molecular profile"){
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
										   data_type = x_genomic_profile)
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
				return_all = T)
		} else {
			add_phes$phe_primary = query_tcga_group(
				cancer = add_phes$cancers, custom = add_phes_dat(),
				return_all = T)
		}
		add_phes$group = input$filter_phe_01_by_2
	})

	output$filter_phe_01_out_2 = renderPrint({
		filter_phe_01_out_2_tmp = add_phes$phe_primary %>%
			as.data.frame()

		if(!is.null(samples)){
			if(!is.null(samples())){
				filter_phe_01_out_2_tmp = filter_phe_01_out_2_tmp %>% 
					dplyr::filter(Sample %in% samples())
			}
		} 
		filter_phe_01_out_2_tmp = filter_phe_01_out_2_tmp[,add_phes$group,drop=FALSE]

		filter_phe_01_out_2_tmp = filter_phe_01_out_2_tmp %>% 
			mutate(across(where(is.character), as.factor))
		summary(filter_phe_01_out_2_tmp)
	})


	# 提取分组

	observeEvent(input$button_merge_help, {
	  showModal(
	  	modalDialog(
			title = "How to merge/split groups",
			footer = modalButton("Got it!"),
			size = "l",
			p(strong("Notions:")),
			p("(1) Threshold(s) : how to make groups for the selected phenotype in above step"),
			p("(1.1) For categorical phenotype, categories are sepetated by | or ||. Groups are merged by two sides of ||"),
			p("(1.2) For numerical phenotype,  two or more cutoff in order are sepetated by || to split groups"),
			p("(2) Group names(s) : how to name the new groups corresponding to step(1)"),

			br(),
			p(strong("Examples:")),
			p("'Code' (1) NT||TM|TP; (2) Normal||Tumor: NT → Normal, TM+TP → Tumor"),
			p("'Age' (1) 40||60||60; (2) Young||Middle||Old: <40 → Young, 40~60 → Middle, >60 → Old"),

			br(),
			p(strong("Other:")),
			p("When (1) Threshold(s) is empty (Reset button), it would execute default operations"),
			p("For categorical phenotype, original categories are directly kept"),
			p("For numerical phenotype,  the overall median value is adopted to splited higer/lower groups"),










	  	)
	  )
	})


	observeEvent(input$button_merge_reset, {
	  updateTextInput(session, "spliting_item", value = "")
	  updateTextInput(session, "splitting_name", value = "")
	})


	merge_by = eventReactive(input$button_merge, {
		if(input$spliting_item==""){
			# default
			if(class(add_phes$phe_primary[,add_phes$group,drop=TRUE])=="character"){
				merge_by = NULL
			} else {
				merge_by = list("lower"=median(add_phes$phe_primary[,add_phes$group,drop=TRUE],na.rm = T),
								"higher"=median(add_phes$phe_primary[,add_phes$group,drop=TRUE],na.rm = T))
			} 
		} else {
			merge_by = lapply(strsplit(input$spliting_item,"||",fixed = T)[[1]], function(x){
			    strsplit(x, "|", fixed = T)[[1]]
			})
			if(length(merge_by)<length(strsplit(input$splitting_name,"||",fixed = T)[[1]])){
			  merge_by = lapply(merge_by, as.numeric)
			  merge_by[[length(merge_by)+1]] = merge_by[[2]]
			}
			names(merge_by) = strsplit(input$splitting_name,"||",fixed = T)[[1]]
		}
		merge_by
	})

	# 观察所选分组的分布
	merge_by_out = eventReactive(input$button_merge, {
		if(add_phes$click==0){
			choose_group_2merge_tmp = query_tcga_group(cancer=add_phes$cancers,
				custom=NULL,group = ifelse(is.null(add_phes$group),"Code",add_phes$group),
				merge_by = merge_by())[['data']]
		} else {
			choose_group_2merge_tmp = query_tcga_group(cancer=add_phes$cancers,
				custom=add_phes_dat(), group = ifelse(is.null(add_phes$group),"Code",add_phes$group),
				merge_by = merge_by())[['data']]
		}

		if(!is.null(samples)){
			if(!is.null(samples())){
				choose_group_2merge_tmp = choose_group_2merge_tmp %>% 
					dplyr::filter(Sample %in% samples())
			}
		} 
		# choose_group_2merge_tmp = choose_group_2merge_tmp[,add_phes$group]
		# choose_group_2merge_tmp = choose_group_2merge_tmp %>% 
		# 	mutate(across(where(is.character), as.factor))

		choose_group_2merge_tmp[,add_phes$group] = 
			factor(choose_group_2merge_tmp[,add_phes$group,drop=TRUE])  


		# summary(choose_group_2merge_tmp)
		choose_group_2merge_tmp
	})

	output$choose_group_2merge_out = renderPrint({summary(merge_by_out()[,add_phes$group])})


	output$levels_orders.ui = renderUI({
		fluidRow(
			column(
				6,
				checkboxGroupInput(
					ns("levels_orders"), NULL,
					choices = sort(unique(merge_by_out()[,add_phes$group,drop=T])),
					selected = head(sort(unique(merge_by_out()[,add_phes$group,drop=T])),2),
					inline=TRUE
				)	
			),
			column(6, materialSwitch(ns("reverse_level"), "Adjust orders"))
		)
	})



	group_final = eventReactive(input$button_extract,{
		merge_by_out2 = merge_by_out()
		merge_by_out2 = merge_by_out2[
				merge_by_out2[,add_phes$group,drop=TRUE] %in% input$levels_orders,]


		raw_levels = sort(input$levels_orders)

		if(input$reverse_level){
			merge_by_out2[,add_phes$group,drop=TRUE] = 
				factor(merge_by_out2[,add_phes$group,drop=TRUE], 
					   levels = rev(raw_levels))  
		} else {
			merge_by_out2[,add_phes$group,drop=TRUE] = 
				factor(merge_by_out2[,add_phes$group,drop=TRUE], 
					   levels = raw_levels)  
		}
		merge_by_out2$phenotype = colnames(merge_by_out2)[4]
		colnames(merge_by_out2)[4] = "group"
		merge_by_out2
	})

	choose_group_2levels_out = eventReactive(input$button_extract,{
		level_list = as.list(table(group_final()[,"group",drop=T]))


		paste0("NOTE: Two groups for ",add_phes$group,": ",
				paste(paste0(names(level_list),"(",unlist(level_list),")"),collapse = ", "))
		# summary(group_final()[,add_phes$group])
	})
	output$choose_group_2levels_out = renderText({choose_group_2levels_out()})
	return(group_final)
}


# # A tibble: 6 × 5
#   Sample          Patient      Cancer group phenotype
#   <chr>           <chr>        <chr>  <fct> <chr>    
# 1 TCGA-3C-AAAU-01 TCGA-3C-AAAU BRCA   Tumor Code     
# 2 TCGA-3C-AALI-01 TCGA-3C-AALI BRCA   Tumor Code     
# 3 TCGA-3C-AALJ-01 TCGA-3C-AALJ BRCA   Tumor Code     
# 4 TCGA-3C-AALK-01 TCGA-3C-AALK BRCA   Tumor Code     
# 5 TCGA-4H-AAAK-01 TCGA-4H-AAAK BRCA   Tumor Code     
# 6 TCGA-5L-AAT0-01 TCGA-5L-AAT0 BRCA   Tumor Code     