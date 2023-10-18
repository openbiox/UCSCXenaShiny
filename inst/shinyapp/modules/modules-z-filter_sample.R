filter_samples_UI = function(id, button_name="Filter by multi-conditions"){
	ns = NS(id)
	tagList(
		shinyWidgets::actionBttn(
			ns("filter_phe"), button_name,
	        style = "gradient",
	        icon = icon("search"),
	        color = "primary",
	        block = TRUE,
	        size = "sm"
		)
	)
}



filter_samples_Server = function(input, output, session, cancers=NULL){
	ns <- session$ns


	observeEvent(input$filter_phe, {
		# message("filter samples by one/multiple phenotype(s)")
		showModal(
			modalDialog(
				title = "Filter samples by one/multiple phenotype(s)",
				footer = modalButton("Done!"),
				size = "l",
				fluidPage(
			        wellPanel(
				        h4("1. Add phenotypes (Optional)"),
				        selectInput(
				        	ns("data_L1"), "Data type:",
				        	choices = c("Molecular profile", "Tumor index", "Immune Infiltration", "Pathway activity"),
				        	selected = "Molecular profile"
				        ),
						tabsetPanel(
							    id = ns("data_L2_tab"),
							    type = "hidden",
							    # selected = "Molecular_profile",
								tabPanel("Molecular profile", 
									selectInput(
										ns("genomic_profile"), "Data subtype:",
										choices = c("mRNA Expression", "Transcript Expression", "DNA Methylation", 
													"Protein Expression", "miRNA Expression", "Mutation status","Copy Number Variation"),
										selected = "mRNA Expression"),
						            selectizeInput(
						              inputId = ns("genomic_profile_id"),
						              label = "Identifier:",
						              choices = NULL,
						              options = list(create = TRUE, maxOptions = 5))
								),
								tabPanel("Tumor index",
									selectInput(
										ns("tumor_index"), "Data subtype:",
										choices = c("Tumor Purity","Tumor Stemness","Tumor Mutation Burden",
													"Microsatellite Instability","Genome Instability"),
										selected = "Tumor Purity"),
						            selectizeInput(
						              inputId = ns("tumor_index_id"),
						              label = "Identifier:",
						              choices = NULL)
								),
								tabPanel("Immune Infiltration",
									selectInput(
										ns("immune_infiltration"), "Data subtype:",
										choices = sort(unique(TIL_meta$method)),
										selected = "CIBERSORT"),
						            selectizeInput(
						              inputId = ns("immune_infiltration_id"),
						              label = "Identifier:",
						              choices = NULL)
								),
								tabPanel("Pathway activity",
									selectInput(
										ns("pathway_activity"), "Data subtype:",
										choices = c("HALLMARK","KEGG","IOBR"),
										selected = "HALLMARK"),
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
					    verbatimTextOutput(ns("add_info"))
			        ),
			        wellPanel(
			        	h4("2. Obsever distribution"),
						uiOutput(ns("filter_phe_01_by.ui")),
						verbatimTextOutput(ns("filter_phe_01_out"))
					),
				    wellPanel(
					    h4("3. Set conditions"),
					    fluidRow(
					    	column(4,
					    		textInput(ns("filter_phe_02_by"),"(1) Phenotype(s):", value = "Code")),
					    	column(4,
					    		textInput(ns("filter_phe_02_cutoff"),"(2) Threshold(s):", value = "TP")),
					    	column(4,
					    		textInput(ns("filter_phe_02_direct"),"(3) Direction(s):", value = "+")),
					    ),
					    fluidRow(
						    actionBttn(
						      inputId = ns("button_phe_filter"),
						      label = "Run",
						      color = "primary",
						      style = "bordered", size = "sm",
						      block = F
						    ),
						    actionBttn(
						      inputId = ns("button_phe_filter_reset"),
						      label = "Reset",
						      color = "primary",
						      style = "bordered", size = "sm",
						      block = F
						    )       
					    ),
						textOutput(ns("filter_phe_02_out")),
						br(),
						p(strong("Notions:")),
						p("(1) Phenotype(s) : choose one or multiple phenotypes (seperated by ||), which is/are in above choices."),
						p("(2) Threshold(s) : choose the respective thresholds (seperated by || for multiple phenotypes) according to above distributions."),
						p("(2.1) For categorical phenotype, please input one or multiple phenotypes (seperated by |);"),
						p("(2.2) For numerical phenotype, please input one cutoff."),	
						p("(3) Direction(s) : set the respective logical symbol (seperated by || for multiple phenotypes)."),
						p("(3.1) For discrete phenotype, set '+' or '-' to retain or discard samples"),
						p("(3.2) For numerical phenotype, set '>' or '<' to retain samples."),	
						br(),
						p(strong("Examples:")),
						p("(1) Code; (2) TP; (3) + : retain all tumor samples."),
						p("(1) Code||Age; (2) TP|| 60; (3) + | <; : retain  tumor samples with age < 60."),
						p("(1) Stage_ajcc; (2) Stage I | Stage II, (3) - : discard samples in Stage I or II of Stage_ajcc."),
						p("(1) Add_1; (2) NA, (3) - : discard samples with NA value for Add_1 phenotype.")
					)

		        )
		    )
		)
		# 更新分子的三级选择菜单
		observe({
		  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
		}) 
		observeEvent(input$genomic_profile, {
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
		    updateSelectizeInput(
		      session,
		      "genomic_profile_id",
		      choices = genomic_profile_choices()$all,
		      selected = genomic_profile_choices()$default,
		      server = TRUE
		    )
		})
		observeEvent(input$tumor_index, {
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
		    updateSelectizeInput(
		      session,
		      "tumor_index_id",
		      choices = tumor_index_choices()$all,
		      selected = tumor_index_choices()$default,
		      server = TRUE
		    )
		})
		observeEvent(input$immune_infiltration, {
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
		    updateSelectizeInput(
		      session,
		      "immune_infiltration_id",
		      choices = immune_infiltration_choices()$all,
		      selected = immune_infiltration_choices()$default,
		      server = TRUE
		    )
		})
		observeEvent(input$pathway_activity, {
			pathway_activity_choices <- reactive({
			  switch(input$pathway_activity,
			    `HALLMARK` = list(all = sort(PW_meta$Name[PW_meta$Type=="HALLMARK"]), default = "APOPTOSIS"),
			    `KEGG` = list(all = sort(PW_meta$Name[PW_meta$Type=="KEGG"]), default = "CELL_CYCLE"),
			    `IOBR` = list(all = sort(PW_meta$Name[PW_meta$Type=="IOBR"]), default = "Biotin_Metabolism"),
			    list(all = "NONE", default = "NONE")
			  )
			})
		    updateSelectizeInput(
		      session,
		      "pathway_activity_id",
		      choices = pathway_activity_choices()$all,
		      selected = pathway_activity_choices()$default,
		      server = TRUE
		    )
		})



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


	add_phes <- reactiveValues(cancers = "ACC",click=0, name = list(), label=list(), show="")

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


	## step2观察表型
	output$filter_phe_01_by.ui = renderUI({
		selectInput(ns("filter_phe_01_by"), NULL,
			choices = colnames(add_phes$phe_primary)[-1:-2], selected = "Code"
		)
	})
	# 初始表型/添加表型
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
	})
	# 观察当前表型的分布
	output$filter_phe_01_out = renderPrint({

		filter_phe_01_out_tmp = as.data.frame(add_phes$phe_primary[,input$filter_phe_01_by])
		head(filter_phe_01_out_tmp)
		filter_phe_01_out_tmp = filter_phe_01_out_tmp %>% 
			mutate(across(where(is.character), as.factor))
		summary(filter_phe_01_out_tmp)
	})

	# filter--phe--step3
	# 重置筛选条件
	observeEvent(input$button_phe_filter_reset, {
	  updateTextInput(session, "filter_phe_02_by", value = "Code")
	  updateTextInput(session, "filter_phe_02_cutoff", value = "TP")
	  updateTextInput(session, "filter_phe_02_direct", value = "+")
	  add_phes$filter_phe_id = NULL
	})
	# 提取筛选条件
	filter_by_phe = eventReactive(input$button_phe_filter, {
		if(input$filter_phe_02_by==""){
			NULL
		} else {
			lapply(seq(length(strsplit(input$filter_phe_02_by,"||",fixed = T)[[1]])), function(i){
						  c(strsplit(input$filter_phe_02_by,"||",fixed = T)[[1]][i],
						    strsplit(input$filter_phe_02_cutoff,"||",fixed = T)[[1]][i],
						    strsplit(input$filter_phe_02_direct,"||",fixed = T)[[1]][i])
						})
		}
	})

	# 获取筛选后的样本ID
	observe({
		if(add_phes$click==0){
			add_phes$filter_phe_id = query_tcga_group(cancer = add_phes$cancers,
				filter_by = filter_by_phe(),)[["data"]]$Sample
		} else {
			add_phes$filter_phe_id = query_tcga_group(cancer = add_phes$cancers,
				custom = add_phes_dat(), filter_by = filter_by_phe())[["data"]]$Sample
		}
		output$filter_phe_02_out = renderText({
			if(is.null(add_phes$filter_phe_id)){
				paste0("No sample was filtered.")
			} else {
				paste0(length(add_phes$filter_phe_id), " samples were left.")
			}		
		})
	})
	return(reactive(add_phes$filter_phe_id))


}