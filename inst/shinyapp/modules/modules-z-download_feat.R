download_feat_UI = function(id, button_name="Query data"){
	ns = NS(id)
	tagList(
	    shinyWidgets::prettyRadioButtons(
	        inputId = ns("data_L1"), label = "Data type:",
	        choiceValues = c("Molecular_profile", "Tumor_index", "Immune_Infiltration", "Pathway_activity", "Custom_metadata"),
	        choiceNames = c("Molecular profile", "Tumor index", "Immune Infiltration", "Pathway activity", "Custom metadata"),
	        selected = "Molecular_profile"
	    ),
	    tabsetPanel(
		    id = ns("data_L2_tab"),
		    type = "hidden",
		    # selected = "Molecular_profile",
			tabPanel("Molecular_profile", 
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
			tabPanel("Tumor_index",
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
			tabPanel("Immune_Infiltration",
				selectInput(
					ns("immune_infiltration"), "Data subtype:",
					choices = sort(unique(TIL_meta$method)),
					selected = "CIBERSORT"),
	            selectizeInput(
	              inputId = ns("immune_infiltration_id"),
	              label = "Identifier:",
	              choices = NULL)
			),
			tabPanel("Pathway_activity",
				selectInput(
					ns("pathway_activity"), "Data subtype:",
					choices = c("HALLMARK","KEGG","IOBR"),
					selected = "HALLMARK"),
	            selectizeInput(
	              inputId = ns("pathway_activity_id"),
	              label = "Identifier:",
	              choices = NULL)	
			),
			tabPanel("Custom_metadata",
				selectInput(
					ns("custom_metadata"), "Data subtype:",
					choices = c("custom_metadata"),
					selected = "custom_metadata"),
	            selectizeInput(
	              inputId = ns("custom_metadata_id"),
	              label = "Identifier:",
	              choices = NULL)	
			)

		),
		shinyWidgets::actionBttn(
			ns("query_data"), button_name,
	        style = "gradient",
	        icon = icon("search"),
	        color = "primary",
	        block = TRUE,
	        size = "sm"
		)
	)
}



download_feat_Server = function(input, output, session, cancers=NULL, samples=NULL, custom_metadata=NULL, opt_pancan=NULL){
	ns <- session$ns
	observe({
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
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





	download_data = eventReactive(input$query_data, {
		if(input$data_L1 == "Molecular_profile"){
			# 自定义数据集参数
			if(is.null(opt_pancan)){
				opt_pancan = list(
					  toil_mRNA = list(),
					  toil_transcript = list(),
					  toil_protein = list(),
					  toil_mutation = list(),
					  toil_cnv = list(use_thresholded_data = TRUE),
					  toil_methylation = list(type = "450K", aggr = "Q25"),
					  toil_miRNA = list()
				)
			} else {
				opt_pancan = opt_pancan()
			}
			x_genomic_profile = switch(input$genomic_profile,
				`mRNA Expression` = "mRNA",
				`Transcript Expression` = "transcript",
				`DNA Methylation` = "methylation",
				`Protein Expression` = "protein",
				`miRNA Expression` = "miRNA",
				`Mutation status` = "mutation",
				`Copy Number Variation` = "cnv"
			)
			x_data <- query_pancan_value(input$genomic_profile_id, 
									   data_type = x_genomic_profile,
									   opt_pancan = opt_pancan
								       )
			if (is.list(x_data)) x_data <- x_data[[1]]
			x_data <- data.frame(id = input$genomic_profile_id,
							 	 sample = names(x_data), value = as.numeric(x_data),
							 	 level2 = input$genomic_profile)
		} else if (input$data_L1 == "Tumor_index"){
			x_tumor_index = switch(input$tumor_index,
			    `Tumor Purity` = "tcga_purity",
			    `Tumor Stemness` = "tcga_stemness",
			    `Tumor Mutation Burden` = "tcga_tmb",
			    `Microsatellite Instability` = "tcga_msi",
			    `Genome Instability` = "tcga_genome_instability"
			)
			x_data = tumor_index_list[[x_tumor_index]][,c("sample", input$tumor_index_id)]
			colnames(x_data)[2] = "value"
			x_data = x_data %>% 
				dplyr::mutate(id = input$tumor_index_id, .before = 1) %>%
				dplyr::mutate(level2 = input$tumor_index) %>%
				dplyr::filter(!is.na(value))
		} else if (input$data_L1 == "Immune_Infiltration"){
			x_immune_infiltration = input$immune_infiltration
			x_data = tcga_TIL[,c("cell_type",
							paste0(input$immune_infiltration_id,"_",x_immune_infiltration))]
			colnames(x_data) = c("sample","value")
			x_data = x_data %>% 
				dplyr::mutate(id = input$immune_infiltration_id, .before = 1) %>%
				dplyr::mutate(level2 = input$immune_infiltration) %>%
				dplyr::filter(!is.na(value))
		} else if (input$data_L1 == "Pathway_activity"){
			x_pathway_activity = input$pathway_activity
			x_data = tcga_PW[,paste0(x_pathway_activity,"_",input$pathway_activity_id),drop=FALSE]
			colnames(x_data) = "value"
			x_data = x_data %>% as.data.frame() %>%
				tibble::rownames_to_column("sample") %>%
				dplyr::mutate(id = input$pathway_activity_id, .before = 1) %>%
				dplyr::mutate(level2 = input$pathway_activity) %>%
				dplyr::filter(!is.na(value))		
		} else if (input$data_L1 == "Custom_metadata"){
			x_data = custom_metadata()[,c("Sample", input$custom_metadata_id)]
			colnames(x_data) = c("sample","value")
			x_data = x_data %>% as.data.frame() %>%
				dplyr::mutate(id = input$custom_metadata_id, .before = 1) %>%
				dplyr::mutate(level2 = input$custom_metadata) %>%
				dplyr::filter(!is.na(value))		
		}

		x_data2 = x_data %>% 
		  dplyr::mutate(level1 = input$data_L1) %>%
		  dplyr::inner_join(load_data("tcga_clinical")[,c("sample","type")]) %>% 
		  # dplyr::filter(type %in% cancer_choose$name) %>% 
		  dplyr::select(id, level1, level2, sample, value, type) %>%
		  dplyr::rename(cancer = type) %>%
		  dplyr::arrange(cancer,sample)

		# if(!is.null(cancer_choose$filter_phe_id)){
		# 	x_data2 = x_data2 %>% 
		# 		dplyr::filter(sample %in% cancer_choose$filter_phe_id)
		# }
		if(!is.null(cancers)){
			x_data2 = x_data2 %>%
				dplyr::filter(cancer %in% cancers())
		}
		if(!is.null(samples)){
			if(!is.null(samples())){
				x_data2 = x_data2 %>%
					dplyr::filter(sample %in% samples())

			}
		}
		x_data2
	})
	return(download_data)
}


#     id            level1          level2          sample value cancer
# 1 TP53 Molecular_profile mRNA Expression TCGA-3C-AAAU-01 5.445   BRCA
# 2 TP53 Molecular_profile mRNA Expression TCGA-3C-AALI-01 3.446   BRCA
# 3 TP53 Molecular_profile mRNA Expression TCGA-3C-AALJ-01 4.946   BRCA
# 4 TP53 Molecular_profile mRNA Expression TCGA-3C-AALK-01 5.107   BRCA
# 5 TP53 Molecular_profile mRNA Expression TCGA-4H-AAAK-01 5.440   BRCA
# 6 TP53 Molecular_profile mRNA Expression TCGA-5L-AAT0-01 5.239   BRCA
