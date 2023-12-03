download_feat_UI = function(id, button_name="Query data",id_option = tcga_id_option){
	ns = NS(id)
	tagList(
		shinyWidgets::actionBttn(
			ns("query_data"), button_name,
	        style = "gradient",
	        icon = icon("search"),
	        color = "primary",
	        block = TRUE,
	        size = "sm"
		),
	    fluidRow(
	    	column(
	    		6,
			    selectInput(
			    	ns("data_L1"), label = "Data type:",
			    	choices = names(id_option),
			    	selected = "Molecular profile"
			    )
	    	),
	    	column(
	    		6,
			    tabsetPanel(
				    id = ns("data_L2_tab"),
				    type = "hidden",
					tabPanel("Molecular profile", 
						selectInput(
							ns("genomic_profile"), "Data subtype:",
							choices = names(id_option[["Molecular profile"]]),
							selected = "mRNA Expression")
					),
					tabPanel("Tumor index",
						selectInput(
							ns("tumor_index"), "Data subtype:",
							choices = names(id_option[["Tumor index"]]),
							selected = "Tumor Purity")
					),
					tabPanel("Immune Infiltration",
						selectInput(
							ns("immune_infiltration"), "Data subtype:",
							choices = names(id_option[["Immune Infiltration"]]),
							selected = "CIBERSORT")
					),
					tabPanel("Pathway activity",
						selectInput(
							ns("pathway_activity"), "Data subtype:",
							choices = names(id_option[["Pathway activity"]]),
							selected = "HALLMARK")
					),
					tabPanel("Phenotype data",
						selectInput(
							ns("phenotype_data"), "Data subtype:",
							choices = names(id_option[["Phenotype data"]]),
							selected = "Clinical Phenotye")
					)
				)
	    	)
	    ),
	    tabsetPanel(
		    id = ns("data_L3_tab"),
		    type = "hidden",
			tabPanel("Molecular profile",
	            selectizeInput(
	              inputId = ns("genomic_profile_id"),
	              label = "Identifier:",
	              choices = NULL,
	              options = list(create = TRUE, maxOptions = 10))
			),
			tabPanel("Tumor index",
	            selectizeInput(
	              inputId = ns("tumor_index_id"),
	              label = "Identifier:",
	              choices = NULL,
	              options = list(create = FALSE, maxOptions = 10))
			),
			tabPanel("Immune Infiltration",
	            selectizeInput(
	              inputId = ns("immune_infiltration_id"),
	              label = "Identifier:",
	              choices = NULL,
	              options = list(create = FALSE, maxOptions = 10))
			),
			tabPanel("Pathway activity",
	            selectizeInput(
	              inputId = ns("pathway_activity_id"),
	              label = "Identifier:",
	              choices = NULL,
	              options = list(create = FALSE, maxOptions = 10))
			),
			tabPanel("Phenotype data",
	            selectizeInput(
	              inputId = ns("phenotype_data_id"),
	              label = "Identifier:",
	              choices = NULL,
	              options = list(create = FALSE, maxOptions = 10))
			)
		),
		uiOutput(ns("x_axis_data_table"))
	)
}



download_feat_Server = function(input, output, session, cohort = "TOIL",id_option=tcga_id_option,
								samples=NULL, custom_metadata=NULL, opt_pancan=NULL, check_numeric=FALSE){
	ns <- session$ns
	observe({
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
	}) 

	genomic_profile_choices <- reactive({
	  id_option[["Molecular profile"]][[input$genomic_profile]]
	})

	tumor_index_choices <- reactive({
	  id_option[["Tumor index"]][[input$tumor_index]]
	})

	immune_infiltration_choices <- reactive({
	  id_option[["Immune Infiltration"]][[input$immune_infiltration]]
	})

	pathway_activity_choices <- reactive({
	  id_option[["Pathway activity"]][[input$pathway_activity]]
	})


	phenotype_data_choices <- reactive({
	    id_tmp = id_option[["Phenotype data"]]
		if(!is.null(custom_metadata)){
			id_tmp[["Custom metadata"]]$all = sort(colnames(custom_metadata()[-1]))
			id_tmp[["Custom metadata"]]$default = sort(colnames(custom_metadata()[-1]))[1]
		}
		id_tmp[[input$phenotype_data]]
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
	    "phenotype_data_id",
	    choices = phenotype_data_choices()$all,
	    selected = phenotype_data_choices()$default,
	    server = TRUE
	  )
	})


	download_data = eventReactive(input$query_data, {
		L2_x = switch(input$data_L1,
		    `Molecular profile` = input$genomic_profile,
		    `Tumor index` = input$tumor_index,
		    `Immune Infiltration` = input$immune_infiltration,
		    `Pathway activity` = input$pathway_activity,
		    `Phenotype data` = input$phenotype_data
		)
		L3_x = switch(input$data_L1,
		    `Molecular profile` = input$genomic_profile_id,
		    `Tumor index` = input$tumor_index_id,
		    `Immune Infiltration` = input$immune_infiltration_id,
		    `Pathway activity` = input$pathway_activity_id,
		    `Phenotype data` = input$phenotype_data_id
		)
		id_category = lapply(id_option, names)
		L1_x = names(id_category)[sapply(id_category, function(x){any(x %in% L2_x)})]
		if(is.null(opt_pancan)){
			# opt_pancan = list(
			# 	  toil_mRNA = list(),
			# 	  toil_transcript = list(),
			# 	  toil_protein = list(),
			# 	  toil_mutation = list(),
			# 	  toil_cnv = list(use_thresholded_data = TRUE),
			# 	  toil_methylation = list(type = "450K", aggr = "NA", rule_out = NULL),
			# 	  toil_miRNA = list(),
			# 	  pcawg_mRNA = list(),
			# 	  pcawg_fusion = list(),
			# 	  pcawg_miRNA = list(norm_method = "TMM"),
			# 	  pcawg_promoter = list(type = "relative"),
			# 	  pcawg_APOBEC = list()
			# )
			opt_pancan = .opt_pancan
		} else {
			opt_pancan = opt_pancan()
		}
		## 利用内部自定义下载函数获取数据
		if(cohort=="TOIL"){
			clinical_phe = tcga_clinical_fine
			x_data = UCSCXenaShiny:::batch_download(L1_x, L2_x, L3_x, cohort,
						   tumor_index_list, tcga_TIL, tcga_PW, tcga_clinical_fine,
						   opt_pancan,custom_metadata())
		} else if(cohort=="PCAWG"){
			clinical_phe = pcawg_info_fine
			x_data = UCSCXenaShiny:::batch_download(L1_x, L2_x, L3_x, cohort,
						   pcawg_index_list, pcawg_TIL, pcawg_PW, pcawg_info_fine,
						   opt_pancan,custom_metadata())
		} else if (cohort=="CCLE"){
			clinical_phe = ccle_info_fine
			x_data = UCSCXenaShiny:::batch_download(L1_x, L2_x, L3_x, cohort,
						   ccle_index_list, NULL, NULL, ccle_info_fine,
						   opt_pancan,custom_metadata())
		}
		if(!is.null(samples)){
			if(!is.null(samples())){
				x_data = x_data %>%
					dplyr::filter(sample %in% samples())
			}
		}
		x_data$level1 = L1_x
		## 这里PCAWG应该是Project，但为了统一起见，先都叫cancer，包括后面的ccle
		x_data$cancer = clinical_phe[,2,drop=T][match(x_data$sample, clinical_phe$Sample)]
		x_data = x_data[,c("id","level1","level2","sample","value","cancer")] %>%
			dplyr::arrange(cancer,sample)
		x_data
	})

	observeEvent(input$query_data,{
		output$x_axis_data_table = renderUI({
			output$x_tmp_table = renderDataTable({
				shiny::validate(
					need(try(nrow(download_data())>0), 
						"No sample data were available. Please inspect operations in Preset step."),
				)
				if(check_numeric){
					shiny::validate(
						need(try(class(download_data()$value)!="character"), 
							"Please select a numeric variable."),
					)	
				}
				x_axis_data_ = download_data()[,c("sample","value","cancer")]

				if(class(x_axis_data_[,"value"])=="numeric"){
					x_axis_data_[,"value"] = round(x_axis_data_[,"value"], digits = 3)
				}
				datatable(x_axis_data_, 
					options = list(pageLength = 3,
						columnDefs = list(list(className = 'dt-center', targets="_all")))
				)
				# download_data()
			}) 
			dataTableOutput(ns("x_tmp_table"))
		})
	})

	# observeEvent(input$query_data,{
	# 	if(input$query_data==1){
	# 	  shinyalert(
	# 	    title = "Clicked",
	# 	    text = paste("You query has been uploaded successfully.\n",
	# 	    	"Please wait a moment for Molecular profile type due to network.\n",
	# 	    	"The notion will only appear once."),
	# 	    size = "s", 
	# 	    closeOnEsc = TRUE,
	# 	    closeOnClickOutside = FALSE,
	# 	    html = FALSE,
	# 	    type = "success",
	# 	    showConfirmButton = TRUE,
	# 	    showCancelButton = FALSE,
	# 	    confirmButtonText = "OK",
	# 	    confirmButtonCol = "#AEDEF4",
	# 	    timer = 0,
	# 	    imageUrl = "",
	# 	    animation = TRUE
	# 	  )
	# 	}
	# })
	return(download_data)
}


#     id            level1          level2          sample value cancer
# 1 TP53 Molecular profile mRNA Expression TCGA-3C-AAAU-01 5.445   BRCA
# 2 TP53 Molecular profile mRNA Expression TCGA-3C-AALI-01 3.446   BRCA
# 3 TP53 Molecular profile mRNA Expression TCGA-3C-AALJ-01 4.946   BRCA
# 4 TP53 Molecular profile mRNA Expression TCGA-3C-AALK-01 5.107   BRCA
# 5 TP53 Molecular profile mRNA Expression TCGA-4H-AAAK-01 5.440   BRCA
# 6 TP53 Molecular profile mRNA Expression TCGA-5L-AAT0-01 5.239   BRCA
