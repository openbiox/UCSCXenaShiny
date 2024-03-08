download_feat_UI = function(id, button_name="Query data", database = "toil"){
	ns = NS(id)

	id_option = switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)

	tagList(
		# shinyWidgets::actionBttn(
		# 	ns("query_data"), button_name,
	 #        style = "gradient",
	 #        icon = icon("search"),
	 #        color = "primary",
	 #        block = TRUE,
	 #        size = "sm"
		# ),
		# fluidRow(
		# 	column(10, offset = 1,
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
			            virtualSelectInput(
			              inputId = ns("genomic_profile_id"),
			              label = "Identifier:",
			              choices = NULL,
			              search = TRUE,
			              allowNewOption = TRUE,
			              dropboxWidth = "200%")
					),
					tabPanel("Tumor index",
			            virtualSelectInput(
			              inputId = ns("tumor_index_id"),
			              label = "Identifier:",
			              choices = NULL,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Immune Infiltration",
			            virtualSelectInput(
			              inputId = ns("immune_infiltration_id"),
			              label = "Identifier:",
			              choices = NULL,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Pathway activity",
			            virtualSelectInput(
			              inputId = ns("pathway_activity_id"),
			              label = "Identifier:",
			              choices = NULL,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Phenotype data",
			            virtualSelectInput(
			              inputId = ns("phenotype_data_id"),
			              label = "Identifier:",
			              choices = NULL,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					)
				),
				shinyWidgets::actionBttn(
					ns("query_data"), button_name,
			        style = "gradient",
			        icon = icon("search"),
			        color = "primary",
			        block = TRUE,
			        size = "sm"
				),
		# 	),
		# ),

		# br(),
		div(uiOutput(ns("x_axis_data_table")),
			style = "margin-top: 5px; margin-bottom: 0px;"
		)
	)
}



download_feat_Server = function(input, output, session, database = "toil",#id_option=tcga_id_option,
								samples=NULL, custom_metadata=NULL, opt_pancan=NULL, check_numeric=FALSE, table.ui=TRUE){
	ns <- session$ns
	# id_option = tcga_id_option
	id_option = switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)
	id_category = lapply(id_option, names)

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
	  updateVirtualSelect(
	    "genomic_profile_id",
	    choices = genomic_profile_choices()$all,
	    selected = genomic_profile_choices()$default
	  )
	  updateVirtualSelect(
	    "tumor_index_id",
	    choices = tumor_index_choices()$all,
	    selected = tumor_index_choices()$default
	  )
	  updateVirtualSelect(
	    "immune_infiltration_id",
	    choices = immune_infiltration_choices()$all,
	    selected = immune_infiltration_choices()$default
	  )
	  updateVirtualSelect(
	    "pathway_activity_id",
	    choices = pathway_activity_choices()$all,
	    selected = pathway_activity_choices()$default
	  )
	  updateVirtualSelect(
	    "phenotype_data_id",
	    choices = phenotype_data_choices()$all,
	    selected = phenotype_data_choices()$default
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
		L1_x = names(id_category)[sapply(id_category, function(x){any(x %in% L2_x)})]
		if(is.null(opt_pancan)){
			opt_pancan = .opt_pancan
		} else {
			opt_pancan = opt_pancan()
		}
		## 利用内部自定义下载函数获取数据
		if(database=="toil"){
			clinical_phe = tcga_phenotype_value[["Clinical Phenotype"]]
			x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
						   tcga_index_value, tcga_immune_value, tcga_pathway_value, 
						   clinical_phe,
						   opt_pancan,custom_metadata())
		} else if(database=="pcawg"){
			clinical_phe = pcawg_phenotype_value[["Clinical Phenotype"]]
			x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
						   # pcawg_index_list, pcawg_TIL, pcawg_PW, pcawg_info_fine,
						   pcawg_index_value, pcawg_immune_value, pcawg_pathway_value,
						   clinical_phe,
						   opt_pancan,custom_metadata())
		} else if (database=="ccle"){
			clinical_phe = ccle_phenotype_value[["Clinical Phenotype"]]
			x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
						   # ccle_index_list, NULL, NULL, ccle_info_fine,
						   ccle_index_value, NULL, NULL, 
						   clinical_phe,
						   opt_pancan,custom_metadata())
		}
		if(!is.null(samples)){
			if(!is.null(samples())){
				x_data = x_data %>%
					dplyr::filter(Sample %in% samples())
			}
		}
		x_data$level1 = L1_x
		## 这里PCAWG应该是Project，但为了统一起见，先都叫cancer，包括后面的ccle
		x_data$cancer = clinical_phe[,2,drop=T][match(x_data$Sample, clinical_phe$Sample)]
		x_data = x_data[,c("id","level1","level2","Sample","value","cancer")] %>%
			dplyr::arrange(cancer,Sample)
		x_data
	})

	w <- waiter::Waiter$new(id = ns("x_axis_data_table"), html = waiter::spin_hexdots(), color = "black")

	observeEvent(input$query_data,{
		w$show()
		output$x_axis_data_table = renderUI({
			if(table.ui){
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
					x_axis_data_ = download_data()[,c("Sample","value","cancer")]

					if(class(x_axis_data_[,"value"])=="numeric"){
						x_axis_data_[,"value"] = round(x_axis_data_[,"value"], digits = 3)
					}
					datatable(x_axis_data_, 
						options = list(pageLength = 3,
							columnDefs = list(list(className = 'dt-center', targets="_all")))
					)
				}) 
				dataTableOutput(ns("x_tmp_table"))
			} else {
				output$x_tmp_table = renderPrint({
					# ids_num = nrow()
					cat(paste0("Tip: identifier values of ", nrow(download_data()), " samples are prepared."))
				})
				verbatimTextOutput(ns("x_tmp_table"))
			}

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


#     id            level1          level2          Sample value cancer
# 1 TP53 Molecular profile mRNA Expression TCGA-3C-AAAU-01 5.445   BRCA
# 2 TP53 Molecular profile mRNA Expression TCGA-3C-AALI-01 3.446   BRCA
# 3 TP53 Molecular profile mRNA Expression TCGA-3C-AALJ-01 4.946   BRCA
# 4 TP53 Molecular profile mRNA Expression TCGA-3C-AALK-01 5.107   BRCA
# 5 TP53 Molecular profile mRNA Expression TCGA-4H-AAAK-01 5.440   BRCA
# 6 TP53 Molecular profile mRNA Expression TCGA-5L-AAT0-01 5.239   BRCA
