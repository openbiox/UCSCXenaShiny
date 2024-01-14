multi_upload_UI = function(id, button_name = "Query data(x-axis)", database = "toil"){
	ns = NS(id)

	id_option = switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)

	tagList(
		# shinyWidgets::actionBttn(
		# 	ns("inspect_data_x"), button_name,
	 #        style = "gradient",
	 #        icon = icon("search"),
	 #        color = "primary",
	 #        block = TRUE,
	 #        size = "sm"
		# ),

		# 选择major/minor type
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

		prettyRadioButtons(ns("L3_x_type"),"Choose multi-ids by",
			choices = c("Selection","All","File"), selected = "Selection", inline=TRUE),
			 # %>% 
				# helper(type = "markdown", sie = "m", fade = TRUE,
				# 		title = "Notes for IDs selction", content = "batch_ids"),
		tabsetPanel(id = ns("L3_x_type_tab"),
			type = "hidden",
			tabPanel("Selection",
			    tabsetPanel(
				    id = ns("data_L3_tab"),
				    type = "hidden",
					tabPanel("Molecular profile",
			            virtualSelectInput(
			              inputId = ns("genomic_profile_id"),
			              label = NULL,
			              choices = NULL, multiple = TRUE,
			              search = TRUE,
			              allowNewOption = TRUE,
			              dropboxWidth = "200%")
					),
					tabPanel("Tumor index",
			            virtualSelectInput(
			              inputId = ns("tumor_index_id"),
			              label = NULL,
			              choices = NULL, multiple = TRUE,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Immune Infiltration",
			            virtualSelectInput(
			              inputId = ns("immune_infiltration_id"),
			              label = NULL,
			              choices = NULL, multiple = TRUE,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Pathway activity",
			            virtualSelectInput(
			              inputId = ns("pathway_activity_id"),
			              label = NULL,
			              choices = NULL, multiple = TRUE,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					),
					tabPanel("Phenotype data",
			            virtualSelectInput(
			              inputId = ns("phenotype_data_id"),
			              label = NULL,
			              choices = NULL, multiple = TRUE,
			              search = TRUE,
			              allowNewOption = FALSE,
			              dropboxWidth = "200%")
					)
				),	
			),
			tabPanel("All",
				uiOutput(ns("tab_All"))
			),
			tabPanel("File",
				fluidRow(
					column(8, fileInput(ns("fl_L3_x"),NULL, accept = ".txt")),
					column(3, downloadButton(ns("dw_L3_x"), "e.g."))
				)
			)
		),
		shinyWidgets::actionBttn(
			ns("inspect_data_x"), button_name,
	        style = "gradient",
	        icon = icon("search"),
	        color = "primary",
	        block = TRUE,
	        size = "sm"
		),
	    div(verbatimTextOutput(ns("L3s_x_tip")),
	      style = "margin-top: 5px; margin-bottom: 0px;"
	    ),
		# verbatimTextOutput(ns("L3s_x_tip")),
		uiOutput(ns("L3s_x_data.ui"))
	)
}


multi_upload_Server = function(input, output, session, database = "toil", #id_option = tcga_id_option,
							   samples=NULL,custom_metadata=NULL, opt_pancan=NULL, table.ui=TRUE){
	ns <- session$ns

	id_option = switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)
	id_category = lapply(id_option, names)
	
	observe({
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "L3_x_type_tab", selected = input$L3_x_type)
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


	L2_x = reactive({
	  switch(input$data_L1,
	    `Molecular profile` = input$genomic_profile,
	    `Tumor index` = input$tumor_index,
	    `Immune Infiltration` = input$immune_infiltration,
	    `Pathway activity` = input$pathway_activity,
	    `Phenotype data` = input$phenotype_data
	  )
	})

	output$tab_All = renderUI({
		if(L2_x() %in% id_category[["Molecular profile"]]){
			selectInput(ns("tab_All_PW"),"ids belong to Pathway: ",
				choices = PW_meta$Name, selected = "ANGIOGENESIS")
		}
	})

	output$dw_L3_x = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			all_ids = id_option[[input$data_L1]][[L2_x()]]$all 
			sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
			if(L2_x()=="Custom metadata"){
				if(is.null(custom_metadata)){
					sample_ids = NULL
				} else {
					sample_ids = sample(colnames(custom_metadata()[-1]),
						ifelse(length(colnames(custom_metadata()[-1]))>10,10,
							   length(colnames(custom_metadata()[-1]))))
				}
			}
			write.table(sample_ids, file,
				quote = F, row.names = F, col.names = F)
		}
	)

	L3s_x = reactive({
		if(input$L3_x_type=="Selection"){
			L3s_x = switch(input$data_L1,
				    `Molecular profile` = input$genomic_profile_id,
				    `Tumor index` = input$tumor_index_id,
				    `Immune Infiltration` = input$immune_infiltration_id,
				    `Pathway activity` = input$pathway_activity_id,
				    `Phenotype data` = input$phenotype_data_id
				  )
		} else if (input$L3_x_type=="File"){
			file = input$fl_L3_x
			if(is.null(file$datapath)){  # 如果空文件
				L3s_x = NULL
			} else {
				L3s_x = read.table(file$datapath)[,1]
				L3s_x = L3s_x[L3s_x %in% id_option[[input$data_L1]][[L2_x()]]$all]
				if(length(L3s_x)>1000 & L2_x() %in% id_category[["Molecular profile"]]){
					L3s_x = L3s_x[1:1000]
				} 
			}
		} else if (input$L3_x_type=="All"){
			pw_sle = ifelse(is.null(input$tab_All_PW),"ANGIOGENESIS",input$tab_All_PW)
			pw_genes = strsplit(PW_meta$Gene[PW_meta$Name==pw_sle],"/")[[1]]
			L3s_x = id_option[[input$data_L1]][[L2_x()]]$all
			if(L2_x() %in% 
				c("mRNA Expression","DNA Methylation","Mutation status","Copy Number Variation")){
				L3s_x = L3s_x[L3s_x %in% pw_genes]
			} else if(L2_x() %in% c("Transcript Expression")){
				L3s_x = L3s_x[L3s_x %in% tcga_id_referrence[[1]][[5]]$Level3[tcga_id_referrence[[1]][[5]]$Symbol %in% pw_genes]]
			}
			if(L2_x()=="Custom metadata" & !is.null(custom_metadata)){
				L3s_x = colnames(custom_metadata()[-1])
			}
		}
		L3s_x
	})
	output$L3s_x_tip = renderPrint({
		cat(paste0("Tip: ",length(L3s_x())," ids are selected.\n"))
		# str(L3s_x())
	})

	L3s_x_data = eventReactive(input$inspect_data_x, {
		L1_x = names(id_category)[sapply(id_category, function(x){any(x %in% L2_x())})]
		withProgress(message = "Please wait for a while.",{
			x_data_merge = lapply(seq(L3s_x()), function(i){
				# 进度提醒
			    incProgress(1 / length(L3s_x()), detail = paste0("(Run ",i,"/",length(L3s_x()),")"))

				L3_x = L3s_x()[i]
				L2_x = L2_x()

				if(is.null(opt_pancan)){
					opt_pancan = .opt_pancan
				} else {
					opt_pancan = opt_pancan()
				}

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
				x_data = x_data %>%
					dplyr::filter(Sample %in% samples()) %>%
				    dplyr::select(id, Sample, value)
				# 默认批量下载的应为数值型变量，若不是则剔除
				if(class(x_data$value)=="character"){  
					return(NULL)
				} else {
					return(x_data)
				}
			}) %>% do.call(rbind, .)
			x_data_merge =  x_data_merge %>%
				dplyr::arrange(id,Sample)
		})
	})

	observeEvent(input$inspect_data_x,{
		output$L3s_x_data.ui = renderUI({
			shiny::validate(
				need(try(nrow(L3s_x_data())>0), 
					"No sample data were available. Please inspect operations in Preset step."),
			)
			if(table.ui){
				L3s_x_data_ = L3s_x_data()[,c("id","Sample","value")]
				if(class(L3s_x_data_[,"value"])=="numeric"){
					L3s_x_data_[,"value"] = round(L3s_x_data_[,"value"], digits = 3)
				}
				output$L3s_x_data = renderDataTable({
					datatable(L3s_x_data_,
						# class = "nowrap row-border",
						options = list(pageLength = 3, 
							columnDefs = list(list(className = 'dt-center', targets="_all")))
					)
				})
				dataTableOutput(ns("L3s_x_data"))
			} else {
				output$L3s_x_data = renderPrint({
					ids_num = length(unique(L3s_x_data()$id))
					cat(paste0("Tip: ", ids_num, " ids are prepared."))
				})
				verbatimTextOutput(ns("L3s_x_data"))
			}
		})

	})


	return(L3s_x_data)
}
