ui.modules_download_pancan = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				4,
				wellPanel(
					style = "height:1000px",
					h3("1. Select one database"),
					radioGroupButtons(
					   inputId = ns("L0"),
					   label = NULL,
					   choiceNames = c("TOIL", "PCAWG", "CCLE"),
					   choiceValues = c("toil", "pcawg", "ccle"),
					   selected = "toil",
					   justified = TRUE,
					   checkIcon = list(
					      yes = icon("ok", 
					    lib = "glyphicon"))
					),
					tabsetPanel(id = ns("L0_datasets_tab"),
						type = "hidden",
						tabPanel("toil",
							mol_origin_UI(ns("mol_origin2toil"), database = "toil"),
						),
						tabPanel("pcawg",
							mol_origin_UI(ns("mol_origin2pcawg"), database = "pcawg"),
						),
						tabPanel("ccle",
							mol_origin_UI(ns("mol_origin2ccle"), database = "ccle"),
						),
					),

					h3("2. Select multiple ids"),	
					# 选择major/minor type
				    fluidRow(
				    	column(
				    		6,
						    selectInput(
						    	ns("data_L1"), label = "Data type:",
						    	choices = c("Molecular profile","Tumor index","Immune Infiltration","Pathway activity","Phenotype data"),
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
										choices = NULL,
										selected = "mRNA Expression")
								),
								tabPanel("Tumor index",
									selectInput(
										ns("tumor_index"), "Data subtype:",
										choices = NULL,
										selected = "Tumor Purity")
								),
								tabPanel("Immune Infiltration",
									selectInput(
										ns("immune_infiltration"), "Data subtype:",
										choices = NULL,
										selected = "CIBERSORT")
								),
								tabPanel("Pathway activity",
									selectInput(
										ns("pathway_activity"), "Data subtype:",
										choices = NULL,
										selected = "HALLMARK")
								),
								tabPanel("Phenotype data",
									selectInput(
										ns("phenotype_data"), "Data subtype:",
										choices = NULL,
										selected = "Clinical Phenotye")
								)
							)
				    	)
				    ),
					prettyRadioButtons(ns("L3_x_type"),"Choose multi-ids by", 
						choices = c("Selection","All","File"), selected = "Selection"
					),
					tabsetPanel(id = ns("L3_x_type_tab"),
						type = "hidden",
						tabPanel("Selection",
						    tabsetPanel(
							    id = ns("data_L3_tab"),
							    type = "hidden",
								tabPanel("Molecular profile",
						            selectizeInput(
						              inputId = ns("genomic_profile_id"),
						              label = NULL,
						              choices = NULL, multiple = TRUE,
						              options = list(create = TRUE, maxOptions = 10))
								),
								tabPanel("Tumor index",
						            selectizeInput(
						              inputId = ns("tumor_index_id"),
						              label = NULL,
						              choices = NULL, multiple = TRUE,
						              options = list(create = FALSE, maxOptions = 10))
								),
								tabPanel("Immune Infiltration",
						            selectizeInput(
						              inputId = ns("immune_infiltration_id"),
						              label = NULL,
						              choices = NULL, multiple = TRUE,
						              options = list(create = FALSE, maxOptions = 10))
								),
								tabPanel("Pathway activity",
						            selectizeInput(
						              inputId = ns("pathway_activity_id"),
						              label = NULL,
						              choices = NULL, multiple = TRUE,
						              options = list(create = FALSE, maxOptions = 10))
								),
								tabPanel("Phenotype data",
						            selectizeInput(
						              inputId = ns("phenotype_data_id"),
						              label = NULL,
						              choices = NULL, multiple = TRUE,
						              options = list(create = FALSE, maxOptions = 10))
								)
							),	
						),
						tabPanel("All",
							uiOutput(ns("tab_All"))
						),
						tabPanel("Pathway",
							uiOutput(ns("tab_Pathway"))
						),
						tabPanel("File",
							fluidRow(
								column(8, fileInput(ns("fl_L3_x"),NULL, accept = ".txt")),
								column(3, downloadButton(ns("dw_L3_x"), "e.g."))
							)
						)
					),
					# verbatimTextOutput(ns("L3s_x_tip")),
					verbatimTextOutput(ns("L3s_x_tip")),
					shinyWidgets::actionBttn(
						ns("inspect_data_x"), "Query",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					verbatimTextOutput(ns("L3s_x_tip2")),
					br(),
					h3("3. Download results"),
				    fluidRow(
				    	column(3, downloadButton(ns("save_csv"), "Download table(.csv)")),
				    	column(3, offset = 2, downloadButton(ns("save_rda"), "Download table(.rda)"))
				    ),
				)
			),
			column(
				5,
				offset = 1,
				br(),br(),br(),br(),
				dataTableOutput(ns("L3s_x_data")),
				br(),br(),
				strong(h3("NOTEs:")),
				h5("1. To get the whole dataset, please click 'Respository' page and download derictly from UCSC website."),
				h5("2. Queried data in long format is for easy display and it is downloaded as the wide format. "),
			)
		)
	)
}


server.modules_download_pancan = function(input, output, session, custom_metadata = NULL, opt_pancan = NULL){
	ns = session$ns

	id_option = reactive({switch(input$L0, 
					"toil"=tcga_id_option,
					"pcawg"=pcawg_id_option,
					"ccle"=ccle_id_option)})
	id_category = reactive({lapply(id_option(), names)})
	# id_option = reactive({tcga_id_option})


	observe({
	  updateTabsetPanel(inputId = "L0_datasets_tab", selected = input$L0)
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "L3_x_type_tab", selected = input$L3_x_type)
	}) 


	opt_pancan_toil = callModule(mol_origin_Server, "mol_origin2toil", database = "toil")
	opt_pancan_pcawg = callModule(mol_origin_Server, "mol_origin2pcawg", database = "pcawg")
	opt_pancan_ccle = callModule(mol_origin_Server, "mol_origin2ccle", database = "ccle")

	opt_pancan = reactive({
		switch(input$L0,
			`toil`=opt_pancan_toil(),
			`pcawg`=opt_pancan_pcawg(),
			`ccle`=opt_pancan_ccle(),
		)
	})

	genomic_profile_choices <- reactive({
	  id_option()[["Molecular profile"]][[input$genomic_profile]]
	})

	tumor_index_choices <- reactive({
	  id_option()[["Tumor index"]][[input$tumor_index]]
	})

	immune_infiltration_choices <- reactive({
	  id_option()[["Immune Infiltration"]][[input$immune_infiltration]]
	})

	pathway_activity_choices <- reactive({
	  id_option()[["Pathway activity"]][[input$pathway_activity]]
	})

	phenotype_data_choices <- reactive({
	  id_option()[["Phenotype data"]][[input$phenotype_data]]
	})

	# update L2 choice
	observe({
	  #L2
	  updateSelectizeInput(
	    session,
	    "genomic_profile",
	    choices = names(id_option()[["Molecular profile"]]),
	    server = TRUE
	  )
	  updateSelectizeInput(
	    session,
	    "tumor_index",
	    choices = names(id_option()[["Tumor index"]]),
	    server = TRUE
	  )
	  updateSelectizeInput(
	    session,
	    "immune_infiltration",
	    choices = names(id_option()[["Immune Infiltration"]]),
	    server = TRUE
	  )
	  updateSelectizeInput(
	    session,
	    "pathway_activity",
	    choices = names(id_option()[["Pathway activity"]]),
	    server = TRUE
	  )
	  updateSelectizeInput(
	    session,
	    "phenotype_data",
	    choices = names(id_option()[["Phenotype data"]])[1],
	    server = TRUE
	  )
	})

	# update L3 choice
	observe({
	  #L3
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

	L2_x = reactive({
	  switch(input$data_L1,
	    `Molecular profile` = input$genomic_profile,
	    `Tumor index` = input$tumor_index,
	    `Immune Infiltration` = input$immune_infiltration,
	    `Pathway activity` = input$pathway_activity,
	    `Phenotype data` = input$phenotype_data
	  )
	})


	observe(
		if(input$data_L1 == "Molecular profile"){
			if(L2_x() %in% c("miRNA Expression","Protein Expression")){
				updatePrettyRadioButtons(
					inputId = "L3_x_type",
					choices = c("Selection","File"),
					inline=TRUE
				)
			} else {
				updatePrettyRadioButtons(
					inputId = "L3_x_type",
					choices = c("Selection","Pathway","File"),
					inline=TRUE
				)
			}
		} else {
			updatePrettyRadioButtons(
				inputId = "L3_x_type",
				choices = c("Selection","All","File"),
				inline=TRUE
			)
		}
	)

	output$tab_All = renderUI({
		p(strong(span("All ids under the data range will be selected.", style="color:grey")))
	})
	output$tab_Pathway = renderUI({
		selectInput(ns("tab_All_PW"),"ids belong to Pathway: ",
			choices = PW_meta$Name, selected = "ANGIOGENESIS")
	})
	output$dw_L3_x = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			all_ids = id_option()[[input$data_L1]][[L2_x()]]$all 
			sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
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
			file = input$upload_sp_info
			if(is.null(file$datapath)){  # 如果空文件
				L3s_x = NULL
			} else {
				L3s_x = read.table(file$datapath)[,1]
				L3s_x = L3s_x[L3s_x %in% all_ids]
				if(length(L3s_x)>500 & L2_x() %in% id_category()[["Molecular profile"]]){
					L3s_x = L3s_x[1:500]
				}
			}
		} else if (input$L3_x_type=="All"){
			L3s_x = id_option()[[input$data_L1]][[L2_x()]]$all
		} else if (input$L3_x_type=="Pathway"){
			pw_sle = ifelse(is.null(input$tab_All_PW),"ANGIOGENESIS",input$tab_All_PW)
			pw_genes = strsplit(PW_meta$Gene[PW_meta$Name==pw_sle],"/")[[1]]
			L3s_x = id_option()[[input$data_L1]][[L2_x()]]$all
			if(L2_x() %in% 
				c("mRNA Expression","DNA Methylation","Mutation status","Copy Number Variation","Gene Fusion")){
				L3s_x = L3s_x[L3s_x %in% pw_genes]
			} else if(L2_x() %in% c("Transcript Expression")){
				L3s_x = L3s_x[L3s_x %in% tcga_id_referrence[[1]][[5]]$Level3[tcga_id_referrence[[1]][[5]]$Symbol %in% pw_genes]]
			}
		}
		L3s_x
	})
	output$L3s_x_tip = renderPrint({
		cat(paste0("Tip: ",length(L3s_x())," unique ids are selected.\n"))
		# str(L3s_x())
	})
	L3s_x_data = eventReactive(input$inspect_data_x, {
		L1_x = names(id_category())[sapply(id_category(), function(x){any(x %in% L2_x())})]
		withProgress(message = "Please wait for a while...",{
			x_data_merge = lapply(seq(L3s_x()), function(i){
				# 进度提醒
			    incProgress(1 / length(L3s_x()), detail = paste0("(Run ",i,"/",length(L3s_x()),")"))

				L3_x = L3s_x()[i]
				L2_x = L2_x()
				database = input$L0

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
					dplyr::arrange(Sample) %>%
					# dplyr::filter(Sample %in% samples()) %>%
				    dplyr::select(id, Sample, value)
				# 默认批量下载的应为数值型变量，若不是则剔除
				# if(class(x_data$value)=="character"){  
				# 	return(NULL)
				# } else {
				# 	return(x_data)
				# }
				## 提醒用户注意
				return(x_data)
			}) %>% do.call(rbind, .)
			# x_data_merge =  x_data_merge %>%
			# 	tibble::rownames_to_column("Sample")
			x_data_merge
		})
	})
	observeEvent(input$inspect_data_x,{
		shiny::validate(
			need(try(nrow(L3s_x_data())>0), 
				"No sample data were available. Please inspect operations in Preset step."),
		)
		output$L3s_x_tip2 = renderPrint({
			ids_num = length(unique(L3s_x_data()$id))
			cat(paste0("Tip: ", ids_num, " ids are queried successfully!"))
		})
		verbatimTextOutput(ns("L3s_x_tip2"))

		L3s_x_data_ = L3s_x_data()


		if(class(L3s_x_data_[,"value"])=="numeric"){
			L3s_x_data_[,"value"] = round(L3s_x_data_[,"value"], digits = 3)
		}
		output$L3s_x_data = renderDataTable({
			datatable(L3s_x_data_,
				# class = "nowrap row-border",
				options = list(pageLength = 10, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		})

	})
	output$save_csv = downloadHandler(
		filename = function(){
			paste0("Batch_query_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			L3s_x_data_ = L3s_x_data()
			L3s_x_data_wide = reshape2::dcast(L3s_x_data_, Sample~id, value.var = "value")
			write.csv(L3s_x_data_wide, file, row.names = FALSE)
		}
	)
	output$save_rda = downloadHandler(
		filename = function(){
			paste0("Batch_query_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rda")
		},
		content = function(file){
			L3s_x_data_ = L3s_x_data()
			L3s_x_data_wide = reshape2::dcast(L3s_x_data_, Sample~id, value.var = "value")
			query_data = L3s_x_data_wide
			save(query_data, file = file)
		}
	)
}