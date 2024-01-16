ui.modules_download_pancan = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				8,
				wellPanel(
					style = "height:1000px",
					fluidRow(
						column(6,
							h2("Part1: Download molecular data", align = "center"),
							h3("1. Select one database"),
							radioGroupButtons(
							   inputId = ns("L0"),
							   label = NULL,
							   choiceNames = c("TCGA(TOIL)", "PCAWG", "CCLE"),
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
								    	# choices = c("Molecular profile","Tumor index","Immune Infiltration","Pathway activity","Phenotype data"),
								    	choices = c("Molecular profile"),
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
								choices = c("Selection","All","File"), selected = "Selection",
								inline=TRUE
							),
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
									# uiOutput(ns("tab_All"))
									fluidRow(
										uiOutput(ns("msigdb_note.ui")), 
										column(
											4,
								            virtualSelectInput(
								              inputId = ns("msigdbr_cat"),
								              label = NULL,
								              choices = msigdbr_types$gs_subcat_label, 
								              selected =  msigdbr_types$gs_subcat_label[1],
								              dropboxWidth = "200%")
										),
										column(
											8,
								            virtualSelectInput(
								              inputId = ns("msigdbr_pw"),
								              label = NULL,
								              choices = NULL, 
								              selected =  NULL,
								              search = TRUE,
								              dropboxWidth = "200%")
										)
									)
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
							# br(),
							h3("3. Download results"),
						    fluidRow(
						    	column(3, downloadButton(ns("save_csv"), "Download table(.csv)")),
						    	column(3, offset = 2, downloadButton(ns("save_rda"), "Download table(.rda)"))
						    )
						),
						column(6,
							style = "height:1000px",
							br(),br(),br(),br(),
							dataTableOutput(ns("L3s_x_data")),
							br(),br(),
							strong(h3("NOTEs:")),
							h5("1. To get the whole dataset, please click 'Respository' page and download derictly from UCSC website."),
							h5("2. Queried data in long format is for easy display and it is downloaded as the wide format. "),	
						)
					)
				)
			),
			column(
				4,
				wellPanel(
					style = "height:1000px",
					h2("Part2: Download other data", align = "center"),
					h3("1. TCGA database"),
					selectInput(ns("tcga_other_type"),NULL,
						choices = c("Basic Phenotype data","Survival data", "Tumor index", "Immune Infiltration", "Pathway activity"),
						selected = c("Basic Phenotype data")
					),
					tabsetPanel(
						id = ns("tcga_other_type_tab"),
						type = "hidden",
						tabPanel("Basic Phenotype data",
							wellPanel(downloadButton(ns("save_tcga_phe"), "Download Basic Phenotype(.csv)",style="width:300px;"))
						),
						tabPanel("Survival data",
							wellPanel(downloadButton(ns("save_tcga_sur"), "Download Survival data(.csv)",style="width:300px;"))
						),
						tabPanel("Tumor index",
							wellPanel(
								fluidRow(downloadButton(ns("save_tcga_idx_purity"), "Download Tumor Purity(.csv)",style="width:300px;")),
								fluidRow(downloadButton(ns("save_tcga_idx_stemness"), "Download Tumor Stemness(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_idx_tmb"), "Download Tumor Mutation Burden(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_idx_msi"), "Download Microsatellite Instability(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_idx_gi"), "Download Genome Instability(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;")
							)

						),
						tabPanel("Immune Infiltration",
							wellPanel(
								fluidRow(downloadButton(ns("save_tcga_til_cib"), "Download CIBERSORT(.csv)",style="width:300px;")),
								fluidRow(downloadButton(ns("save_tcga_til_cib_abs"), "Download CIBERSORT-ABS(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;"),
								fluidRow(downloadButton(ns("save_tcga_til_epic"), "Download EPIC(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_til_mcp"), "Download MCPCOUNTER(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_til_quan"), "Download QUANTISEQ(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_til_tim"), "Download TIMER(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_tcga_til_xce"), "Download XCELL(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;")
							)

						),
						tabPanel("Pathway activity",
							wellPanel(
								fluidRow(downloadButton(ns("save_tcga_pw_hm"), "Download HALLMARK(.csv)",style="width:300px;")),
								fluidRow(downloadButton(ns("save_tcga_pw_kegg"), "Download KEGG(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;"),
								fluidRow(downloadButton(ns("save_tcga_pw_iobr"), "Download IOBR(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;")							)
						),
					),

					h3("2. PCAWG database"),
					selectInput(ns("pcawg_other_type"),NULL,
						choices = c("Basic Phenotype data","Survival data", "Tumor index", "Immune Infiltration", "Pathway activity"),
						selected = c("Basic Phenotype data")
					),
					tabsetPanel(
						id = ns("pcawg_other_type_tab"),
						type = "hidden",
						tabPanel("Basic Phenotype data",
							wellPanel(downloadButton(ns("save_pcawg_phe"), "Download Basic Phenotype(.csv)",style="width:300px;"))
						),
						tabPanel("Survival data",
							wellPanel(downloadButton(ns("save_pcawg_sur"), "Download Survival data(.csv)",style="width:300px;"))
						),
						tabPanel("Tumor index",
							wellPanel(
								fluidRow(downloadButton(ns("save_pcawg_idx_purity"), "Download Tumor Purity(.csv)",style="width:300px;"))
							)
						),
						tabPanel("Immune Infiltration",
							wellPanel(
								fluidRow(downloadButton(ns("save_pcawg_til_cib"), "Download CIBERSORT(.csv)",style="width:300px;")),
								fluidRow(downloadButton(ns("save_pcawg_til_cib_abs"), "Download CIBERSORT-ABS(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;"),
								fluidRow(downloadButton(ns("save_pcawg_til_epic"), "Download EPIC(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_pcawg_til_mcp"), "Download MCPCOUNTER(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_pcawg_til_quan"), "Download QUANTISEQ(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_pcawg_til_tim"), "Download TIMER(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;"),
								fluidRow(downloadButton(ns("save_pcawg_til_xce"), "Download XCELL(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;")
							)

						),
						tabPanel("Pathway activity",
							wellPanel(
								fluidRow(downloadButton(ns("save_pcawg_pw_hm"), "Download HALLMARK(.csv)",style="width:300px;")),
								fluidRow(downloadButton(ns("save_pcawg_pw_kegg"), "Download KEGG(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;"),
								fluidRow(downloadButton(ns("save_pcawg_pw_iobr"), "Download IOBR(.csv)",style="width:300px;"),
									style = "margin-top: 5px; margin-bottom: 0px;width:300px;")							)
						),
					),
					h3("3. CCLE database"),
					selectInput(ns("ccle_other_type"),NULL,
						choices = c("Basic Phenotype data","Tumor index"),
						selected = c("Basic Phenotype data")
					),
					tabsetPanel(
						id = ns("ccle_other_type_tab"),
						type = "hidden",
						tabPanel("Basic Phenotype data",
							wellPanel(downloadButton(ns("save_ccle_phe"), "Download Basic Phenotype(.csv)",style="width:300px;"))
						),
						tabPanel("Tumor index",
							wellPanel(
								fluidRow(downloadButton(ns("save_ccle_idx_purity"), "Download Tumor Purity(.csv)",style="width:300px;"))
							)
						),
					),


				)

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

	observe({
	  updateTabsetPanel(inputId = "L0_datasets_tab", selected = input$L0)
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
	  updateTabsetPanel(inputId = "L3_x_type_tab", selected = input$L3_x_type)
	  updateTabsetPanel(inputId = "tcga_other_type_tab", selected = input$tcga_other_type)
	  updateTabsetPanel(inputId = "pcawg_other_type_tab", selected = input$pcawg_other_type)
	  updateTabsetPanel(inputId = "ccle_other_type_tab", selected = input$ccle_other_type)
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
	  updateSelectInput(
	  	session,
	    "genomic_profile",
	    choices = names(id_option()[["Molecular profile"]])
	  )
	  updateSelectInput(
	  	session,
	    "tumor_index",
	    choices = names(id_option()[["Tumor index"]])
	  )
	  updateSelectInput(
	  	session,
	    "immune_infiltration",
	    choices = names(id_option()[["Immune Infiltration"]])
	  )
	  updateSelectInput(
	  	session,
	    "pathway_activity",
	    choices = names(id_option()[["Pathway activity"]])
	  )
	  updateSelectInput(
	  	session,
	    "phenotype_data",
	    choices = names(id_option()[["Phenotype data"]])[1]
	  )
	})

	# update L3 choice
	observe({
	  #L3
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

	msigdbr_query = reactive({
		category = msigdbr_types$gs_cat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		subcategory = msigdbr_types$gs_subcat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		if(length(category)!=0){
			msigdbr_query = msigdbr(species = "Homo sapiens", 
			                      category = category, 
			                      subcategory = subcategory)
			msigdbr_query
		}
	})

	observe({
		if(!is.null(msigdbr_query())){
			msigdbr_term_stat = as.data.frame(table(msigdbr_query()$gs_name)) %>% 
			  dplyr::rename(term=Var1, size=Freq) %>% 
			  dplyr::arrange(term) %>% 
			  dplyr::mutate(term_size = paste0(term," (", size,")"))
			updateVirtualSelect(
			  "msigdbr_pw",
			  choices = msigdbr_term_stat$term_size,
			  selected = msigdbr_term_stat$term_size[1]
			)
		}
	})

	output$msigdb_note.ui = renderUI({
		# pw_sle = ifelse(is.null(input$msigdbr_pw),"HALLMARK_ADIPOGENESIS",input$msigdbr_pw)
		pw_sle = str_split(input$msigdbr_pw," ")[[1]][1]
		term_link = sprintf("https://www.gsea-msigdb.org/gsea/msigdb/human/%s.html",
                    		ifelse(is.null(pw_sle),"HALLMARK_ADIPOGENESIS",pw_sle))
		msigdb_link = "https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp"

		h5(strong(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Note: For Molecular profile, select ids in ",
			  a("one pathway", href = term_link)," from ",
			  a("MSigDB database", href = msigdb_link), ":"))

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
			pw_genes = msigdbr_query() %>% 
			  dplyr::filter(gs_name %in% str_split(input$msigdbr_pw," ")[[1]][1]) %>% 
			  dplyr::pull(gene_symbol)
			# pw_genes = strsplit(PW_meta$Gene[PW_meta$Name==pw_sle],"/")[[1]]
			# L3s_x = id_option[[input$data_L1]][[L2_x()]]$all #!!! 2w候选基因
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

	## Part two
	output$save_tcga_phe = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_phenotype_value$`Clinical Phenotype`, file, row.names = FALSE) }
	)
	output$save_tcga_sur = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_surv, file, row.names = FALSE) }
	)
	output$save_tcga_idx_purity = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_index_value$`Tumor Purity`, file, row.names = FALSE) }
	)
	output$save_tcga_idx_stemness = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_index_value$`Tumor Stemness`, file, row.names = FALSE) }
	)
	output$save_tcga_idx_tmb = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_index_value$`Tumor Mutation Burden`, file, row.names = FALSE) }
	)
	output$save_tcga_idx_msi = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_index_value$`Microsatellite Instability`, file, row.names = FALSE) }
	)
	output$save_tcga_idx_gi = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_index_value$`Genome Instability`, file, row.names = FALSE) }
	)
	output$save_tcga_til_cib = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`CIBERSORT`, file, row.names = FALSE) }
	)
	output$save_tcga_til_cib_abs = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`CIBERSORT-ABS`, file, row.names = FALSE) }
	)
	output$save_tcga_til_epic = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`EPIC`, file, row.names = FALSE) }
	)
	output$save_tcga_til_mcp = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`MCPCOUNTER`, file, row.names = FALSE) }
	)
	output$save_tcga_til_quan = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`QUANTISEQ`, file, row.names = FALSE) }
	)
	output$save_tcga_til_tim = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`TIMER`, file, row.names = FALSE) }
	)
	output$save_tcga_til_xce = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`XCELL`, file, row.names = FALSE) }
	)
	output$save_tcga_til_xce = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_immune_value$`XCELL`, file, row.names = FALSE) }
	)
	output$save_tcga_pw_hm = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_pathway_value$`HALLMARK`, file, row.names = FALSE) }
	)	
	output$save_tcga_pw_kegg = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_pathway_value$`KEGG`, file, row.names = FALSE) }
	)	
	output$save_tcga_pw_iobr = downloadHandler(
		filename = function(){ paste0("TCGA_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(tcga_pathway_value$`IOBR`, file, row.names = FALSE) }
	)	

	### PCAWG
	output$save_pcawg_phe = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_phenotype_value$`Clinical Phenotype`, file, row.names = FALSE) }
	)
	output$save_pcawg_sur = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ 
			sur_dat_raw = pcawg_info[,c("dcc_project_code","icgc_specimen_id","OS","OS.time")]
			colnames(sur_dat_raw) = c("cancer","Sample","status","time")
			sur_dat_sub = sur_dat_raw %>% dplyr::distinct() 
			write.csv(sur_dat_sub, file, row.names = FALSE) 
		}
	)
	output$save_pcawg_idx_purity = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_index_value$`Tumor Purity`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_cib = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`CIBERSORT`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_cib_abs = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`CIBERSORT-ABS`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_epic = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`EPIC`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_mcp = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`MCPCOUNTER`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_quan = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`QUANTISEQ`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_tim = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`TIMER`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_xce = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`XCELL`, file, row.names = FALSE) }
	)
	output$save_pcawg_til_xce = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_immune_value$`XCELL`, file, row.names = FALSE) }
	)
	output$save_pcawg_pw_hm = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_pathway_value$`HALLMARK`, file, row.names = FALSE) }
	)	
	output$save_pcawg_pw_kegg = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_pathway_value$`KEGG`, file, row.names = FALSE) }
	)	
	output$save_pcawg_pw_iobr = downloadHandler(
		filename = function(){ paste0("PCAWG_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(pcawg_pathway_value$`IOBR`, file, row.names = FALSE) }
	)	

	### CCLE
	output$save_ccle_phe = downloadHandler(
		filename = function(){ paste0("CCLE_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(ccle_phenotype_value$`Clinical Phenotype`, file, row.names = FALSE) }
	)
	output$save_ccle_idx_purity = downloadHandler(
		filename = function(){ paste0("CCLE_metadata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv") },
		content = function(file){ write.csv(ccle_index_value$`Tumor Purity`, file, row.names = FALSE) }
	)
}
