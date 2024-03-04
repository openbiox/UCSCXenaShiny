filter_samples_UI = function(id, database="toil"){
	ns = NS(id)

	id_option =switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)


	tagList(
		dropMenu(
			shinyWidgets::actionBttn(
				ns("filter_phe"), "Enter",
		        style = "gradient",
		        icon = icon("filter"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			),
			div(h5("Filter samples by one/multiple phenotype(s):"),style="width:800px;"),
	        wellPanel(
	        	style = "background: #f7f7f7",
		        h4("1. Add phenotypes (Optional)"),

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
									selected = "Clinical Phenotype")
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
	        	style = "background: #f7f7f7",
	        	h4("2. Observe distribution"),
				uiOutput(ns("filter_phe_01_by.ui")),
				verbatimTextOutput(ns("filter_phe_01_out"))
			),
		    wellPanel(
		    	style = "background: #f7f7f7",
			    h4("3. Set conditions"),
			    fluidRow(
				    actionBttn(
				      inputId = ns("add_condi"),
				      label = "Add filter",
				      color = "primary",
				      style = "bordered", size = "sm",
				      block = F
				    ),
				    actionBttn(
				      inputId = ns("del_condi"),
				      label = "Del filter",
				      color = "primary",
				      style = "bordered", size = "sm",
				      block = F
				    )
			    ),
			    fluidRow(
			    	column(5, h5("Phenotype(s):")),
			    	column(5, h5("Direction(s):")),
			    	column(2, h5("Threshold(s):"))
			    ),
			    uiOutput(ns("multi_condi.ui")),
			    verbatimTextOutput(ns("filter_by_phe_prompt")),
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
			),
			placement = "right-end",
			maxWidth = "1000px"
		)
	)
}




filter_samples_Server = function(input, output, session, database="toil", #id_option=tcga_id_option,
								 cancers=NULL, custom_metadata=NULL, opt_pancan=NULL){
	ns <- session$ns

	id_option =switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)

	observe({
	  updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
  	  updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
	}) 

	observeEvent(input$genomic_profile, {
		genomic_profile_choices <- reactive({
		  id_option[["Molecular profile"]][[input$genomic_profile]]
		})
	    updateVirtualSelect(
	      "genomic_profile_id",
	      choices = genomic_profile_choices()$all,
	      selected = genomic_profile_choices()$default
	    )
	})
	observeEvent(input$tumor_index, {
		tumor_index_choices <- reactive({
		  id_option[["Tumor index"]][[input$tumor_index]]
		})
	    updateVirtualSelect(
	      "tumor_index_id",
	      choices = tumor_index_choices()$all,
	      selected = tumor_index_choices()$default
	    )
	})
	observeEvent(input$immune_infiltration, {
		immune_infiltration_choices <- reactive({
		  id_option[["Immune Infiltration"]][[input$immune_infiltration]]
		})
	    updateVirtualSelect(
	      "immune_infiltration_id",
	      choices = immune_infiltration_choices()$all,
	      selected = immune_infiltration_choices()$default
	    )
	})
	observeEvent(input$pathway_activity, {
		pathway_activity_choices <- reactive({
		  id_option[["Pathway activity"]][[input$pathway_activity]]
		})
	    updateVirtualSelect(
	      "pathway_activity_id",
	      choices = pathway_activity_choices()$all,
	      selected = pathway_activity_choices()$default
	    )
	})
	observeEvent(input$phenotype_data, {
		phenotype_data_choices <- reactive({
		    id_tmp = id_option[["Phenotype data"]]
			if(!is.null(custom_metadata)){
				id_tmp[["Custom metadata"]]$all = sort(colnames(custom_metadata()[-1]))
				id_tmp[["Custom metadata"]]$default = sort(colnames(custom_metadata()[-1]))[1]
			}
			id_tmp[[input$phenotype_data]]
		})
		updateVirtualSelect(
		  "phenotype_data_id",
		  choices = phenotype_data_choices()$all,
		  selected = phenotype_data_choices()$default
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


	add_phes <- reactiveValues(cancers = "ACC",click=0, name = list(), label=list(), show="")

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
			paste0("Condition_",add_phes$click))
		add_phes$show = paste0(add_phes$show,
			"Condition_",add_phes$click,": ", input$data_L1,"--",add_level2(),
			"--",add_level3(),"\n")
	}) 
	observeEvent(input$button_phe_add_reset, {
		add_phes$click = 0	
		add_phes$name = list()
		add_phes$label=list()
		add_phes$show=""
	  	dynamic_condi$sum = 0
	}) 
	output$add_info = renderPrint({cat(add_phes$show)})

	# 下载已添加的变量数据
	add_phes_dat = eventReactive(input$button_phe_add, {

		x_tmp = lapply(seq(add_phes$name), function(i){
			L1_x = str_split(add_phes$name[[i]], "--")[[1]][1]   # Level-1
			L2_x = str_split(add_phes$name[[i]], "--")[[1]][2]    # Level-2
			L3_x = str_split(add_phes$name[[i]], "--")[[1]][3] # Level-3

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
			# x_data = x_data[,c("sample","value")]
			# 以clinical_phe作为最终的背景人群参考
			x_data = clinical_phe[,"Sample"] %>% 
			  dplyr::rename(sample=Sample) %>% 
			  dplyr::mutate(value=x_data$value[match(sample, x_data$Sample)]) 

			colnames(x_data)[2] = add_phes$label[[i]]
			x_data %>%
				tibble::column_to_rownames("sample")

		}) %>% do.call(cbind, .)
		x_tmp %>% tibble::rownames_to_column("sample")
	})


	## step2观察表型
	output$filter_phe_01_by.ui = renderUI({
		selectInput(ns("filter_phe_01_by"), NULL,
			choices = colnames(add_phes$phe_primary)[-1:-2], selected = "Gender"
		)
	})
	# 初始表型/添加表型
	observe({
		if(add_phes$click==0){
			add_phes$phe_primary = query_tcga_group(
				database = database,
				cancer = add_phes$cancers, 
				return_all = T)
		} else {
			add_phes$phe_primary = query_tcga_group(
				database = database,
				cancer = add_phes$cancers, custom = add_phes_dat(),
				return_all = T)
		}
	})
	# 观察当前表型的分布
	output$filter_phe_01_out = renderPrint({

		filter_phe_01_out_tmp = as.data.frame(add_phes$phe_primary[,input$filter_phe_01_by,drop=FALSE])

    	if(is.null(input$filter_phe_01_by)) return(NULL)

	    if(class(filter_phe_01_out_tmp[,input$filter_phe_01_by,drop=TRUE])=="numeric"){
	      summary(filter_phe_01_out_tmp[,input$filter_phe_01_by,drop=TRUE])
	    } else {
	      table(filter_phe_01_out_tmp[,input$filter_phe_01_by,drop=TRUE])
	    }

	})

	# filter--phe--step3
	dynamic_condi = reactiveValues(add = 0, del = 0, sum = 0)
	observeEvent(input$add_condi,{
		dynamic_condi$add = dynamic_condi$add + 1
		dynamic_condi$sum = dynamic_condi$add - dynamic_condi$del
	})
	observeEvent(input$del_condi,{
		dynamic_condi$del = dynamic_condi$del + 1

		if(dynamic_condi$del >= dynamic_condi$add){
			dynamic_condi$del = dynamic_condi$add
		}
		dynamic_condi$sum = dynamic_condi$add - dynamic_condi$del
	})

	# 更新：个性化添加/减少条件
	output$multi_condi.ui = renderUI({
    	if(dynamic_condi$sum == 0) return()
    	inputTagList <- tagList()
    	lapply(1:dynamic_condi$sum,function(i){
    		# item1(id) -- phenotype(label)
    		id_condi_item1 = paste0("item1_",i)
    		label_condi_item1 = paste0("Phe-",i)

    		# item2(id) -- threshold(label)
    		id_condi_item2 = paste0("item2_",i)
    		choices_condi_item2 = unique(add_phes$phe_primary[,
    			ifelse(is.null(input[[id_condi_item1]]),"Gender",input[[id_condi_item1]]),drop=T])
    		label_condi_item2 = paste0("Thres-",i)

    		# item3(id) -- direction(label)
    		id_condi_item3 = paste0("item3_",i)
    		if(class(choices_condi_item2)=="character"){
    			choices_condi_item3 = c("+", "-")
			} else {
				choices_condi_item3 = c(">", "<", "%>", "%<")
			}
    		label_condi_item3 = paste0("Direc-",i)

    		# 添加新筛选器，不重置旧数据
			new_phe <- "Gender"
			if (id_condi_item1 %in% names(input)) {
				new_phe <- input[[id_condi_item1]]
			}

			new_thres = NULL
			if (id_condi_item2 %in% names(input)) {
				new_thres <- input[[id_condi_item2]]
			}

			new_direc <- NULL
			if (id_condi_item3 %in% names(input)) {
				new_direc <- input[[id_condi_item3]]
			}

			## 设置3个输入控件
			dynamic_input = fluidRow(
				column(
					5, 
					selectInput(ns(id_condi_item1), NULL,#label_condi_item1, 
	    						colnames(add_phes$phe_primary)[-1:-2], selected=new_phe)),
				column(
					2, 
					selectInput(ns(id_condi_item3),  NULL,#label_condi_item3, 
	    						choices_condi_item3, selected=new_direc)),
				column(
					5, 
					if(class(choices_condi_item2)=="character"){
						selectInput(ns(id_condi_item2), NULL,#label_condi_item2, 
		    						sort(choices_condi_item2,na.last = T), 
		    						selected=new_thres, multiple = T)
					} else {
						numericInput(ns(id_condi_item2), NULL,# label_condi_item2, 
							value = new_thres)
					}
				)
			)

	    	# phe_Input <- selectInput(id_condi_item1, label_condi_item1, 
	    	# 						 c("Option 1", "Option 2", "Option 3"), selected=new_phe)

			inputTagList <<- tagAppendChild(inputTagList,       
			                              	dynamic_input)
			
    	})
    	inputTagList
	})


	filter_by_phe = eventReactive(input$button_phe_filter, {
		if(dynamic_condi$sum==0){
			NULL
		} else {
			lapply(1:dynamic_condi$sum,function(i){
				# 验证非空
				if(input[[paste0("item3_",i)]] %in% c("+","-")){
					logic_item2_blank = is.null(input[[paste0("item2_",i)]])
				} else {
					logic_item2_blank = is.na(input[[paste0("item2_",i)]])
				}
				shiny::validate(
					need(!logic_item2_blank, "Please input the non-null threshold."),
				)
				# 验证范围（百分位数）
				if(input[[paste0("item3_",i)]] %in% c("%>","%<")){
					logic_item2_quant = input[[paste0("item2_",i)]]>1 | input[[paste0("item2_",i)]]<0
					shiny::validate(
						need(!logic_item2_quant, "Please inspect the threshold range (0~1) when %> or %<.")
					)
				}

			    condi_item1 = input[[paste0("item1_",i)]]  #表型
			    condi_item2 = paste(input[[paste0("item2_",i)]],collapse="|") #阈值
			    condi_item3 = input[[paste0("item3_",i)]] #方向
			    c(condi_item1,condi_item2,condi_item3)
		    })
		}
	})
	output$filter_by_phe_prompt = renderPrint({
		if(inherits(filter_by_phe(),"list")){
			cat("Above conditions have been executed.")
		} else {
			filter_by_phe()
		}
	}) 

	# 重置筛选条件
	observeEvent(input$button_phe_filter_reset, {
	  dynamic_condi$add = 0
	  dynamic_condi$del = 0
	  dynamic_condi$sum = 0
	  add_phes$filter_phe_id = NULL
	})
	# 提取筛选条件



	# 获取筛选后的样本ID
	observeEvent(input$button_phe_filter, {
		shiny::validate(
			need(try(dynamic_condi$sum > 0), 
				"Please set at least one condition."),
		)
		if(add_phes$click==0){
			add_phes$filter_phe_id = query_tcga_group(database = database,cancer = add_phes$cancers,
				filter_by = filter_by_phe(),)[["data"]]$Sample
		} else {
			add_phes$filter_phe_id = query_tcga_group(database = database,cancer = add_phes$cancers,
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
