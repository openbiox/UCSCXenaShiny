ui.modules_pancan_comp_batch = function(id) {
	ns = NS(id)
	fluidPage(
		# 第一行：选择肿瘤及样本
		fluidRow(
			# 选择分组依据
			column(
				2,
				wellPanel(
					style = "height:1000px",
					h2("S1: Preset", align = "center"),

					h4("1. Choose one cancer"),
					pickerInput(
						ns("choose_cancer"), NULL,
						choices = sort(tcga_cancer_choices),
						selected = "BRCA"),
					br(),br(),

					h4("2. Filter samples[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Choose samples for personalized need", 
					                   content = "choose_samples"),
					h5("Quick filter:"),
					pickerInput(
						ns("filter_by_code"), NULL,
						choices = NULL, selected =  NULL,
						multiple = TRUE, options = list(`actions-box` = TRUE)
					),
					h5("Exact filter:"),
					filter_samples_UI(ns("filter_samples2comp_batch")),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),br(),
					
					h4("3. Upload metadata[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Upload sample info", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2comp_batch")),
					br(),br(),

					h4("4. Modify datasets[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Set molecular profile origin", 
					                   content = "data_origin"),
					mol_origin_UI(ns("mol_origin2comp_batch"))
				)
			),
			# 分组设置
			column(
				3,
				wellPanel(
					style = "height:1000px",
					h2("S2: Group samples", align = "center"),
					# 调用分组模块UI
					group_samples_UI(ns("group_samples2comp_batch"))  
				)
			),
			column(
				3,
				wellPanel(
					style = "height:1000px",

					h2("S3: Select items for compare", align = "center"),

					br(),br(),
					shinyWidgets::actionBttn(
						ns("inspect_data_x"), "Query data",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),br(),

					selectInput(ns("L2_x"), "Choose data type", id_category),
					
					prettyRadioButtons(ns("L3_x_type"),"Choose multi-ids by",
						choices = c("Selection","All","File"), selected = "Selection", inline=TRUE) %>% 
							helper(type = "markdown", sie = "m", fade = TRUE,
									title = "Notes for IDs selction", content = "batch_ids"),
					tabsetPanel(id = ns("L3_x_type_tab"),
						type = "hidden",
						tabPanel("Selection",
							# materialSwitch(ns("all_ids"), "All ids?", inline = FALSE),
							selectizeInput(ns("L3_x"), NULL,choices=NULL, multiple=T),
						),
						tabPanel("File",
							fluidRow(
								column(8, fileInput(ns("fl_L3_x"),NULL, accept = ".txt")),
								column(3, downloadButton(ns("dw_L3_x"), "e.g."))
							)
						),
						tabPanel("All"
						)
					),

					br(),
					verbatimTextOutput(ns("L3s_x_tip")),
					br(),
					uiOutput(ns("L3s_x_data.ui"))

				)
			),
			column(
				4,
				wellPanel(
					style = "height:1000px",
					h2("S4: Batch analyze", align = "center"),

					br(),br(),
					shinyWidgets::actionBttn(
						ns("cal_batch_comp"), "Start calculation",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),br(),
					h4("1. Set method"),
					selectInput(ns("comp_method"), NULL,choices = c("t.test", "wilcox.test")),

					br(),br(),
					uiOutput(ns("comp_stat_tb.ui")),
					br(),
					uiOutput(ns("comp_stat_dw.ui"))
				)

			)
		)
	)
}



server.modules_pancan_comp_batch = function(input, output, session) {
	ns <- session$ns


	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", filter_phe_id=NULL,
		phe_primary=query_tcga_group(cancer = "BRCA", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancer
		cancer_choose$phe_primary <- query_tcga_group(cancer = cancer_choose$name, return_all = T)
	})

	## 过滤样本
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2comp_batch",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta()),
					   opt_pancan = reactive(opt_pancan()))
	# quick filter widget
	observe({
		code_types_valid = code_types[names(code_types) %in% 
							unique(cancer_choose$phe_primary$Code)]
		updatePickerInput(
			session,
			"filter_by_code",
			choices = unlist(code_types_valid,use.names = F),
			selected =  unlist(code_types_valid,use.names = F)
		)
	})
	# 综合上述二者
	observe({
		# quick filter
		choose_codes = names(code_types)[unlist(code_types) %in% input$filter_by_code]
		filter_phe_id2 = cancer_choose$phe_primary %>%
			dplyr::filter(Code %in% choose_codes) %>%
			dplyr::pull("Sample")

		# exact filter
		if(is.null(filter_phe_id())){
			cancer_choose$filter_phe_id = filter_phe_id2
		} else {
			cancer_choose$filter_phe_id = intersect(filter_phe_id2,filter_phe_id())
		}

		output$filter_phe_id_info = renderPrint({
			cat(paste0("Tip: ", length(cancer_choose$filter_phe_id), " samples are retained"))
		})
	})

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2comp_batch")


	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2comp_batch")


	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2comp_batch",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(cancer_choose$filter_phe_id),
						   custom_metadata=reactive(custom_meta()),
						   opt_pancan = reactive(opt_pancan())
						   )



	# 
	id_list_custom = reactive({

		id_list$Custom_metadata = list(
			all = sort(colnames(custom_meta()[-1])),
			default = sort(colnames(custom_meta()[-1]))[1])
		id_list
	})


	observe({
	  updateSelectizeInput(
	    session,
	    "L3_x",
	    choices = id_list_custom()[[input$L2_x]]$all,
	    selected = id_list_custom()[[input$L2_x]]$default,
	    server = TRUE
	  )
	})

	observeEvent(input$L3_x_type, {
	  updateTabsetPanel(inputId = "L3_x_type_tab", 
	  	selected = input$L3_x_type)
	}) 


	output$dw_L3_x = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			all_ids = id_list_custom()[[input$L2_x]]$all
			sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
			write.table(sample_ids, file,
				quote = F, row.names = F, col.names = F)
		}
	)

	#  多分子数据
	L3s_x = reactive({
		if(input$L3_x_type=="Selection"){
			L3s_x = input$L3_x
		} else if (input$L3_x_type=="File"){
			file = input$upload_sp_info
			all_ids = id_list_custom()[[input$L2_x]]$all
			if(is.null(file$datapath)){  # 如果空文件
				set.seed(42)
				sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
				L3s_x = sample_ids
			} else {
				L3s_x = read.table(file$datapath)[,1]
				L3s_x = L3s_x[L3s_x %in% all_ids]
				if(length(input$L2_x)>100 & input$L2_x %in% id_category[["Molecular_profile"]]){
					L3s_x = L3s_x[1:100]
				}
			}
		} else if (input$L3_x_type=="All"){
			L3s_x = id_list_custom()[[input$L2_x]]$all
			if(input$L2_x %in% id_category[["Molecular_profile"]]){
				set.seed = 42
				L3s_x = sample(L3s_x, 100)
			}
		}
		L3s_x = L3s_x[L3s_x %in% id_list_custom()[[input$L2_x]]$all]
		L3s_x
	})

	L3s_x_data = eventReactive(input$inspect_data_x, {
		L1_x = names(id_category)[sapply(id_category, function(x){any(x %in% input$L2_x)})]
		withProgress(message = "Your provided ids are being inspected and prepared. Please wait for a while.",{
			x_data_merge = lapply(seq(L3s_x()), function(i){
				# 进度提醒
			    incProgress(1 / length(L3s_x()), detail = paste0("(Run ",i,"/",length(L3s_x()),")"))

				L3_x = L3_x = L3s_x()[i]
				x_data = batch_download(L1_x, input$L2_x, L3_x,
							   tumor_index_list, tcga_TIL, tcga_PW, opt_pancan())
				x_data = x_data %>%
					dplyr::inner_join(load_data("tcga_clinical")[,c("sample","type")]) %>%
					dplyr::filter(type %in% cancer_choose$name) %>%
					dplyr::filter(sample %in% cancer_choose$filter_phe_id) %>%
				    dplyr::select(id, sample, value)
			}) %>% do.call(rbind, .)
			x_data_merge
		})
	})
	L3s_x_tip = eventReactive(input$inspect_data_x, {
		paste0("Tip: ",length(L3s_x())," valid ids are successfully provided.\n")
	})
	output$L3s_x_tip = renderPrint({
		cat(L3s_x_tip())
	})

	output$L3s_x_data.ui = renderUI({
		output$L3s_x_data = renderDataTable({
			datatable(L3s_x_data(),
				# class = "nowrap row-border",
				options = list(pageLength = 5, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			) %>%
				formatRound(columns = c("value"), digits = 3)

		})
	dataTableOutput(ns("L3s_x_data"))
	})


	## 比较分析
	comp_stat = eventReactive(input$cal_batch_comp, {
		shiny::validate(
			need(try(nrow(group_final())>0), 
				"Please inspect whether to set groups in S2 step."),
		)
		withProgress(message = "Your analyzation has been submitted. Please wait for a while.",{
			comp_stat = lapply(seq(L3s_x()), function(i){
				# i = 1
			    incProgress(1 / length(L3s_x()), detail = paste0("(Finished ",i,"/",length(L3s_x()),")"))

				L3_x = L3s_x()[i]

				y_data = L3s_x_data() %>%
					dplyr::filter(id == L3_x) %>% 
					dplyr::select(sample, value)

				group_data = group_final()[,c(1,4,5)]
				colnames(group_data) = c("sample","group","phenotype")
				data = dplyr::inner_join(y_data, group_data) %>%
					dplyr::select(sample, value, group, everything()) %>% na.omit()
				# 检查数据是否合理
				if(nrow(data)==0 | sd(data$value)==0 | length(unique(data$group))==1) return(c(NaN, NaN, NaN))
				
				if(input$comp_method == "t.test"){
					comp_obj = t.test(value ~ group, data)
				} else if (input$comp_method == "wilcox.test"){
					comp_obj = wilcox.test(value ~ group, data)
				}
				means = data %>%
				  dplyr::group_by(group) %>% 
				  dplyr::summarise(value=mean(value)) %>% 
				  dplyr::pull(value)

				comp_res = c(means, comp_obj$p.value)
				names(comp_res) = c(levels(data$group),"p.value")
				comp_res
			}) %>% do.call(rbind, .) %>% as.data.frame()
			comp_stat = comp_stat %>% 
			  dplyr::mutate(id = L3s_x(), .before = 1)
			comp_stat
		})
	})

	output$comp_stat_tb.ui = renderUI({
		output$comp_stat_tb = renderDataTable({
			comp_stat_ = comp_stat()
			comp_stat_$p.value = format(comp_stat_$p.value, scientific=T, digits = 3)

			datatable(comp_stat_,
				# class = "nowrap row-border",
				options = list(pageLength = 5, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			) %>%
				formatRound(columns = levels(group_final()$group), digits = 3)
		}) 
	dataTableOutput(ns("comp_stat_tb"))
	})

	output$comp_stat_dw.ui = renderUI({
		fluidRow(
			column(6,downloadButton(ns("comp_batch_raw"), "Save raw date(.csv)")),
			column(6,downloadButton(ns("comp_batch_res"), "Save result data(.csv)"))
		)
	})
	output$comp_batch_raw = downloadHandler(
		filename = function(){
			paste0("Batch_comparison_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			group_data = group_final()[,c(1,4,5)]
			colnames(group_data) = c("sample","group","phenotype")

			data = L3s_x_data() %>%
				dplyr::mutate(level2=input$L2_x) %>%
				dplyr::inner_join(group_data) %>% na.omit()

			write.csv(data, file, row.names = FALSE)
		}
	)
	output$comp_batch_res = downloadHandler(
		filename = function(){
			paste0("Batch_comparison_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			comp_stat_ = comp_stat()
			write.csv(comp_stat_, file, row.names = FALSE)
		}
	)
}