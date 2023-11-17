ui.modules_pancan_sur_batch = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
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
					filter_samples_UI(ns("filter_samples2sur_batch")),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),br(),
					
					h4("3. Upload metadata[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Upload sample info", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2sur_batch")),
					br(),br(),

					h4("4. Modify datasets[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Set molecular profile origin", 
					                   content = "data_origin"),
					mol_origin_UI(ns("mol_origin2sur_batch"))
				)
			),
			column(
				3,
				wellPanel(
					style = "height:1000px",

					h2("S2: Select event and items", align = "center"),

					br(),
					shinyWidgets::actionBttn(
						ns("inspect_data_x"), "Query data",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),

				    shinyWidgets::prettyRadioButtons(
				        inputId = ns("endpoint_type"), label = "Endpoint type:",
				        choiceValues = c("OS", "DSS", "DFI", "PFI"),
				        choiceNames = c("OS", "DSS", "DFI", "PFI"),
				        # choiceNames = c("OS (Overall Survial)", "DSS (Disease-Specific Survival)", 
				        # 				"DFI (Disease-Free Interval)", "PFI (Progression-Free Interval)"),
				        selected = "OS",inline = TRUE
				    ),

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
					verbatimTextOutput(ns("L3s_x_tip")),
					br(),
					uiOutput(ns("L3s_x_data_sur.ui")),
				)
			),	
			column(
				3,
				wellPanel(
					style = "height:1000px",

					h2("S3: Group samples", align = "center"),

					br(),
					shinyWidgets::actionBttn(
						ns("set_group"), "Set groups",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					uiOutput(ns("set_quantile.ui")),
					uiOutput(ns("set_group1.ui")),
					uiOutput(ns("set_group2.ui")),
					br(),
					uiOutput(ns("L3s_x_data_sur_group.ui"))
				)
			),
			column(
				4,
				wellPanel(
					style = "height:1000px",
					h2("S4: Batch analyze", align = "center"),
					br(),
					shinyWidgets::actionBttn(
						ns("cal_batch_sur"), "Start calculation",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),br(),
					selectInput(ns("sur_method"), "Analysis method",c("Log-rank test","Cox regression")),
				    materialSwitch(ns("use_origin"), 
					    	"Whether use initial data before grouping") %>% 
							helper(type = "markdown", size = "m", fade = TRUE, 
						                   title = "About the initial phenotype", 
						                   content = "sur_initial_group"),
					br(),br(),
					uiOutput(ns("sur_stat_tb.ui")),
					br(),
					uiOutput(ns("sur_stat_dw.ui"))
				)
			)
		)
	)
}





server.modules_pancan_sur_batch = function(input, output, session) {
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2sur_batch",
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
	custom_meta = callModule(custom_meta_Server, "custom_meta2sur_batch")


	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2sur_batch")



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

	L3s_x_data_sur = eventReactive(input$inspect_data_x, {
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
		})
		sur_dat_raw = load_data("tcga_surv") %>%
			dplyr::select("sample",contains(input$endpoint_type)) %>%
			na.omit()
		colnames(sur_dat_raw)[2:3] = c("status","time")
		x_data_merge = x_data_merge %>%
			dplyr::inner_join(sur_dat_raw)
		x_data_merge
	})

	L3s_x_tip = eventReactive(input$inspect_data_x, {
		paste0("Tip: ",length(L3s_x())," valid ids are successfully provided.\n")
	})
	output$L3s_x_tip = renderPrint({
		cat(L3s_x_tip())
	})
	output$L3s_x_data_sur.ui = renderUI({
		output$L3s_x_data_sur = renderDataTable({
			L3s_x_data_sur_ = L3s_x_data_sur()
			colnames(L3s_x_data_sur_)[4:5] = paste0(input$endpoint_type,c(".status",".time"))
			L3s_x_data_sur_ = L3s_x_data_sur_[,c(-4,-5)]
			datatable(L3s_x_data_sur_, 
				options = list(pageLength = 5,
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		}) 
		dataTableOutput(ns("L3s_x_data_sur"))
	})

	# 分组方式
	output$set_group1.ui = renderUI({
		choice_chrs = ifelse(input$inspect_data_x==0,0,L3s_x_data_sur()$value)
		fluidRow(
			if(class(choice_chrs)=="character"){
				column(
					8,
					selectInput(ns("group1_range"),"Group-1 Range",
						sort(unique(choice_chrs), ,na.last = T), multiple=T)
				)
			} else {
				column(8,
					fluidRow(
						column(6,
							numericInput(ns("group1_min"),"Group-1 min",value=NA)),
						column(6,
							numericInput(ns("group1_max"),"max",
								value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T))))
					)
				)
			}
			,
			column(
				4,
				textInput(ns("group1_name"),"Group-1 Name", "Group1")
			)
		)
	})
	output$set_group2.ui = renderUI({
		choice_chrs = ifelse(input$inspect_data_x==0,0,L3s_x_data_sur()$value)
		fluidRow(
			if(class(choice_chrs)=="character"){
				column(
					8,
					selectInput(ns("group2_range"),"Group-2 Range",
						sort(unique(choice_chrs), ,na.last = T), multiple=T)
				)
			} else {
				column(8,
					fluidRow(
						column(6,
							numericInput(ns("group2_min"),"Group-2 min",
								value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T)))),
						column(6,
							numericInput(ns("group2_max"),"max",value=NA))
					)
				)
			}
			,
			column(
				4,
				textInput(ns("group2_name"),"Group-2 Name", "Group2")
			)
		)
	})
	output$set_quantile.ui = renderUI({
		choice_chrs = ifelse(input$inspect_data_x==0,0,L3s_x_data_sur()$value)
		if(length(choice_chrs)==0) choice_chrs = 1:100
		if(class(choice_chrs)!="character"){
			materialSwitch(ns("set_quantile"),"Whether group by percentile?",value = TRUE)
		}
	})

	L3s_x_data_sur_group = eventReactive(input$set_group,{
		datas = L3s_x_data_sur()
		choice_chrs = L3s_x_data_sur()$value

		merge_by = list(
			c(input$group1_min, input$group1_max),
			c(input$group2_min, input$group2_max)
		)
		names(merge_by) = c(input$group1_name, input$group2_name)

		if(class(datas$value)=="character"){
		    shiny::validate(
		      need(length(intersect(input$group1_range,input$group2_range))>0, 
		        "Please inspect whether to input independent group range.")
		    )
			datas$group = NA
			datas$group[choice_chrs %in% input$group1_range] = input$group1_name
			datas$group[choice_chrs %in% input$group2_range] = input$group2_name
			datas = na.omit(datas)
		} else if (input$set_quantile) {
			if(is.na(merge_by[[1]][1])) merge_by[[1]][1] = 0
			if(is.na(merge_by[[2]][1])) merge_by[[1]][1] = 0
			if(is.na(merge_by[[1]][2])) merge_by[[1]][1] = 1
			if(is.na(merge_by[[2]][2])) merge_by[[2]][2] = 1
			datas_split = split(datas, datas$id)
			datas = lapply(datas_split, function(data_id){
				choice_chrs_id = data_id$value
				merge_by_one = merge_by
		        for (i in seq(merge_by_one)){
		          merge_by_one[[i]] = quantile(choice_chrs_id, merge_by_one[[i]], na.rm = T)
		        }
				data_id$group = NA
				data_id$group[which(findInterval(choice_chrs_id, merge_by_one[[1]])==1)] = names(merge_by_one[1])
				data_id$group[which(findInterval(choice_chrs_id, merge_by_one[[2]], rightmost.closed = TRUE)==1)] = names(merge_by_one[2])
				data_id = na.omit(data_id)
			}) %>% do.call(rbind, .) %>% tibble::remove_rownames()
		} else {
			if(is.na(merge_by[[1]][1])) merge_by[[1]][1] = min(choice_chrs, na.rm=T)
			if(is.na(merge_by[[2]][1])) merge_by[[2]][1] = min(choice_chrs, na.rm=T)
			if(is.na(merge_by[[1]][2])) merge_by[[1]][2] = max(choice_chrs, na.rm=T)
			if(is.na(merge_by[[2]][2])) merge_by[[2]][2] = max(choice_chrs, na.rm=T)
			datas$group = NA
			datas$group[which(findInterval(choice_chrs, merge_by[[1]])==1)] = names(merge_by[1])
			datas$group[which(findInterval(choice_chrs, merge_by[[2]], rightmost.closed = TRUE)==1)] = names(merge_by[2])
			datas = na.omit(datas)
		}
		## 去除分组结果里只有一组的id
		valid_ids = datas %>%
			dplyr::distinct(id, group) %>%
			dplyr::count(id) %>%
			dplyr::filter(n==2) %>% dplyr::pull("id")
		datas_sub = datas %>% 
			dplyr::filter(id %in% valid_ids)
		datas_sub
	})

	output$L3s_x_data_sur_group.ui = renderUI({
		output$L3s_x_data_sur_group = renderDataTable({
			group_stat = L3s_x_data_sur_group()
			group_stat = group_stat %>%
				dplyr::count(id, group) %>% 

			datatable(group_stat,
				# class = "nowrap row-border",
				options = list(pageLength = 6, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		})
	dataTableOutput(ns("L3s_x_data_sur_group"))
	})


	sur_stat = eventReactive(input$cal_batch_sur,{
		datas = L3s_x_data_sur_group()
		valid_ids = unique(datas$id)

		sur_stat = lapply(seq(valid_ids), function(i){
			valid_id = valid_ids[i]
			datas_sub = subset(datas, id %in% valid_id)
			if(input$sur_method=="Log-rank test"){
				if(!input$use_origin){
					datas_sub$Group = datas_sub$group
				} else {
					datas_sub$Group = datas_sub$value
					if(class(datas_sub$value)!="character"){
						res.cut <- surv_cutpoint(datas_sub, 
							time = "time", event = "status", variables = "Group")
						res.cat <- surv_categorize(res.cut)
						datas_sub$Group = res.cat$Group
					}
				}
				surv_diff <- survdiff(Surv(time, status) ~ Group, data = datas_sub)
				pval = 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
				sur_res = summary(survfit(Surv(time, status) ~ Group, data = datas_sub))$table %>% 
					    as.data.frame() %>% dplyr::pull(median)
				sur_res = c(sur_res,pval)
				names(sur_res) = c(input$group1_name,input$group2_name,"p.value")
				names(sur_res)[1:2] = paste0(names(sur_res)[1:2],"_median.time")
			} else if (input$sur_method=="Cox regression"){
				if(!input$use_origin){
					datas_sub$Group = datas_sub$group
				} else {
					datas_sub$Group = datas_sub$value
				}
				fit <- coxph(Surv(time, status) ~ Group, data = datas_sub)
				sur_res = summary(fit)$coefficients %>% as.data.frame() %>%
					tibble::rownames_to_column("Group.obs")
				sur_res = sur_res[,c(1,3,6)]
				sur_res[,1] = gsub("^Group","",sur_res[,1])
				sur_res[,2] = round(sur_res[,2], 3)
				colnames(sur_res)[2:3] = c("HR","p.value")
			}
			sur_res
		}) %>% do.call(rbind, .)
		sur_stat = sur_stat %>% as.data.frame() %>%
			dplyr::mutate(id = valid_ids, .before = 1)
		sur_stat
	})
	output$sur_stat_tb.ui = renderUI({
		output$sur_stat_tb = renderDataTable({

			sur_stat_ = sur_stat()
			sur_stat_$p.value = format(sur_stat_$p.value, scientific=T, digits = 3)

			datatable(sur_stat_,
				# class = "nowrap row-border",
				options = list(pageLength = 5, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		}) 
	dataTableOutput(ns("sur_stat_tb"))
	})

	output$sur_stat_dw.ui = renderUI({
		fluidRow(
			column(6,downloadButton(ns("sur_batch_raw"), "Save raw date(.csv)")),
			column(6,downloadButton(ns("sur_batch_res"), "Save result data(.csv)"))
		)
	})

	output$sur_batch_raw = downloadHandler(
		filename = function(){
			paste0("Batch_survival_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			datas = L3s_x_data_sur_group()
			write.csv(datas, file, row.names = FALSE)
		}
	)
	output$sur_batch_res = downloadHandler(
		filename = function(){
			paste0("Batch_comparison_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			sur_stat_ = sur_stat()
			write.csv(sur_stat_, file, row.names = FALSE)
		}
	)
}



#         id          sample value status time  group
# 1 ATP6V1B1 TCGA-C8-A1HL-01 3.660      0  317 Group2
# 2 ATP6V1B1 TCGA-EW-A2FS-01 2.057      0 1604 Group1
# 3 ATP6V1B1 TCGA-B6-A402-01 4.793      0 2281 Group2
# 4 ATP6V1B1 TCGA-A2-A3XX-01 3.254      1 1439 Group2
# 5 ATP6V1B1 TCGA-BH-A0BQ-11 4.131      0 2255 Group2
# 6 ATP6V1B1 TCGA-Z7-A8R5-01 4.114      0 3287 Group2