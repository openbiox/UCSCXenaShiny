ui.modules_pcawg_sur_m2o = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					h4(strong("S1.1 Modify datasets"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Modify datasets", 
					                   content = "data_origin"),
					mol_origin_UI(ns("mol_origin2sur"), database = "pcawg"),

					h4(strong("S1.2 Choose project")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "PCAWG projects", 
					                   content = "pcawg_projects"),
					pickerInput(
						ns("choose_cancer"),NULL,
						choices = pcawg_items),
					br(),

					h4(strong("S1.3 Filter samples"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Filter samples", 
					                   content = "choose_samples"),
					h5("Quick filter:"),
					pickerInput(
						ns("filter_by_code"), NULL,
						choices = NULL, selected =  NULL,
						multiple = TRUE, options = list(`actions-box` = TRUE)
					),
					h5("Exact filter:"),
					filter_samples_UI(ns("filter_samples2sur"), database = "pcawg"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),

					h4(strong("S1.4 Upload metadata"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Upload metadata", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2sur")),
					br(),

					h4(strong("S1.5 Add signature"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Add signature", 
					                   content = "add_signature"),
					add_signature_UI(ns("add_signature2sur"), database = "pcawg")
				)
			),	
			column(
				4,
				wellPanel(
					style = "height:1100px",

					h2("S2: Get data", align = "center"),
					h4(strong("S2.1 Select survival endpoint")), 
					p("Only OS (Overall Survial) is supported."),
				    br(),

					h4(strong("S2.2 Divide 2 groups by batch conditions")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Divide 2 groups by batch conditions", 
					                   content = "get_batch_data_set_groups"), 
					multi_upload_UI(ns("multi_upload2sur"),	button_name="Observe",database = "pcawg"),
				    # uiOutput(ns("L3s_x_data_sur.ui")),


					# br(),
					uiOutput(ns("set_quantile.ui")),
					uiOutput(ns("set_group1.ui")),
					uiOutput(ns("set_group2.ui")),
					shinyWidgets::actionBttn(
						ns("set_group"), "Group",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					# uiOutput(ns("L3s_x_data_sur_group.ui"))
				    div(uiOutput(ns("L3s_x_data_sur_group.ui")),
				      style = "margin-top: 5px; margin-bottom: 0px;"
				    ),
				)
			),	
			column(
				5,
				wellPanel(
					style = "height:1100px",
					h2("S3: Analyze", align = "center") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Analyze", 
					                   content = "analyze_sur_3"),
					# br(),
					h4(strong("S3.1 Set analysis parameters")), 
					# br(),br(),
					selectInput(ns("sur_method"), "Survival method:",c("Log-rank test","Cox regression")),
				    materialSwitch(ns("use_origin"), 
					    	"Whether use initial data before grouping?") %>% 
							helper(type = "markdown", size = "m", fade = TRUE, 
						                   title = "About the initial phenotype", 
						                   content = "sur_initial_group"),
					br(),br(),
					shinyWidgets::actionBttn(
						ns("cal_batch_sur"), "Run",
				        style = "gradient",
				        icon = icon("table"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),br(),
					fluidRow(
						column(10, offset = 1,
							   div(uiOutput(ns("sur_stat_tb.ui")),style = "height:600px"),
							   )
					),
					h4(strong("S3.2 Download results")), 
					uiOutput(ns("sur_stat_dw.ui"))
				)
			)
		)
	)
}



server.modules_pcawg_sur_m2o = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BLCA-US", phe_primary="",
		filter_phe_id=query_tcga_group(database = "pcawg", cancer = "BLCA-US", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancer
		cancer_choose$phe_primary <- query_tcga_group(database = "pcawg",
			cancer = cancer_choose$name, return_all = T)
	})

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2sur", database = "pcawg")

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2sur", database = "pcawg")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2sur", database = "pcawg")
	custom_meta_sig = reactive({
		if(is.null(custom_meta())){
			return(sig_dat())
		} else {
			if(is.null(sig_dat())){
				return(custom_meta())
			} else {
				custom_meta_sig = dplyr::inner_join(custom_meta(),sig_dat())
				return(custom_meta_sig)
			}
		}
	})



	## 过滤样本
	# quick filter widget
	observe({
		code_types_valid = unique(cancer_choose$phe_primary$Type)
		updatePickerInput(
			session,
			"filter_by_code",
			choices = code_types_valid,
			selected =  code_types_valid
		)
	})
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2sur",
					   database = "pcawg",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta_sig()),
					   opt_pancan = reactive(opt_pancan()))

	# 综合上述二者
	observe({
		# quick filter
		filter_phe_id2 = cancer_choose$phe_primary %>%
			dplyr::filter(Type %in% input$filter_by_code) %>%
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


	# 批量下载数据
	L3s_x_data =  callModule(multi_upload_Server, "multi_upload2sur", 
							 database = "pcawg",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     table.ui = FALSE
							 )
	L3s_x = reactive({
		unique(L3s_x_data()$id)
	})

	L3s_x_data_sur = reactive({
		sur_dat_raw = pcawg_info[,c("icgc_specimen_id","OS","OS.time")]
		colnames(sur_dat_raw) = c("Sample","status","time")
		sur_dat_sub = sur_dat_raw %>%  dplyr::distinct() %>% na.omit()

		x_data_merge = L3s_x_data() %>%
			dplyr::inner_join(sur_dat_sub)
		x_data_merge
	})

	# 分组方式
	output$set_group1.ui = renderUI({
		choice_chrs = ifelse(input$inspect_data_x==0,0,L3s_x_data_sur()$value)
		fluidRow(
			if(class(choice_chrs)=="character"){
				column(
					8,
					selectInput(ns("group1_range"),"Group1 Range",
						sort(unique(choice_chrs), ,na.last = T), multiple=T)
				)
			} else {
				column(8,
					fluidRow(
						column(6,
							numericInput(ns("group1_min"),"Group1 [min",value=NA)),
						column(6,
							numericInput(ns("group1_max"),"max)",
								value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T))))
					)
				)
			}
			,
			column(
				4,
				textInput(ns("group1_name"),"Group Name", "Group1")
			)
		)
	})
	output$set_group2.ui = renderUI({
		choice_chrs = ifelse(input$inspect_data_x==0,0,L3s_x_data_sur()$value)
		fluidRow(
			if(class(choice_chrs)=="character"){
				column(
					8,
					selectInput(ns("group2_range"),"Group2 Range",
						sort(unique(choice_chrs), ,na.last = T), multiple=T)
				)
			} else {
				column(8,
					fluidRow(
						column(6,
							numericInput(ns("group2_min"),"Group2 [min",
								value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T)))),
						column(6,
							numericInput(ns("group2_max"),"max]",value=NA))
					)
				)
			}
			,
			column(
				4,
				textInput(ns("group2_name"),"Group Name", "Group2")
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


	w <- waiter::Waiter$new(id = ns("L3s_x_data_sur_group"), html = waiter::spin_hexdots(), color = "black")

	output$L3s_x_data_sur_group.ui = renderUI({
		output$L3s_x_data_sur_group = renderDataTable({
			w$show()
			group_stat = L3s_x_data_sur_group()
			group_stat = group_stat %>%
				dplyr::count(id, group) %>% 

			datatable(group_stat,
				# class = "nowrap row-border",
				options = list(pageLength = 2, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		})
	dataTableOutput(ns("L3s_x_data_sur_group"))
	})


	sur_stat = eventReactive(input$cal_batch_sur,{
		datas = L3s_x_data_sur_group()
		valid_ids = unique(datas$id)

		withProgress(message = "Please wait for a while.",{
			sur_stat = lapply(seq(valid_ids), function(i){
			    incProgress(1 / length(valid_ids), detail = paste0("(Finished ",i,"/",length(valid_ids),")"))
				valid_id = valid_ids[i]
				datas_sub = subset(datas, id %in% valid_id)
				if(input$sur_method=="Log-rank test"){
					if(!input$use_origin){
						datas_sub$Group = datas_sub$group
					} else {
						datas_sub$Group = datas_sub$value
						if(class(datas_sub$value)!="character"){
							res.cut <- surv_cutpoint(datas_sub, 
								time = "time", event = "status", variables = "value")
							groups_1_2 = datas_sub %>% 
								  dplyr::group_by(group) %>% 
								  dplyr::summarise(mean = mean(value)) %>% 
								  dplyr::arrange(mean) %>% 
								  dplyr::pull(group) %>% as.character()
							datas_sub$Group = ifelse(surv_categorize(res.cut)$value=="low", groups_1_2[1], groups_1_2[2])
							datas_sub$Group = factor(datas_sub$Group, levels=groups_1_2)
						}
					}
					surv_diff <- survdiff(Surv(time, status) ~ Group, data = datas_sub)
					pval = 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
					sur_res = summary(survfit(Surv(time, status) ~ Group, data = datas_sub))$table %>% 
						    as.data.frame() %>% dplyr::pull(median)
					sur_res = c(sur_res,pval)
					names(sur_res) = c(input$group1_name,input$group2_name,"p.value")
					names(sur_res)[1:2] = paste0(names(sur_res)[1:2],"\nmedian.time")
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
				dplyr::mutate(id = valid_ids, .before = 1) %>%
				dplyr::arrange(p.value)
			sur_stat
		})
	})

	output$sur_stat_tb.ui = renderUI({
		output$sur_stat_tb = renderDataTable({
			sur_stat_ = sur_stat() %>%
				dplyr::rename("Batch identifiers"="id")
			sur_stat_$p.value = format(sur_stat_$p.value, scientific=T, digits = 3)

			dt = datatable(sur_stat_,
				# class = "nowrap row-border",
				options = list(pageLength = 10, 
					columnDefs = list(
						list(className = 'dt-center', targets="_all"),
						list(orderable=TRUE, targets = 0)))
			)
			dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
			dt
		}) 
		dataTableOutput(ns("sur_stat_tb"))
	})

	output$sur_stat_dw.ui = renderUI({
		fluidRow(
			column(6,downloadButton(ns("sur_batch_raw"), "Raw data(.csv)")),
			column(6,downloadButton(ns("sur_batch_res"), "Result data(.csv)"))
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
			paste0("Batch_survival_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			sur_stat_ = sur_stat()
			write.csv(sur_stat_, file, row.names = FALSE)
		}
	)



}