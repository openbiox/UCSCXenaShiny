ui.modules_pancan_cor_o2m = function(id) {
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
					mol_origin_UI(ns("mol_origin2cor"), database = "toil"),

					h4(strong("S1.2 Choose cancers")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Cancer types", 
					                   content = "tcga_types"),
					pickerInput(
						ns("choose_cancers"), NULL,
						choices = sort(tcga_names),
						multiple = TRUE,
						selected = sort(tcga_names),
						options = list(`actions-box` = TRUE)
					),

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
					filter_samples_UI(ns("filter_samples2cor"), database = "toil"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),

					h4(strong("S1.4 Upload metadata"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Upload metadata", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2cor")),
					br(),

					h4(strong("S1.5 Add signature"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Add signature", 
					                   content = "add_signature"),
					add_signature_UI(ns("add_signature2cor"), database = "toil"),
				)
			),
			# 下载X轴数据
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					# 调用下载模块UI
					h4(strong("S2.1 Get data for X-axis")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Get one data", 
					                   content = "get_one_data"), 
					download_feat_UI(ns("download_x_axis"), button_name="Query", database = "toil"),
		            # br(),
		            h4(strong("S2.2 Get data for Y-axis")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Get one data", 
					                   content = "get_one_data"), 
					download_feat_UI(ns("download_y_axis"), button_name="Query", database = "toil"), 
				)
			),
			# 分析/绘图/下载
			column(
				5,
				wellPanel(
					h2("S3: Analyze & Visualize", align = "center") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Analyze & Visualize", 
					                   content = "analyze_cor_2"),  
					style = "height:1100px",
					h4(strong("S3.1 Set analysis parameters")), 
					# br(),
					selectInput(ns("cor_method"), "Correlation method:",choices = c("Spearman","Pearson"), selected="Spearman"),
					shinyWidgets::actionBttn(
						ns("step3_plot_bar_1"), "Run (Calculate)",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					verbatimTextOutput(ns("message1")),
					h4(strong("S3.2 Set visualization parameters")), 
					fluidRow(
						column(3, colourpicker::colourInput(inputId = ns("positive_color"), "Positive color:", "#d53e4f")),
						column(3, colourpicker::colourInput(inputId = ns("negative_color"), "Negative color:", "#3288bd"))
					),
					dropMenu(
						actionBttn(ns("more_visu"), label = "Other options", style = "bordered",color = "success",icon = icon("bars")),
						div(h3("1. Select ggplot theme:"),style="width:400px;"),
						fluidRow(
							column(6,
								selectInput(inputId = ns("theme"), label = NULL, 
											choices = names(themes_list), selected = "Minimal")
							)
						),
						div(h3("2. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size"), label = "Text size:", value = 18, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size"), label = "Title size:", value = 20, step = 0.5)),
							column(4, numericInput(inputId = ns("label_size"), label = "Label size:", value = 5, step = 0.5)),
						),				
						div(h3("3. Adjust lab and title name:"),style="width:400px;"),
						fluidRow(
							column(6, textInput(inputId = ns("x_name"), label = "X-axis name:", 
								value = "estimate coefficient")),
							column(6, textInput(inputId = ns("title_name"), label = "Title name:",
								value = NULL))
						),	
						div(h5("Note: You can download the raw data and plot in local R environment for more detailed adjustment.")),
					),					
					br(),
					shinyWidgets::actionBttn(
						ns("step3_plot_bar_2"), "Run (Visualize)",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("cor_plot_bar")}, height = "470px") 
						)
					),
					br(),
					h4(strong("S3.3 Download results")), 
				    download_res_UI(ns("download_res2cor"))
				)
			)
		)
	)
}


server.modules_pancan_cor_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "ACC", phe_primary="",
		filter_phe_id=query_tcga_group(database = "toil", cancer = "BRCA", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
		cancer_choose$phe_primary <- query_tcga_group(database = "toil", cancer = cancer_choose$name, return_all = T)
	})
	

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2cor", database = "toil")

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2cor", database = "toil")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2cor", database = "toil")

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
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2cor",
					   database = "toil",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta_sig()),
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


	## x-axis data
	x_axis_data = callModule(download_feat_Server, "download_x_axis", 
							 database = "toil",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     check_numeric=TRUE
							 )
	## y-axis data
	y_axis_data = callModule(download_feat_Server, "download_y_axis", 
							 database = "toil",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     check_numeric=TRUE
							 )

	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_bar = eventReactive(input$step3_plot_bar_1, {
		message("Start merging data...")
		x_axis_data = x_axis_data()
		colnames(x_axis_data)[c(1:3,5)] = paste0("x_",colnames(x_axis_data)[c(1:3,5)])
		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])
		data = dplyr::inner_join(x_axis_data, y_axis_data) %>%
			dplyr::select(cancer, Sample, everything())
		
		data <- data %>%
			dplyr::filter(cancer %in% (
				data %>%
					dplyr::count(cancer) %>%
					dplyr::filter(n > 2) %>%
					dplyr::pull(cancer)
			))
		# print(head(data))
		data
	})

	cor_data_bar = eventReactive(input$step3_plot_bar_1, {
		shinyjs::disable("step3_plot_bar_1")
		merge_data_bar = merge_data_bar()
		cor_method = switch(isolate(input$cor_method),
			Pearson = "parametric", Spearman = "nonparametric")
		valid_cancer_choose = sort(unique(merge_data_bar$cancer))

		withProgress(message = "Please wait for a while.",{
			stat_cor = lapply(seq(valid_cancer_choose), function(i){
			  tcga_type = valid_cancer_choose[i]
			  p = ggscatterstats(
			    subset(merge_data_bar, cancer==tcga_type),
			    x = "x_value",
			    y = "y_value",
			    type = cor_method)
			  incProgress(1 / length(valid_cancer_choose), detail = paste0("(Finished ",i,"/",length(valid_cancer_choose),")"))
			  return(extract_stats(p)$subtitle_data)
			}) %>% do.call(rbind, .) %>% 
			dplyr::select(!expression) %>% 
			dplyr::mutate(cancer = valid_cancer_choose, .before=1)
			stat_cor
		})
		shinyjs::enable("step3_plot_bar_1")
		stat_cor
	})
	output$message1 = renderPrint({
		req(cor_data_bar())
		shiny::validate(
			need(try(nrow(cor_data_bar())>0), 
				"Please inspect whether to download valid data in S2 step."),
		)
		cat(paste("The calculation has been successfully completed! (",format(Sys.time(), "%H:%M:%S"),")"))
	})

	cor_plot_bar = eventReactive(input$step3_plot_bar_2, {
		# shiny::validate(
		# 	need(try(input$step3_plot_bar_1>0), 
		# 		"Please run the calculation step (S3.1) above."),
		# )
		p = plot_cor_o2m(
			data=cor_data_bar(), label_size=input$label_size, x_name=input$x_name, title_name=input$title_name,
			negative_color=input$negative_color, positive_color=input$positive_color, axis_size=input$axis_size, 
			title_size=input$title_size,
			custom_theme = themes_list[[input$theme]]
		)
		return(p)
	})



	output$cor_plot_bar = renderPlot({cor_plot_bar()})

	# Download results
	observeEvent(input$step3_plot_bar_2,{
		res1 = cor_plot_bar()
		res2 = merge_data_bar()

		p_cor = cor_data_bar()
		p_cor$parameter1 = unique(merge_data_bar()$x_axis)
		p_cor$parameter2 = unique(merge_data_bar()$y_axis)	
		res3 = p_cor
		callModule(download_res_Server, "download_res2cor", res1, res2, res3)
	})
}

