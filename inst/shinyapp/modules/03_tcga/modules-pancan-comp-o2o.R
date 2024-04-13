ui.modules_pancan_comp_o2o = function(id) {
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
					mol_origin_UI(ns("mol_origin2comp"), database = "toil"),

					h4(strong("S1.2 Choose cancer")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Cancer types", 
					                   content = "tcga_types"),
					pickerInput(
						ns("choose_cancer"), NULL,
						choices = sort(tcga_names),
						selected = "BRCA"),
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
					filter_samples_UI(ns("filter_samples2comp"), database = "toil"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),

					h4(strong("S1.4 Upload metadata"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Upload metadata", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2comp")),
					br(),
					
					h4(strong("S1.5 Add signature"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Add signature", 
					                   content = "add_signature"),
					add_signature_UI(ns("add_signature2comp"), database = "toil"),
				)
			),
			# 分组设置
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					h4(strong("S2.1 Divide 2 groups by one condition")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Divide 2 groups", 
					                   content = "set_groups"),
					# 调用分组模块UI
					group_samples_UI(ns("group_samples2comp"), database = "toil"),
					h4(strong("S2.2 Get data for comparison")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Get one data", 
					                   content = "get_one_data"), 
					# 下载待比较数据
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
					                   content = "analyze_comp_1"),  
					style = "height:1100px",
					h4(strong("S3.1 Set analysis parameters")), 
					selectInput(ns("comp_method"), "Comparison method:",choices = c("t-test", "wilcoxon")),

					h4(strong("S3.2 Set visualization parameters")), 
					fluidRow(
						column(3, colourpicker::colourInput(inputId = ns("group_1_color"), "Color (Group 1):", "#E69F00")),
						column(3, colourpicker::colourInput(inputId = ns("group_2_color"), "Color (Group 2):", "#56B4E9")),
					),
					dropMenu(
						actionBttn(ns("more_visu"), label = "Other options", style = "bordered",color = "success",icon = icon("bars")),
						div(h3("1. Select ggplot theme:"),style="width:400px;"),
						fluidRow(
							column(6,
								selectInput(inputId = ns("theme"), label = NULL, 
											choices = names(themes_list), selected = "ggstatplot")
							)
						),
						div(h3("2. Adjust points:"),style="width:400px;"),
						fluidRow(
							column(3, numericInput(inputId = ns("point_size"), label = "Point size:", value = 3, step = 0.5)),
							column(3, numericInput(inputId = ns("point_alpha"), label = "Point alpha:", value = 0.4, step = 0.1, min = 0, max = 1)),
						),
						div(h3("3. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size"), label = "Text size:", value = 18, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size"), label = "Title size:", value = 20, step = 0.5))
						),				
						div(h3("4. Adjust lab and title name:"),style="width:400px;"),
						fluidRow(
							column(4, textInput(inputId = ns("x_name"), label = "X-axis name:")),
							column(4, textInput(inputId = ns("y_name"), label = "Y-axis name:")),
							column(4, textInput(inputId = ns("title_name"), label = "Title name:"))
						),	
						div(h5("Note: You can download the raw data and plot in local R environment for more detailed adjustment.")),

					),
					br(),
					# verbatimTextOutput(ns("tmp123")),
					shinyWidgets::actionBttn(
						ns("step3_plot_box"), "Run",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("comp_plot_box")}, height = "480px") 
						)
					),
					br(),
					h4(strong("S3.3 Download results")), 
					download_res_UI(ns("download_res2comp"))
				)
			)
		)
	)
}


server.modules_pancan_comp_o2o = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", 
		phe_primary=query_tcga_group(database = "toil",cancer = "BRCA", return_all = T),
		filter_phe_id=NULL, single_cancer_ok = TRUE, multi_cancer_ok=FALSE)
	observe({
		cancer_choose$name = input$choose_cancer
		cancer_choose$phe_primary <- query_tcga_group(database = "toil",cancer = cancer_choose$name, return_all = T)
	})

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2comp", database = "toil")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2comp", database = "toil")

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

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2comp", database = "toil")

	## 过滤样本
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2comp",
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


	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2comp",
					   	   database = "toil",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(cancer_choose$filter_phe_id),
						   custom_metadata=reactive(custom_meta_sig()),
						   opt_pancan = reactive(opt_pancan())
						   )

	# 下载待比较数据
	y_axis_data = callModule(download_feat_Server, "download_y_axis", 
							 database = "toil",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     check_numeric=TRUE,
						     table.ui=FALSE
							 )


	# 合并分析
	# boxviolin逻辑：先绘图，再提取相关性结果
	merge_data_box = eventReactive(input$step3_plot_box, {
		group_data = group_final()[,c(1,3,4)]
		colnames(group_data) = c("Sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, Sample, value, group, everything())
		data
	})

	# 检查数据
	observe({
		cancer_choose$single_cancer_ok = min(table(merge_data_box()$group))>=3
	})


	observe({
		updateTextInput(session, "x_name", value = "group")
		updateTextInput(session, "y_name", value = unique(y_axis_data()$id))
		updateTextInput(session, "title_name", value = cancer_choose$name)
	})


	comp_plot_box = eventReactive(input$step3_plot_box, {
		shiny::validate(
			need(try(nrow(merge_data_box())>0), 
				"Please inspect whether to set groups or download variable data in S2 or S3 step."),
		)
		shiny::validate(
			need(try(length(unique(merge_data_box()$group))==2), 
				"No enough samples for comparing, please check your input."),
		)
		merge_data_box = merge_data_box()

		if(!cancer_choose$single_cancer_ok){
			return("No enough samples for comparing, please check your input.")
		} else {
			p = plot_comb_o2o(
				data = merge_data_box(), xlab=input$x_name, ylab=input$y_name, title=input$title_name,
				comp_method=input$comp_method, point_size=input$point_size, point_alpha=input$point_alpha,
				group_1_color=input$group_1_color, group_2_color=input$group_2_color,
				axis_size=input$axis_size, title_size=input$title_size,
				custom_theme=themes_list[[input$theme]]
			)
			return(p)
		}
	})
	output$comp_plot_box = renderPlot({comp_plot_box()})

	# Download results
	observeEvent(input$step3_plot_box,{
		res1 = comp_plot_box()
		res2 = merge_data_box()
		p_comp = extract_stats(comp_plot_box())$subtitle_data
		p_comp = p_comp[-which(colnames(p_comp)=="expression")]
		p_comp$identifier = unique(merge_data_box()$id)
		p_comp$phenotype = unique(merge_data_box()$phenotype)
		p_comp$group_1 = levels(merge_data_box()$group)[1]
		p_comp$group_2 = levels(merge_data_box()$group)[2]
		res3 = p_comp
		callModule(download_res_Server, "download_res2comp", res1, res2, res3)
	})
}