ui.modules_pancan_comp_o2m = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),
					h4("1. Modify datasets[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Set molecular profile origin", 
					                   content = "data_origin"),
					mol_origin_UI(ns("mol_origin2comp"), database = "toil"),

					h4("2. Choose cancer(s)"),
					pickerInput(
						ns("choose_cancers"), NULL,
						choices = sort(tcga_cancer_choices),
						multiple = TRUE,
						selected = sort(tcga_cancer_choices),
						options = list(`actions-box` = TRUE)
					),

				    br(),br(),

					h4("3. Filter samples[opt]") %>% 
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
					filter_samples_UI(ns("filter_samples2comp")),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),br(),

					h4("4. Upload metadata[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Upload sample info", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2comp")),
					br(),

					h4("5. Add signature[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Add molecular signature", 
					                   content = "add_signature"),
					add_signature_UI(ns("add_signature2comp"))

				)
			),
			# 分组设置
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					# 调用分组模块UI
					group_samples_UI(ns("group_samples2comp"), database = "toil"),  
					# 下载待比较数据
					download_feat_UI(ns("download_y_axis"), button_name="Query variable to compare", database = "toil")

				)
			),
			# 分析/绘图/下载
			column(
				5,
				wellPanel(
					h2("S4: Analyze", align = "center"),
					style = "height:1100px",

					shinyWidgets::actionBttn(
						ns("step3_plot_line"), "Go/Update line plot",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					selectInput(ns("comp_method"), "Comparison metohd",choices = c("t-test", "wilcoxon")),
					br(),

					column(3, colourpicker::colourInput(inputId = ns("group_1_color_2"), "Color (group-1)", "#E69F00")),
					column(3, colourpicker::colourInput(inputId = ns("group_2_color_2"), "Color (group-2)", "#56B4E9")),
					column(6, radioButtons(inputId = ns("significance"), label = "Significance", 
						choices = c("Value", "Symbol"), selected="Symbol",inline = TRUE)),

					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("comp_plot_line")}, height = "500px") 
						)
					),

				    br(),
				    fluidRow(
				    	column(3, downloadButton(ns("save_plot_bt"), "Figure")),
				    	column(3, offset = 0, downloadButton(ns("save_data_raw"), "Raw data(.csv)")),
				    	column(3, offset = 1, downloadButton(ns("save_data_res"), "Analyzed data(.csv)"))
				    ),

				    br(),
				    fluidRow(
				    	column(2, p("Plot Height:")),
				    	column(3, numericInput(ns("save_plot_H"), NULL ,min = 1, max = 20, value = 10, step = 0.5)),
				    	column(2, p("Plot Width:")),
				    	column(3, numericInput(ns("save_plot_W"), NULL, min = 1, max = 20, value = 10, step = 0.5)),
				        column(
				        	2,
					        prettyRadioButtons(
					          inputId = ns("save_plot_F"),
					          label = NULL,
					          choices = c("pdf", "png"),
					          selected = "pdf",
					          inline = TRUE,
					          icon = icon("check"),
					          animation = "jelly",
					          fill = TRUE
					        )
				        )
				    )
				)
			)
		)
	)
}


server.modules_pancan_comp_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", 
		phe_primary=query_tcga_group(database = "toil",cancer = "BRCA", return_all = T),
		filter_phe_id=NULL, single_cancer_ok = TRUE)
	observe({
		cancer_choose$name = input$choose_cancers
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
						     check_numeric=TRUE
							 )

	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_line = eventReactive(input$step3_plot_line, {
		group_data = group_final()[,c(1,3,4)]
		colnames(group_data) = c("sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, sample, value, group, everything())
		data
	})


	# 仍需要检查数据，因为即使二分组本身成立，但可能样本缺失variable数据导致仍然只有一个分组
	observe({
		cancer_choose$multi_cancer_ok = 
		    merge_data_line() %>%
		      dplyr::group_by(cancer, group) %>%
		      dplyr::summarise(n1=n()) %>%
		      dplyr::filter(n1>=2) %>% # 每小组的样本数大于等于2
		      dplyr::distinct(cancer, group) %>%
		      dplyr::count(cancer,name = "n2") %>%
		      dplyr::filter(n2==2) %>% dplyr::pull("cancer") # 每个肿瘤有两组
	})
	comp_data_line = eventReactive(input$step3_plot_line, {
		merge_data_line = merge_data_line()
		comp_method = switch(isolate(input$comp_method),
			`t-test` = "parametric", wilcoxon = "nonparametric")
		valid_cancer_choose = cancer_choose$multi_cancer_ok
		stat_comp = lapply(sort(valid_cancer_choose), function(tcga_type){
		  p = ggbetweenstats(
		    subset(merge_data_line, cancer==tcga_type),
		  	x = "group",
		  	y = "value",
		    type = comp_method)
		  extract_stats(p)$subtitle_data
		}) %>% do.call(rbind, .) %>% 
		dplyr::select(!expression) %>% 
		dplyr::mutate(cancer = sort(valid_cancer_choose), .before=1)
	})


	comp_plot_line = eventReactive(input$step3_plot_line, {
		shiny::validate(
			need(try(nrow(merge_data_line())>0), 
				"Please inspect whether to set groups or download variable data in S2 or S3 step."),
		)
		merge_data_line_sub = merge_data_line() %>%
			dplyr::filter(cancer %in% unique(merge_data_line()$cancer))

		p1 = ggplot(merge_data_line_sub) + 
		  stat_summary(aes(x=cancer, y=value, color=group),
		               position=position_dodge(width=0.5)) + 
		  xlab("") + ylab("TP53") + 
		  ggplot2::scale_color_manual(values = c(isolate(input$group_1_color_2), isolate(input$group_2_color_2))) +
		  coord_flip() +
		  theme_minimal() +
		  theme(legend.position = "top",
		        plot.margin = margin(0,0,0,0),
		        text = element_text(size=15))

		if(isolate(input$significance)=="Value"){
			p2 = comp_data_line() %>% ggplot() + 
			  geom_text(aes(label=formatC(p.value, format = "e", digits = 2),
			                x=cancer,y=1),) +
			  coord_flip()
		} else if (isolate(input$significance)=="Symbol"){
			p2 = comp_data_line() %>%
			  dplyr::mutate(p.label=case_when(
			    p.value < 0.001 ~ "***",
			    p.value < 0.01 ~ "**",
			    p.value < 0.05 ~ "*",
			    TRUE ~ "ns"
			  )) %>% ggplot() + 
			  geom_text(aes(label=p.label, x=cancer,y=1),) +
			  coord_flip()
		}
		p2 = p2 +
		  theme_minimal() +
		  theme(axis.ticks = element_blank(),
		        axis.text = element_blank(),
		        axis.line = element_blank(),
		        axis.title = element_blank(),
		        axis.ticks.length.y = unit(0,"pt"),
		        plot.margin = margin(0,0,0,0)) +
		  theme(panel.grid.major.x = element_blank(),
		        panel.grid.minor.x = element_blank())
		
		p = p1 + p2 + patchwork::plot_layout(widths = c(5,1))

		return(p)
	})


	output$comp_plot_line = renderPlot({comp_plot_line()})


	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("LinePlot", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = comp_plot_line()
			
		    if (input$save_plot_F == "pdf") {
		      pdf(file, width = input$save_plot_W, height = input$save_plot_H)
		      print(p)
		      dev.off()
		    } else if (input$save_plot_F == "png"){
		      png(file, width = input$save_plot_W, height = input$save_plot_H, res = 600, units = "in")
		      print(p)
		      dev.off()
		    }
		}
	)
	output$save_data_raw = downloadHandler(
		filename = function(){
			paste0("Compare_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = merge_data_line()
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Compare_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_comp = comp_data_line()
			p_comp$identifier = unique(merge_data_line()$id)
			p_comp$phenotype = unique(merge_data_line()$phenotype)	
			p_comp$group_1 = levels(merge_data_line()$group)[1]
			p_comp$group_2 = levels(merge_data_line()$group)[2]
			write.csv(p_comp, file, row.names = FALSE)
		}
	)
}