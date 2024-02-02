ui.modules_pcawg_cor_o2m = function(id) {
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
					mol_origin_UI(ns("mol_origin2cor"), database = "pcawg"),

					h4(strong("S1.2 Choose projects")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "PCAWG projects", 
					                   content = "pcawg_projects"),
					pickerInput(
						ns("choose_cancers"),NULL,
						choices = pcawg_items,
						multiple = TRUE,
						selected = pcawg_items,
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
					filter_samples_UI(ns("filter_samples2cor"), database = "pcawg"),
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
					add_signature_UI(ns("add_signature2cor"), database = "pcawg")
				)
			),
			# 下载X/Y轴数据
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					# X
					h4(strong("S2.1 Get data for X-axis")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Get one data", 
					                   content = "get_one_data"), 
					download_feat_UI(ns("download_x_axis"), 
						button_name="Query", database = "pcawg"),
		            # br(),br(),
		            # Y
		            h4(strong("S2.2 Get data for Y-axis")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Get one data", 
					                   content = "get_one_data"), 
					download_feat_UI(ns("download_y_axis"), 
						button_name="Query", database = "pcawg")

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
					selectInput(ns("cor_method"), "Correlation method:",choices = c("Pearson", "Spearman")),
					h4(strong("S3.2 Set visualization parameters")), 
					fluidRow(
						column(3, colourpicker::colourInput(inputId = ns("positive_color"), "Positive color:", "#d53e4f")),
						column(3, colourpicker::colourInput(inputId = ns("negative_color"), "Negative color:", "#3288bd")),
					),
					dropMenu(
						actionButton(ns("more_visu"), "Set more visualization params"),
						div(h3("1. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size"), label = "Text size:", value = 18, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size"), label = "Title size:", value = 20, step = 0.5)),
							column(4, numericInput(inputId = ns("label_size"), label = "Label size:", value = 5, step = 0.5)),
						),				
						div(h3("2. Adjust lab and title name:"),style="width:400px;"),
						fluidRow(
							column(4, textInput(inputId = ns("x_name"), label = "X-axis name:", 
								value = "estimate coefficient")),
							column(4, textInput(inputId = ns("title_name"), label = "Title name:",
								value = NULL))
						),	
						div(h5("Note: You can download the raw data and plot in local R environment for more detailed adjustment.")),
					),
					br(),

					shinyWidgets::actionBttn(
						ns("step3_plot_bar"), "Run",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("cor_plot_bar")}, height = "500px") 
						)
					),
					h4(strong("S3.3 Download results")), 
				    fluidRow(
				    	column(3, downloadButton(ns("save_plot_bt"), "Figure")),
				    	column(3, offset = 0, downloadButton(ns("save_data_raw"), "Raw data(.csv)")),
				    	column(3, offset = 1, downloadButton(ns("save_data_res"), "Analyzied data(.csv)"))
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



server.modules_pcawg_cor_o2m = function(input, output, session) {
	ns <- session$ns
	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BLCA-US", phe_primary="",
		filter_phe_id=query_tcga_group(database = "pcawg", cancer = "BLCA-US", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
		cancer_choose$phe_primary <- query_tcga_group(database = "pcawg",
			cancer = cancer_choose$name, return_all = T)
	})

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2cor", database = "pcawg")

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2cor", database = "pcawg")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2cor", database = "pcawg")
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2cor",
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

	## x-axis panel
	x_axis_data = callModule(download_feat_Server, "download_x_axis", 
							 database = "pcawg",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     check_numeric=TRUE
							 )

	## y-axis panel
	y_axis_data = callModule(download_feat_Server, "download_y_axis", 
							 database = "pcawg",
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta_sig()),
						     opt_pancan = reactive(opt_pancan()),
						     check_numeric=TRUE
							 )

	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_bar = eventReactive(input$step3_plot_bar, {
		x_axis_data = x_axis_data()
		colnames(x_axis_data)[c(1:3,5)] = paste0("x_",colnames(x_axis_data)[c(1:3,5)])
		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])

		# inner_join取交集本身可以避免行为0的项目数据
		data = dplyr::inner_join(x_axis_data, y_axis_data) %>%
			dplyr::select(cancer, Sample, everything())
		data
	})

	cor_data_bar = eventReactive(input$step3_plot_bar, {
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
	})

	cor_plot_bar = eventReactive(input$step3_plot_bar, {
		shiny::validate(
			need(try(nrow(cor_data_bar())>0), 
				"Please inspect whether to download valid X/Y axis data in S2 or S3 step."),
		)

		cor_data_bar = cor_data_bar()
		p = cor_data_bar %>% 
		  dplyr::arrange(estimate) %>% 
		  dplyr::mutate(cancer = factor(cancer, levels = cancer)) %>% 
		  dplyr::mutate(group = estimate>0) %>% 
		  ggplot(aes(x=cancer, y=estimate, fill=group)) + 
		  geom_col(color="black") + 
		  geom_text(aes(y=0,label=format(round(estimate,2), nsmall =2),
		                hjust = ifelse(estimate >= 0, 1.5, -0.5)),
		  			size = isolate(input$label_size)) +
		  xlab("") + ylab(isolate(input$x_name)) + #转置
		  ggtitle(label = isolate(input$title_name)) +
		  coord_flip() +
		  scale_fill_manual(values = c(isolate(input$negative_color),isolate(input$positive_color))) +
		  theme_minimal() + 
		  theme(legend.position = "none", text = element_text(size=isolate(input$axis_size)),
				plot.title = element_text(size=isolate(input$title_size), hjust = 0.5))
		return(p)
	})

	output$cor_plot_bar = renderPlot({cor_plot_bar()})

	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("Barplot", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = cor_plot_bar()
			
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
			paste0("Correlation_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = merge_data_bar()
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Correlation_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_cor = cor_data_bar()
			p_cor$parameter1 = unique(merge_data_bar()$x_axis)
			p_cor$parameter2 = unique(merge_data_bar()$y_axis)	
			write.csv(p_cor, file, row.names = FALSE)
		}
	)


}