ui.modules_pancan_cor = function(id) {
	ns = NS(id)
	fluidPage(
		# 第一列：初始参数及绘图参数
		wellPanel(
			h2("S1: Set out cancer(s) and samples", align = "center"),
			style = "height:250px",
			fluidRow(
				column(
					3, offset = 1,
					h4("(1) Choose cancer(s)"),
				    prettyRadioButtons(
				      inputId = ns("overall_mode"),
				      label = NULL,
				      icon = icon("check"),
				      choices = c("Single cancer", "Multi-cancers"),
				      animation = "tada",
				      inline = TRUE,
				      status = "default"),
				    uiOutput(ns("choose_overall_mode"))
				),
				column(
					3, offset = 2,
					h4("(2) Choose samples (optional)"),
					br(),
				    filter_samples_UI(ns("filter_samples2cor")),
				    br(),
				    textOutput(ns("filter_phe_id_info")),

				),
				column(
					3, 
					# h5("Quick Filtering by code"),
					br(),
					uiOutput(ns("filter_by_code.ui"))
				)
			),
		),


		# ),
		# 第二列：下载数据/分析可视化
		fluidRow(
			column(
				3,
				wellPanel(
					style = "height:1000px",
					h2("S2: Select var for X", align = "center"),

					# 调用模块
					download_feat_UI(ns("download_x_axis"), button_name="Query data(x-axis)"),

	            	br(),br(),br(),
		            uiOutput(ns("x_axis_data_table"))

				)
			),
			# 下载Y轴数据
			column(
				3,
				wellPanel(
					style = "height:1000px",
					h2("S3: Select var for Y", align = "center"),
					# h3("select variable on Y axis", align = "center"),

					# 调用模块
					download_feat_UI(ns("download_y_axis"), button_name="Query data(y-axis)"),

	            	br(),br(),br(),
		            uiOutput(ns("y_axis_data_table"))
				)
			),
			# 分析绘图
			column(
				4,
				wellPanel(
					h2("S4: Analyze", align = "center"),
					style = "height:1000px",
					
					uiOutput(ns("step3_plot_bt.ui")),
					br(),
					selectInput(ns("cor_method"), "Correlation metohd",choices = c("Pearson", "Spearman")),

				    tabsetPanel(id = ns("plot_layout"),
				      tabPanel("Scatterplot(single cancer)", 
				      	br(),
						fluidRow(
							column(3, colourpicker::colourInput(inputId = ns("line_color"), "Line color", "#0000FF")),
							column(3, colourpicker::colourInput(inputId = ns("x_hist_color"), "Hist color(x)", "#009E73")),
							column(3, colourpicker::colourInput(inputId = ns("y_hist_color"), "Hist color(y)", "#D55E00"))
						),
						fluidRow(
							column(3, numericInput(inputId = ns("point_size"), label = "Point size", value = 3, step = 0.5)),
							column(3, numericInput(inputId = ns("point_alpha"), label = "Point alpha", value = 0.4, step = 0.1, min = 0, max = 1))
						),
				      	fluidRow(column(12,uiOutput(ns("cor_plot_sct.ui"))))
				      ),
				      tabPanel("Barplot(multi-cancers)", 
				      	br(),
						column(3, colourpicker::colourInput(inputId = ns("positive_color"), "Positive color", "#d53e4f")),
						column(3, colourpicker::colourInput(inputId = ns("negative_color"), "Negative color", "#3288bd")),
				      	fluidRow(column(12,uiOutput(ns("cor_plot_bar.ui"))))
				      )
				    )

				)
			),

			# 下载数据
			column(
				2,
				wellPanel(
					h2("S5: Download", align = "center"),
					style = "height:1000px",
			        br(),br(),br(),br(),
					downloadButton(ns("save_plot_bt"), "Save plot"),
			        br(),br(),
					fluidRow(
						numericInput(ns("save_plot_H"), "Height:", #width = "138px", 
		                         min = 4, max = 20, value = 10, step = 0.5),
						numericInput(ns("save_plot_W"), "Width:", #width = "138px", 
		                         min = 4, max = 20, value = 10, step = 0.5)
					),
			        prettyRadioButtons(
			          inputId = ns("save_plot_F"),
			          label = "Format",
			          choices = c("pdf", "png"),
			          selected = "pdf",
			          inline = TRUE,
			          icon = icon("check"),
			          animation = "jelly",
			          fill = TRUE
			        ),
			        br(),br(),br(),br(),br(),
					downloadButton(ns("save_data_raw"), "Save raw data(.csv)"),
			        br(),br(),br(),br(),br(),
					downloadButton(ns("save_data_cor"), "Save cor data(.csv)")
				)

			)
		)
	)
}


server.modules_pancan_cor = function(input, output, session) {
	ns <- session$ns

	# 初始选择单癌/泛癌
	output$choose_overall_mode = renderUI(
		if(input$overall_mode == "Single cancer"){
			pickerInput(
				ns("choose_cancer"), NULL,#"Choose one cancer type",
				choices = sort(tcga_cancer_choices))
		} else if(input$overall_mode == "Multi-cancers"){
			pickerInput(
				ns("choose_cancers"), NULL,#"Choose multiple cancer types",
				choices = sort(tcga_cancer_choices),
				multiple = TRUE,
				selected = sort(tcga_cancer_choices),
				options = list(`actions-box` = TRUE)
			)
		}
	)

	# 更新绘图按钮
	output$step3_plot_bt.ui = renderUI(
		if(input$overall_mode == "Single cancer"){
			shinyWidgets::actionBttn(
				ns("step3_plot_sct"), "Go/Update scatterplot",
		        style = "gradient",
		        icon = icon("search"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)
		} else if(input$overall_mode == "Multi-cancers"){
			# actionButton(ns("step3_plot_bar"), "GO Barplot")
			shinyWidgets::actionBttn(
				ns("step3_plot_bar"), "Go/Update barplot",
		        style = "gradient",
		        icon = icon("search"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)
		}
	)

	# 更新绘图参数按钮/展示窗口
	observeEvent(input$overall_mode, {
	  updateTabsetPanel(inputId = "plot_layout", 
	  	selected = switch(input$overall_mode,
	  		`Single cancer`="Scatterplot(single cancer)",
	  		`Multi-cancers`="Barplot(multi-cancers)"))
	}) 


	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "ACC", phe_primary="",filter_phe_id=NULL)
	observe({
		if(input$overall_mode == "Single cancer"){
			cancer_choose$name = input$choose_cancer
		} else if(input$overall_mode == "Multi-cancers"){
			cancer_choose$name = input$choose_cancers
		}
	})


	# 过滤样本
	## 快速过滤
	observe({
		phe_primary = query_tcga_group(
			cancer = cancer_choose$name, 
			return_all = T)
		code_types = list("NT"= "NT (normal tissue)",
						  "TP"= "TP (primary tumor)",
						  "TR"= "TR (recurrent tumor)",
						  "TB"= "TB (blood derived tumor)",
						  "TAP"="TAP (additional primary)",
						  "TM"= "TM (metastatic tumor)",
						  "TAM"="TAM (additional metastatic)")
		code_types_valid = code_types[names(code_types) %in% unique(phe_primary$Code)]
		output$filter_by_code.ui = renderUI(
			checkboxGroupInput(
				ns("filter_by_code"),
				"Quick filtering by code:",
				# choices = c("TP","NT","TM"),
				# selected = c("TP","NT","TM")
				choices = unlist(code_types_valid,use.names = F),
				selected = unlist(code_types_valid,use.names = F),
				inline = TRUE
			)
		)
	})

	## 精确过滤
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2cor",
					   cancers=reactive(cancer_choose$name))

	observe({
		phe_primary = query_tcga_group(
			cancer = cancer_choose$name, 
			return_all = T)


		code_types = list("NT"= "NT (normal tissue)",
						  "TP"= "TP (primary tumor)",
						  "TR"= "TR (recurrent tumor)",
						  "TB"= "TB (blood derived tumor)",
						  "TAP"="TAP (additional primary)",
						  "TM"= "TM (metastatic tumor)",
						  "TAM"="TAM (additional metastatic)")
		
		choose_codes = names(code_types)[unlist(code_types) %in% input$filter_by_code]


		# choose_codes =  stringr::str_trim(
		# 	stringr::str_split(choose_codes," ",simplify = T)[,1])

		filter_phe_id2 = phe_primary %>%
			dplyr::filter(Code %in% choose_codes) %>%
			dplyr::pull("Sample")

		# if(length(filter_phe_id2)==0){filter_phe_id2=NULL}

		if(is.null(filter_phe_id())){
			cancer_choose$filter_phe_id = filter_phe_id2
		} else {
			cancer_choose$filter_phe_id = intersect(filter_phe_id2,filter_phe_id())
		}

		
		output$filter_phe_id_info = renderText({
			paste0("NOTE: A total of ", length(cancer_choose$filter_phe_id), " samples are selcted.")

		})

	})


	## x-axis panel
	x_axis_data = callModule(download_feat_Server, "download_x_axis", 
							 cancers=reactive(cancer_choose$name),
							 samples=reactive(cancer_choose$filter_phe_id)
							 )
	output$x_axis_data_table = renderUI({
		output$x_tmp_table = renderDataTable({
			datatable(x_axis_data()[,c("sample","value","cancer")], 
				options = list(pageLength = 5,
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			) %>%  formatRound(columns = "value", digits = 3)
		}) 
	dataTableOutput(ns("x_tmp_table"))
	})



	## y-axis panel
	y_axis_data = callModule(download_feat_Server, "download_y_axis", 
							 cancers=reactive(cancer_choose$name),
							 samples=reactive(cancer_choose$filter_phe_id)
							 )

	output$y_axis_data_table = renderUI({
		output$y_tmp_table = renderDataTable({
			datatable(y_axis_data()[,c("sample","value","cancer")], 
				options = list(pageLength = 5,
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			) %>% formatRound(columns = "value", digits = 3)
		}) 
		dataTableOutput(ns("y_tmp_table"))
	})


	# 合并分析
	# scatterplot逻辑：先绘图，再提取相关性结果
	merge_data_sct = eventReactive(input$step3_plot_sct, {
		x_axis_data = x_axis_data()
		colnames(x_axis_data)[c(1:3,5)] = paste0("x_",colnames(x_axis_data)[c(1:3,5)])
		# colnames(x_axis_data)[which(colnames(x_axis_data)=="value")] = c("x_value")

		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])
		# colnames(y_axis_data)[which(colnames(y_axis_data)=="value")] = c("y_value")
		data = dplyr::inner_join(x_axis_data, y_axis_data) %>%
			dplyr::select(cancer, sample, everything())
		data
	})

	cor_plot_sct = eventReactive(input$step3_plot_sct, {
		merge_data_sct = merge_data_sct()
		cor_method = switch(isolate(input$cor_method),
			Pearson = "parametric", Spearman = "nonparametric")
		p = ggscatterstats(
		  merge_data_sct,
		  x = "x_value",
		  y = "y_value",
		  xlab = unique(merge_data_sct$x_id),
		  ylab = unique(merge_data_sct$y_id),
		  title = unique(merge_data_sct$cancer),
		  type = cor_method,
		  point.args = list(size = isolate(input$point_size), alpha = isolate(input$point_alpha)),
		  smooth.line.args = list(color = isolate(input$line_color),linewidth = 1.5,method = "lm",formula = y ~ x),
		  xsidehistogram.args = list(fill = isolate(input$x_hist_color), color = "black", na.rm = TRUE),
		  ysidehistogram.args = list(fill = isolate(input$y_hist_color), color = "black", na.rm = TRUE),
		  bf.message = FALSE
		) + 
			theme(text = element_text(size=18),
				  plot.title = element_text(size=20, hjust = 0.5))
		return(p)
	})

	observeEvent(input$step3_plot_sct, {
		output$cor_plot_sct.ui = renderUI({
			output$cor_plot_sct_tmp = renderPlot({
				cor_plot_sct()
			}) 
		plotOutput({ns("cor_plot_sct_tmp")}, height = "600px")
		})
	})

	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_bar = eventReactive(input$step3_plot_bar, {
		x_axis_data = x_axis_data()
		colnames(x_axis_data)[c(1:3,5)] = paste0("x_",colnames(x_axis_data)[c(1:3,5)])
		# colnames(x_axis_data)[which(colnames(x_axis_data)=="value")] = c("x_value")

		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])
		# colnames(y_axis_data)[which(colnames(y_axis_data)=="value")] = c("y_value")
		data = dplyr::inner_join(x_axis_data, y_axis_data) %>%
			dplyr::select(cancer, sample, everything())
		data
	})

	cor_data_bar = eventReactive(input$step3_plot_bar, {
		merge_data_bar = merge_data_bar()
		cor_method = switch(isolate(input$cor_method),
			Pearson = "parametric", Spearman = "nonparametric")
		valid_cancer_choose = sort(unique(merge_data_bar$cancer))
		stat_cor = lapply(valid_cancer_choose, function(tcga_type){
		  p = ggscatterstats(
		    subset(merge_data_bar, cancer==tcga_type),
		    x = "x_value",
		    y = "y_value",
		    type = cor_method)
		  extract_stats(p)$subtitle_data
		}) %>% do.call(rbind, .) %>% 
		dplyr::select(!expression) %>% 
		dplyr::mutate(cancer = valid_cancer_choose, .before=1)
	})

	cor_plot_bar = eventReactive(input$step3_plot_bar, {
		cor_data_bar = cor_data_bar()
		p = cor_data_bar %>% 
		  dplyr::arrange(estimate) %>% 
		  dplyr::mutate(cancer = factor(cancer, levels = cancer)) %>% 
		  dplyr::mutate(group = estimate>0) %>% 
		  ggplot(aes(x=cancer, y=estimate, fill=group)) + 
		  geom_col(color="black") + 
		  geom_text(aes(y=0,label=format(round(estimate,2), nsmall =2),
		                hjust = ifelse(estimate >= 0, 1.5, -0.5))) +
		  xlab("") + ylab("estimate coefficient") +
		  coord_flip() +
		  scale_fill_manual(values = c(isolate(input$negative_color),isolate(input$positive_color))) +
		  theme_minimal() + 
		  theme(legend.position = "none",
		        text = element_text(size=18))
		return(p)
	})

	observeEvent(input$step3_plot_bar, {
		output$cor_plot_bar.ui = renderUI({
			output$cor_plot_bar_tmp = renderPlot({
				cor_plot_bar()
			}) 
		plotOutput({ns("cor_plot_bar_tmp")}, height = "600px")
		})
	})

	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0(switch(input$overall_mode, `Single cancer` = "Scatterplot", `Multi-cancers` = "Barplot"),
						  "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = switch(input$overall_mode,
				`Single cancer` = cor_plot_sct(),
				`Multi-cancers` = cor_plot_bar()
				)
			
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
			p_raw = switch(input$overall_mode, 
				`Single cancer` = merge_data_sct(), `Multi-cancers` = merge_data_bar())
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
	output$save_data_cor = downloadHandler(
		filename = function(){
			paste0("Correlation_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			if(input$overall_mode=="Single cancer"){
				p_cor = extract_stats(cor_plot_sct())$subtitle_data
				p_cor = p_cor[-which(colnames(p_cor)=="expression")]
				p_cor$parameter1 = unique(merge_data_sct()$x_axis)
				p_cor$parameter2 = unique(merge_data_sct()$y_axis)
				p_cor = p_cor %>% dplyr::mutate(cancer = cancer_choose$name, .before=1)
			} else if (input$overall_mode=="Multi-cancers"){
				p_cor = cor_data_bar()
				p_cor$parameter1 = unique(merge_data_bar()$x_axis)
				p_cor$parameter2 = unique(merge_data_bar()$y_axis)	
			}
			write.csv(p_cor, file, row.names = FALSE)
		}
	)
}

