ui.modules_pancan_cor = function(id) {
	ns = NS(id)
	fluidPage(
		wellPanel(
			h2("TCGA Association Analysis", align = "center"),
			style = "height:150px",
		),

		fluidRow(
			# 初始设置
			column(
				2,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					h4("(1) Choose cancer(s)"),
				    prettyRadioButtons(
				      inputId = ns("overall_mode"),
				      label = NULL,
				      icon = icon("check"),
				      choices = c("Single cancer", "Multi-cancers"),
				      animation = "tada",
				      inline = TRUE,
				      status = "default"),
				    uiOutput(ns("choose_overall_mode")),
				    br(),br(),br(),
					h4("(2) Choose samples[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Choose samples for personalized need", 
					                   content = "choose_samples"),
					filter_samples_UI(ns("filter_samples2cor")),
					uiOutput(ns("filter_by_code.ui")),
					textOutput(ns("filter_phe_id_info")),
					br(),br(),br(),
					h4("(3) Upload metadata[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Upload sample info", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),

					fileInput(ns("upload_sp_info"),"", accept = ".csv"),
					downloadButton(ns("example_sp_info"), "Download example data."),
					br(),br(),br(),

					h4("(4) Set data origin[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Set molecular profile origin", 
					                   content = "data_origin"),
					shinyWidgets::actionBttn(
						ns("data_origin"), "Available respositories",
				        style = "gradient",
				        icon = icon("box"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					)
				)
			),
			# 下载X轴数据
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S2: Select var for X", align = "center"),
					# 调用下载模块UI
					download_feat_UI(ns("download_x_axis"), button_name="Query data(x-axis)"),
	            	br(),br(),br(),
		            uiOutput(ns("x_axis_data_table"))

				)
			),
			# 下载Y轴数据
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S3: Select var for Y", align = "center"),
					# 调用下载模块UI
					download_feat_UI(ns("download_y_axis"), button_name="Query data(y-axis)"),
	            	br(),br(),br(),
		            uiOutput(ns("y_axis_data_table"))
				)
			),
			# 分析/绘图/下载
			column(
				4,
				wellPanel(
					h2("S4: Analyze", align = "center"),
					style = "height:1100px",
					
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
				      	plotOutput({ns("cor_plot_sct")}, height = "500px"),


				      ),
				      tabPanel("Barplot(multi-cancers)", 
				      	br(),
						column(3, colourpicker::colourInput(inputId = ns("positive_color"), "Positive color", "#d53e4f")),
						column(3, colourpicker::colourInput(inputId = ns("negative_color"), "Negative color", "#3288bd")),
				      	br(),br(),br(),br(),
				      	plotOutput({ns("cor_plot_bar")}, height = "500px"),

				      )
				    ),
				    br(),
				    fluidRow(
				    	column(3, downloadButton(ns("save_plot_bt"), "Save plot")),
				    	column(3, offset = 0, downloadButton(ns("save_data_raw"), "Save raw data(.csv)")),
				    	column(3, offset = 1, downloadButton(ns("save_data_cor"), "Save res data(.csv)"))
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


server.modules_pancan_cor = function(input, output, session) {
	ns <- session$ns

	# 初始选择单癌/泛癌
	output$choose_overall_mode = renderUI(
		if(input$overall_mode == "Single cancer"){
			pickerInput(
				ns("choose_cancer"), NULL,
				choices = sort(tcga_cancer_choices))
		} else if(input$overall_mode == "Multi-cancers"){
			pickerInput(
				ns("choose_cancers"), NULL,
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
		        icon = icon("chart-line"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)
		} else if(input$overall_mode == "Multi-cancers"){
			shinyWidgets::actionBttn(
				ns("step3_plot_bar"), "Go/Update barplot",
		        style = "gradient",
		        icon = icon("chart-line"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)
		}
	)

	# 更新展示窗口
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


	# 用户上传自定义数据
	custom_meta = reactive({
		file = input$upload_sp_info
		if(is.null(file$datapath)){
			sp_info = query_tcga_group()$data[,"Sample"]
			set.seed(42)
			scores = matrix(rnorm(nrow(sp_info)*5,mean = 1, sd = 1), ncol = 5) %>% as.data.frame()
			colnames(scores) = paste0("TF",1:5)
			sp_info = cbind(sp_info, scores)
		} else {
			csv_format = tools::file_ext(file$name)=="csv"
			shinyFeedback::feedbackDanger("upload_sp_info", !csv_format, "Non .csv format file")
			req(csv_format,cancelOutput = TRUE)
			read.csv(file$datapath)
		} 
	})
	output$example_sp_info = downloadHandler(
		filename = function(){
			"example_sample_info.csv"
		},
		content = function(file){
			sp_info = query_tcga_group()$data[,"Sample"]
			set.seed(42)
			scores = matrix(rnorm(nrow(sp_info)*5,mean = 1, sd = 1), ncol = 5) %>% as.data.frame()
			colnames(scores) = paste0("TF",1:5)
			sp_info = cbind(sp_info, scores)
			write.csv(sp_info, file, row.names = FALSE)
		}
	)

	# 数据源设置
	observeEvent(input$data_origin, {
		showModal(
			modalDialog(
				title = "Set molecular profiles:",
				footer = modalButton("Done!"),
				size = "l",
				fluidPage(
					h4("1. mRNA Expression"),
					h4("2. Transcript Expression"),
					h4("3. DNA Methylation"),
					fluidRow(
						column(
							6,
							selectInput(ns("L2_3_methy_1"),"(1)Type",
								choices = c("450K","27K"), selected = TRUE)
						),
						column(
							6,
							selectInput(ns("L2_3_methy_2"),"(2)Aggregation",
								choices = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"), 
								selected = "mean")
						)
					),
					## relu_out
					fluidRow(
						column(
							6,
							selectizeInput(ns("L2_3_methy_3_gene"),"(3)Pinpoint CpG by Gene",
								choices = NULL, options = list(create = TRUE, maxOptions = 5))
						),
						column(
							6,
				            selectizeInput(
				              inputId = ns("L2_3_methy_3_cpg"),
				              label = "CpG sites",
				              choices = NULL,
				              multiple = TRUE,
				              options = list(create = TRUE, maxOptions = 5))

						)
					),
					h4("4. Protein Expression"),
					h4("5. miRNA Expression"),
					h4("6. Mutation status"),
					h4("7. Copy Number Variation"),
					fluidRow(
						column(
							6,
							selectInput(ns("L2_7_cnv_1"),"(1)Thresholded data",
								choices = c(TRUE, FALSE), selected = TRUE)
						)
					)
				)
			)
		)
	    updateSelectizeInput(
	      session,
	      "L2_3_methy_3_gene",
	      choices = pancan_identifiers$gene,
	      selected = "TP53",
	      server = TRUE
	    )
		# observeEvent(input$L2_3_methy_3_gene,{
		observe({
			cpg_type = reactive({
				if(is.null(input$L2_3_methy_1)){
					L2_3_methy_1 = "450K"
				} else {
					L2_3_methy_1 = input$L2_3_methy_1
				}
				switch(L2_3_methy_1,
					`450K` = id_merge$id_molecule$id_M450[,c("Level3","CpG")],
					`27K` = id_merge$id_molecule$id_M27K[,c("Level3","CpG")]
				)
			})
			cpg_ids = cpg_type() %>% 
				dplyr::filter(Level3 %in% input$L2_3_methy_3_gene) %>% 
				dplyr::pull(CpG)
		    updateSelectizeInput(
		      session,
		      "L2_3_methy_3_cpg",
		      choices = cpg_ids,
		      selected = NULL,
		      server = TRUE
		    )
		})

	})
	opt_pancan = reactive({
		cpg_type = reactive({
			if(is.null(input$L2_3_methy_1)){
				L2_3_methy_1 = "450K"
			} else {
				L2_3_methy_1 = input$L2_3_methy_1
			}
			switch(L2_3_methy_1,
				`450K` = id_merge$id_molecule$id_M450[,c("Level3","CpG")],
				`27K` = id_merge$id_molecule$id_M27K[,c("Level3","CpG")]
			)
		})
		cpg_ids = cpg_type() %>% 
			dplyr::filter(Level3 %in% input$L2_3_methy_3_gene) %>% 
			dplyr::pull(CpG)
		if(is.null(input$L2_3_methy_3_cpg)){
			cpg_ids_retain = NULL
		} else {
			cpg_ids_retain = setdiff(cpg_ids, input$L2_3_methy_3_cpg)
		}

		list(
			toil_mRNA = list(),
			toil_transcript = list(),
			toil_protein = list(),
			toil_mutation = list(),
			toil_cnv = list(use_thresholded_data = ifelse(is.null(input$L2_7_cnv_1),TRUE,as.logical(input$L2_7_cnv_1))),
			toil_methylation = list(type = ifelse(is.null(input$L2_3_methy_1),"450K",input$L2_3_methy_1), 
									aggr = ifelse(is.null(input$L2_3_methy_2),"NA",input$L2_3_methy_2),
									rule_out = cpg_ids_retain),
			toil_miRNA = list()
		)
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
				choices = unlist(code_types_valid,use.names = F),
				selected = unlist(code_types_valid,use.names = F),
				inline = TRUE
			)
		)
	})

	## 调用模块，精确过滤
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2cor",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta()),
					   opt_pancan = reactive(opt_pancan()))

	## 综合上述二者
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

		filter_phe_id2 = phe_primary %>%
			dplyr::filter(Code %in% choose_codes) %>%
			dplyr::pull("Sample")

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
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta()),
						     opt_pancan = reactive(opt_pancan())
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
							 samples=reactive(cancer_choose$filter_phe_id),
							 custom_metadata=reactive(custom_meta()),
						     opt_pancan = reactive(opt_pancan())
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
		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])

		data = dplyr::inner_join(x_axis_data, y_axis_data) %>%
			dplyr::select(cancer, sample, everything())
		data
	})

	cor_plot_sct = eventReactive(input$step3_plot_sct, {
		shiny::validate(
			need(try(nrow(merge_data_sct())>0), 
				"Please inspect whether to download valid X/Y axis data in S2 or S3 step."),
		)

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
	output$cor_plot_sct = renderPlot({cor_plot_sct()})


	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_bar = eventReactive(input$step3_plot_bar, {
		x_axis_data = x_axis_data()
		colnames(x_axis_data)[c(1:3,5)] = paste0("x_",colnames(x_axis_data)[c(1:3,5)])
		y_axis_data = y_axis_data()
		colnames(y_axis_data)[c(1:3,5)] = paste0("y_",colnames(y_axis_data)[c(1:3,5)])

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
		                hjust = ifelse(estimate >= 0, 1.5, -0.5))) +
		  xlab("") + ylab("estimate coefficient") +
		  coord_flip() +
		  scale_fill_manual(values = c(isolate(input$negative_color),isolate(input$positive_color))) +
		  theme_minimal() + 
		  theme(legend.position = "none",
		        text = element_text(size=18))
		return(p)
	})

	output$cor_plot_bar = renderPlot({cor_plot_bar()})


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

