ui.modules_pancan_comp = function(id) {
	ns = NS(id)
	fluidPage(
		wellPanel(
			h2("TCGA Comparison Analysis", align = "center"),
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
			# 分组设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S2: Select var for group", align = "center"),
					# 调用分组模块UI
					group_samples_UI(ns("group_samples2comp"))  

				)
			),
			# 下载待比较数据
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S3: Select var for compare", align = "center"),
					# 调用模块
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
					selectInput(ns("comp_method"), "Comparison metohd",choices = c("t-test", "wilcoxon")),
					br(),
				    tabsetPanel(id = ns("plot_layout"),
				      tabPanel("BoxViolin(single cancer)", 
				      	br(),
						fluidRow(
							column(3, colourpicker::colourInput(inputId = ns("group_1_color"), "Color (group-1)", "#E69F00")),
							column(3, colourpicker::colourInput(inputId = ns("group_2_color"), "Color (group-2)", "#56B4E9")),
							column(3, numericInput(inputId = ns("point_size"), label = "Point size", value = 3, step = 0.5)),
							column(3, numericInput(inputId = ns("point_alpha"), label = "Point alpha", value = 0.4, step = 0.1, min = 0, max = 1))
						),
				      	plotOutput({ns("comp_plot_box")}, height = "500px")
				      ),
				      tabPanel("Lineplot(multi-cancers)",
				      	br(), 
						fluidRow(
							column(3, colourpicker::colourInput(inputId = ns("group_1_color_2"), "Color (group-1)", "#E69F00")),
							column(3, colourpicker::colourInput(inputId = ns("group_2_color_2"), "Color (group-2)", "#56B4E9")),
							column(6, radioButtons(inputId = ns("significance"), label = "Significance", 
								choices = c("Value", "Symbol"), selected="Symbol",inline = TRUE)),
						),
				      	plotOutput({ns("comp_plot_line")}, height = "500px")
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



server.modules_pancan_comp = function(input, output, session) {
	ns <- session$ns

	# 初始选择单癌/泛癌
	output$choose_overall_mode = renderUI(
		if(input$overall_mode == "Single cancer"){
			pickerInput(
				ns("choose_cancer"), NULL,
				choices = sort(tcga_cancer_choices),
				selected = "BRCA")
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
				ns("step3_plot_box"), "Go/Update BoxViolin",
		        style = "gradient",
		        icon = icon("chart-line"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)

		} else if(input$overall_mode == "Multi-cancers"){
			shinyWidgets::actionBttn(
				ns("step3_plot_line"), "Go/Update line plot",
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
	  		`Single cancer`="BoxViolin(single cancer)",
	  		`Multi-cancers`="Lineplot(multi-cancers)"))
	}) 

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "", phe_primary="",
									filter_phe_id=NULL, 
									single_cancer_ok = TRUE, multi_cancer_ok=FALSE)
	observe({
		if(input$overall_mode == "Single cancer"){
			cancer_choose$name = input$choose_cancer
		} else if(input$overall_mode == "Multi-cancers"){
			cancer_choose$name = input$choose_cancers
		}
	})

	# 样本自定义数据
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2comp",
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




	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2comp",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(cancer_choose$filter_phe_id),
						   custom_metadata=reactive(custom_meta()),
						   opt_pancan = reactive(opt_pancan())
						   )

	output$choose_group_2levels_out = renderPrint({head(group_final())})

	# 下载待比较数据
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
	# boxviolin逻辑：先绘图，再提取相关性结果
	merge_data_box = eventReactive(input$step3_plot_box, {
		group_data = group_final()[,c(1,4,5)]
		colnames(group_data) = c("sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, sample, value, group, everything())
		data
	})

	# 检查数据
	observe({
		cancer_choose$single_cancer_ok = min(table(merge_data_box()$group))>=3
	})


	comp_plot_box = eventReactive(input$step3_plot_box, {
		shiny::validate(
			need(try(nrow(merge_data_box())>0), 
				"Please inspect whether to set groups or download variable data in S2 or S3 step."),
		)
		merge_data_box = merge_data_box()

		if(!cancer_choose$single_cancer_ok){
			return("No enough samples for comparing, please check your input.")
		} else {
			comp_method = switch(isolate(input$comp_method),
				`t-test` = "parametric", wilcoxon = "nonparametric")
			p = ggbetweenstats(
			  data  = merge_data_box,
			  x     = "group",
			  y     = "value",
			  xlab = cancer_choose$group,
			  ylab = unique(merge_data_box$id),
			  title = unique(merge_data_box$cancer),
			  bf.message = FALSE,
			  type = comp_method,
			  centrality.plotting = FALSE,
			  median.color = 'black',
			  point.args = list(size = isolate(input$point_size), alpha = isolate(input$point_alpha),
			  	position = ggplot2::position_jitterdodge(dodge.width = 0.6), stroke = 0, na.rm = TRUE)
			) + 
			  ggplot2::scale_color_manual(values = c(isolate(input$group_1_color), isolate(input$group_2_color))) +
			  theme(text = element_text(size=18),
			        plot.title = element_text(size=20, hjust = 0.5),
			        plot.subtitle = element_text(size = 12))
			return(p)
		}
	})

	output$comp_plot_box = renderPlot({comp_plot_box()})



	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_line = eventReactive(input$step3_plot_line, {
		group_data = group_final()[,c(1,4,5)]
		colnames(group_data) = c("sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, sample, value, group, everything())
		data
	})


	# 检查数据
	observe({
		cancer_choose$multi_cancer_ok = 
			sort(unique(merge_data_line()$cancer))[
				apply(table(merge_data_line()$cancer,merge_data_line()$group),1,function(x) {min(x)>=3})]
	})

	comp_data_line = eventReactive(input$step3_plot_line, {
		merge_data_line = merge_data_line()
		comp_method = switch(isolate(input$comp_method),
			`t-test` = "parametric", wilcoxon = "nonparametric")
		valid_cancer_choose = cancer_choose$multi_cancer_ok
		stat_comp = lapply(valid_cancer_choose, function(tcga_type){
		  p = ggbetweenstats(
		    subset(merge_data_line, cancer==tcga_type),
		  	x = "group",
		  	y = "value",
		    type = comp_method)
		  extract_stats(p)$subtitle_data
		}) %>% do.call(rbind, .) %>% 
		dplyr::select(!expression) %>% 
		dplyr::mutate(cancer = valid_cancer_choose, .before=1)
	})


	comp_plot_line = eventReactive(input$step3_plot_line, {
		shiny::validate(
			need(try(nrow(merge_data_line())>0), 
				"Please inspect whether to set groups or download variable data in S2 or S3 step."),
		)
		merge_data_line_sub = merge_data_line() %>%
			dplyr::filter(cancer %in% cancer_choose$multi_cancer_ok)

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
			paste0(switch(input$overall_mode, `Single cancer` = "BoxViolin", `Multi-cancers` = "LinePlot"),
						  "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = switch(input$overall_mode,
				`Single cancer` = comp_plot_box(),
				`Multi-cancers` = comp_plot_line()
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
			paste0("Compare_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = switch(input$overall_mode, 
				`Single cancer` = merge_data_box(), `Multi-cancers` = merge_data_line())
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
	output$save_data_comp = downloadHandler(
		filename = function(){
			paste0("Compare_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			if(input$overall_mode=="Single cancer"){
				p_comp = extract_stats(comp_plot_box())$subtitle_data
				p_comp = p_comp[-which(colnames(p_comp)=="expression")]
				p_comp$identifier = unique(merge_data_box()$id)
				p_comp$phenotype = unique(merge_data_box()$phenotype)
				p_comp$group_1 = levels(merge_data_box()$group)[1]
				p_comp$group_2 = levels(merge_data_box()$group)[2]
			} else if (input$overall_mode=="Multi-cancers"){
				p_comp = comp_data_line()
				p_comp$identifier = unique(merge_data_line()$id)
				p_comp$phenotype = unique(merge_data_line()$phenotype)	
				p_comp$group_1 = levels(merge_data_line()$group)[1]
				p_comp$group_2 = levels(merge_data_line()$group)[2]
			}
			write.csv(p_comp, file, row.names = FALSE)
		}
	)
}