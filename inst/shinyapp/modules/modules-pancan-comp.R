ui.modules_pancan_comp = function(id) {
	ns = NS(id)
	fluidPage(
		# 第一行：初始参数及绘图参数
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
				    filter_samples_UI(ns("filter_samples2comp")),
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
		# 第二行：下载数据/分析可视化
		fluidRow(
			# 选择分组依据
			column(
				3,
				wellPanel(
					style = "height:1000px",
					h2("S2: Select var for group", align = "center"),
					group_samples_UI(ns("group_samples2comp"))  

				)
			),
			# 下载Y轴数据
			column(
				3,
				wellPanel(
					style = "height:1000px",
					h2("S3: Select var for compare", align = "center"),

					# 调用模块
					download_feat_UI(ns("download_y_axis"), button_name="Query data(y-axis)"),

	            	br(),br(),br(),
		            uiOutput(ns("y_axis_data_table"))				)
			),
			# 分析绘图
			column(
				4,
				wellPanel(
					h2("S4: Analyze", align = "center"),
					style = "height:1000px",

					uiOutput(ns("step3_plot_bt.ui")),
					br(),
					selectInput(ns("comp_method"), "Comparison metohd",choices = c("t-test", "wilcoxon")),
					br(),
				    tabsetPanel(id = ns("plot_layout"),
				      tabPanel("BoxViolin(single cancer)", 
						fluidRow(
							column(3, colourpicker::colourInput(inputId = ns("group_1_color"), "Color (group-1)", "#E69F00")),
							column(3, colourpicker::colourInput(inputId = ns("group_2_color"), "Color (group-2)", "#56B4E9")),
							column(3, numericInput(inputId = ns("point_size"), label = "Point size", value = 3, step = 0.5)),
							column(3, numericInput(inputId = ns("point_alpha"), label = "Point alpha", value = 0.4, step = 0.1, min = 0, max = 1))
						),
				      	fluidRow(column(12,uiOutput(ns("comp_plot_box.ui"))))
				      ),
				      tabPanel("Lineplot(multi-cancers)", 
						fluidRow(
							column(3, colourpicker::colourInput(inputId = ns("group_1_color_2"), "Color (group-1)", "#E69F00")),
							column(3, colourpicker::colourInput(inputId = ns("group_2_color_2"), "Color (group-2)", "#56B4E9")),
							column(6, radioButtons(inputId = ns("significance"), label = "Significance", 
								choices = c("Value", "Symbol"), selected="Symbol",inline = TRUE)),
						),
				      	fluidRow(column(12,uiOutput(ns("comp_plot_line.ui"))))
				      )
				    )

				)
			),
			# 下载
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
					downloadButton(ns("save_data_comp"), "Save comp data(.csv)")
				)

			)
		)
	)
}



server.modules_pancan_comp = function(input, output, session) {
	ns <- session$ns

	# 更新选择器：单癌/泛癌
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
		        icon = icon("search"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)

		} else if(input$overall_mode == "Multi-cancers"){
			shinyWidgets::actionBttn(
				ns("step3_plot_line"), "Go/Update line plot",
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2comp",
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


	# 分组相关操作
	group_final = callModule(group_samples_Server, "group_samples2comp",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(cancer_choose$filter_phe_id))

	output$choose_group_2levels_out = renderPrint({head(group_final())})

	# y-axis panel
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
	observe({
		if(length(cancer_choose$name)==1){
			cancer_choose$single_cancer_ok = min(table(merge_data_box()$group))>=3
		} else {
			cancer_choose$multi_cancer_ok = 
				sort(unique(merge_data_line()$cancer))[
					apply(table(merge_data_line()$cancer,merge_data_line()$group),1,function(x) {min(x)>=3})]
		}
	})

	# boxviolin逻辑：先绘图，再提取相关性结果
	merge_data_box = eventReactive(input$step3_plot_box, {
		group_data = group_final()[,c(1,4,5)]
		colnames(group_data) = c("sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, sample, value, group, everything())
		data
	})

	comp_plot_box = eventReactive(input$step3_plot_box, {
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
	observeEvent(input$step3_plot_box, {
		if(cancer_choose$single_cancer_ok){
			output$comp_plot_box.ui = renderUI({
				output$comp_plot_box_tmp = renderPlot({
					comp_plot_box()
				}) 
			plotOutput({ns("comp_plot_box_tmp")}, height = "600px")
			})
		} else {
			output$comp_plot_box.ui = renderText({comp_plot_box()})
		}
	})

	observeEvent(input$step3_plot_line,{
		output$tmp_step3_plot_line = renderPrint({cancer_choose$multi_cancer_ok})
	})


	# barplot逻辑：先批量计算相关性，再绘图
	merge_data_line = eventReactive(input$step3_plot_line, {
		group_data = group_final()[,c(1,4,5)]
		colnames(group_data) = c("sample","group","phenotype")
		y_axis_data = y_axis_data()
		data = dplyr::inner_join(y_axis_data, group_data) %>%
			dplyr::select(cancer, sample, value, group, everything())
		data
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

		p2 = comp_data_line() %>%
		  dplyr::mutate(p.label=case_when(
		    p.value < 0.001 ~ "***",
		    p.value < 0.01 ~ "**",
		    p.value < 0.05 ~ "*",
		    TRUE ~ "ns"
		  )) %>% ggplot() + 
		  geom_text(aes(label=formatC(p.label, format = "e", digits = 2),
		                x=cancer,y=1),) +
		  coord_flip()
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

	observeEvent(input$step3_plot_line, {
		output$comp_plot_line.ui = renderUI({
			output$comp_plot_line_tmp = renderPlot({
				comp_plot_line()
			}) 
		plotOutput({ns("comp_plot_line_tmp")}, height = "600px")
		})
	})

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