ui.modules_pancan_test = function(id) {
	ns = NS(id)

	fluidPage(
		# 第一行：选择肿瘤及样本
		wellPanel(
			h2("S1: Set out cancer(s) and samples", align = "center"),
			style = "height:250px",
			# fluidRow(
			# 	column(
			# 		3, offset = 1,
			# 		h4("(1) Choose cancer(s)"),
			# 	    prettyRadioButtons(
			# 	      inputId = ns("overall_mode"),
			# 	      label = NULL,
			# 	      icon = icon("check"),
			# 	      choices = c("Single cancer", "Multi-cancers"),
			# 	      animation = "tada",
			# 	      inline = TRUE,
			# 	      status = "default"),
			# 	    uiOutput(ns("choose_overall_mode"))
			# 	),
			# 	column(
			# 		3, offset = 2,
			# 		h4("(2) Choose samples (optional)"),
			# 		br(),
			# 	    filter_samples_UI(ns("filter_samples2test")),
			# 	    br(),
			# 	    textOutput(ns("filter_phe_id_info")),

			# 	),
			# 	column(
			# 		3, 
			# 		# h5("Quick Filtering by code"),
			# 		br(),
			# 		uiOutput(ns("filter_by_code.ui"))
			# 	)
			# ),
		),
		fluidRow(
			# 选择分组依据
			column(
				2,
				wellPanel(
					style = "height:1200px",
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
				    br(),br(),

					h4("(2) Choose samples[opt]"),
					filter_samples_UI(ns("filter_samples2comp")),
					uiOutput(ns("filter_by_code.ui")),
					textOutput(ns("filter_phe_id_info")),

					br(),br(),
					h4("(3) Upload sample info[opt]"),

					fileInput(ns("upload_sp_info"),"User-defined metadata(.csv)", accept = ".csv"),
					
					downloadButton(ns("example_sp_info"), "Download example data."),

					br(),br(),
					h4("(4) Set data origin[opt]"),
					shinyWidgets::actionBttn(
						ns("data_origin"), "enter available respository",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					)

				)


			),
			column(
				3,
				wellPanel(
					style = "height:1200px",
					h2("S2: Select var for group", align = "center"),
					group_samples_UI(ns("group_samples2test"))  
				)
			),
			column(
				3,
				wellPanel(
					style = "height:1200px",
					h2("S2: Select var for group", align = "center"),
					download_feat_UI(ns("download_y_axis"), button_name="Query data(y-axis)"),
		            uiOutput(ns("y_axis_data_table"))				

				)
			)
		)
	)

}



server.modules_pancan_test = function(input, output, session) {
	ns <- session$ns


	# 初始选择单癌/泛癌
	output$choose_overall_mode = renderUI(
		if(input$overall_mode == "Single cancer"){
			pickerInput(
				ns("choose_cancer"), NULL,#"Choose one cancer type",
				choices = sort(tcga_cancer_choices),
				selected = "BRCA")
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
	cancer_choose <- reactiveValues(name = "BRCA", phe_primary="",filter_phe_id=NULL)
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2test",
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
					fluidRow(
						column(
							6,
							selectInput(ns("L2_1_exp_1"),"(1)Thresholded data",
								choices = c(TRUE, FALSE), selected = TRUE)
						)
					),
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
							selectInput(ns("L2_3_methy_2"),"(1)Aggregation",
								choices = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"), 
								selected = "NA")
						)
					),
					h4("4. Protein Expression"),
					h4("5. miRNA Expression"),
					h4("6. Mutation status"),
					h4("7. Copy Number Variation"),
				)
			)
		)
	})
	opt_pancan = reactive({
		list(
			toil_mRNA = list(),
			toil_transcript = list(),
			toil_protein = list(),
			toil_mutation = list(),
			toil_cnv = list(use_thresholded_data = ifelse(is.null(input$L2_1_exp_1),TRUE,as.logical(input$L2_1_exp_1))),
			toil_methylation = list(type = ifelse(is.null(input$L2_3_methy_1),"450K",input$L2_3_methy_1), 
									aggr = ifelse(is.null(input$L2_3_methy_2),"NA",input$L2_3_methy_2)),
			toil_miRNA = list()
		)
	})


	# 分组相关操作
	group_final = callModule(group_samples_Server, "group_samples2test",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(cancer_choose$filter_phe_id),
						   opt_pancan = reactive(opt_pancan())
						   )

	output$choose_group_2levels_out = renderPrint({head(group_final())})



	# 下载数据
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
}