ui.modules_ccle_cross_pw_o2m = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					mol_origin_UI(ns("mol_origin2cor"), database = "ccle"),

					h4(strong("S1.1 Choose tissues")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "CCLE tissues", 
					                   content = "ccle_types"),
					pickerInput(
						ns("choose_cancers"), NULL,
						choices = sort(unique(ccle_info_fine$Site_Primary)),
						multiple = TRUE,
						selected = sort(unique(ccle_info_fine$Site_Primary)),
						options = list(`actions-box` = TRUE)
					),

				    br(),

					h4(strong("S1.2 Filter samples"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Filter samples", 
					                   content = "choose_samples"),
					h5("Exact filter:"),
					filter_samples_UI(ns("filter_samples2cor"), database = "ccle"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br()
				)
			),
			# 下载X轴数据
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					# 调用下载模块UI
					h4(strong("S2.1 Select one pathway")),
                    virtualSelectInput(
                        inputId = ns("pw_id"), 
                        label = NULL,
                        choices = list(
                            HALLMARK = paste0("HALLMARK_",tcga_id.list[["HM"]]),
                            KEGG = paste0("KEGG_",tcga_id.list[["KEGG"]]),
                            IOBR = paste0("IOBR_",tcga_id.list[["IOBR"]])
                        ),
                        selected = "HALLMARK_ADIPOGENESIS", 
                        width = "100%",
                        search = TRUE,
                        dropboxWidth = "200%"
                    ),
					br(),
				)
			),
			# 分析/绘图/下载
			column(
				6,
				wellPanel(
					h2("S3: Analyze & Visualize", align = "center"),
					style = "height:1100px",
					shinyWidgets::actionBttn(
						ns("step3_plot"), "Run (Visualize)",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
                    h4("Note: CCLE pathway data (ssGSEA scores) is currently unavailable in the remote server. This module is under development.", style = "color:red"),
					br(),
					fluidRow(
						column(12, offset = 0,
							   plotOutput({ns("funky_plot")}, height = "700px") 
						)
					)
				)
			)
		)
	)
}


server.modules_ccle_cross_pw_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "lung", phe_primary="",
		filter_phe_id=query_tcga_group(database = "ccle", cancer = "lung", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
		cancer_choose$phe_primary <- query_tcga_group(database = "ccle", cancer = cancer_choose$name, return_all = T)
	})
	
	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2cor", database = "ccle")

    custom_meta_sig = reactive(NULL)
    sig_dat = reactive(NULL)

	## 过滤样本
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2cor",
					   database = "ccle",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta_sig()),
					   opt_pancan = reactive(opt_pancan()))

	# 综合上述二者
	observe({
		# quick filter
		filter_phe_id2 = cancer_choose$phe_primary$Sample

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


	plot_func = eventReactive(input$step3_plot,{
        showNotification("ccle_PW data not found. Please contact developers.", type = "error")
        return(NULL)
	})

	output$funky_plot = renderPlot({
        req(plot_func())
        plot_func()$plot
    })
}
