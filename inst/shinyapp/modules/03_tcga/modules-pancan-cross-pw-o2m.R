ui.modules_pancan_cross_pw_o2m = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					h4(strong("S1.1 Modify datasets"),"[Only for S1.3]") %>% 
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
					# h2("S3: Analyze & Visualize", align = "center"),
					h2("S3: Analyze & Visualize", align = "center") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Analyze & Visualize", 
					                   content = "cross_pw"), 
					style = "height:1100px",
					# h4(strong("S3.1 Set analysis parameters")), 
					shinyWidgets::actionBttn(
						ns("step3_plot"), "Run (Visualize)",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(12, offset = 0,
							   plotOutput({ns("funky_plot")}, height = "700px") 
						)
					),
					br(),
					fluidRow(
						column(3,
							div(shinyjs::hidden(downloadButton(ns("save_plot_bt"), "Figure")), style="display: inline-block;vertical-align:top;"),
							div(style="display: inline-block;vertical-align:top",
								dropMenu(  
									actionBttn(ns("plot_opt"),label = NULL,style = "material-circle", 
												color = "success",icon = icon("gear")),
									div(h3("1. Height:"),style="width:400px;"),
									numericInput(ns("save_plot_H"), NULL ,min = 1, max = 20, value = NULL, step = 0.1),
									div(h3("2. Width:"),style="width:400px;"),
									numericInput(ns("save_plot_W"), NULL ,min = 1, max = 20, value = NULL, step = 0.1),	
									div(h3("3. Format:"),style="width:400px;"),		
									radioGroupButtons(
										inputId = ns("save_plot_F"),
										label = NULL,
										status = "primary",
										choices = c("PDF", "PNG"),
										justified = TRUE
									),
									placement = "top"
								)
							)
						),
						column(3, offset = 0, shinyjs::hidden(downloadButton(ns("save_data_res"), "Statistical data(.csv)")))
					)
				)
			)
		)
	)
}


server.modules_pancan_cross_pw_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "ACC", phe_primary="",
		filter_phe_id=query_tcga_group(database = "toil", cancer = "BRCA", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
		cancer_choose$phe_primary <- query_tcga_group(database = "toil", cancer = cancer_choose$name, return_all = T)
	})
	
	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2cor", database = "toil")

    custom_meta_sig = reactive(NULL)
    sig_dat = reactive(NULL)

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
		code_types_valid = setdiff(code_types_valid, "NT (normal tissue)")
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


	plot_func = eventReactive(input$step3_plot,{
        shinyjs::disable("step3_plot")
		res = vis_pathway_cross_omics(input$pw_id,
                                     tumor_projects = cancer_choose$name,
                                     tumor_samples = cancer_choose$filter_phe_id,
							         return_list = TRUE)
        shinyjs::enable("step3_plot")
        res
	})

	output$funky_plot = renderPlot({plot_func()$plot})

	# three download buttons
    observeEvent(input$step3_plot, {
        shinyjs::show("save_plot_bt")
        shinyjs::show("save_data_res")
		updateNumericInput(session, "save_plot_H", value = plot_func()$plot$height)
		updateNumericInput(session, "save_plot_W", value = plot_func()$plot$width)
    })

	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("Plot", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",tolower(input$save_plot_F))
		},
		content = function(file){
            p = plot_func()$plot
		    if (input$save_plot_F == "PDF") {
		      pdf(file, width = input$save_plot_W, height = input$save_plot_H, onefile = FALSE)
		      print(p)
		      dev.off()
		    } else if (input$save_plot_F == "PNG"){
		      png(file, width = input$save_plot_W, height = input$save_plot_H, res = 600, units = "in")
		      print(p)
		      dev.off()
		    }
		}
	)
	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Statdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
            p_stat = plot_func()$data
			p_stat = apply(p_stat, 2, as.character) #list column
			write.csv(p_stat, file, row.names = FALSE)
		}
	)

}

