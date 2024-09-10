ui.modules_pancan_cross_gene_o2m = function(id) {
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
					h4(strong("S2.1 Select one gene")),
					virtualSelectInput(
						inputId = ns("gene_id"),
						label = NULL,
						choices = NULL,
						search = TRUE),
					br(),
					h4(strong("S2.2 Load mRNA/Mutation/CNV data")),
					shinyWidgets::actionBttn(
						ns("step2_2_load"), "Load",
				        style = "gradient",
				        icon = icon("box"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					), 
					br(),
					verbatimTextOutput(ns("step2_2_text")),
					br(),
					h4(strong("S2.3 Load transcript data")),
					virtualSelectInput(
						inputId = ns("trans_id"),
						multiple = TRUE,
						label = NULL,
						choices = NULL,
						search = TRUE),
					shinyWidgets::actionBttn(
						ns("step2_3_load"), "Load",
				        style = "gradient",
				        icon = icon("box"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					), 
					br(),
					verbatimTextOutput(ns("step2_3_text")),
					br(),
					h4(strong("S2.4 Load methylation (450K) data")),
					virtualSelectInput(
						inputId = ns("methy_id"),
						multiple = TRUE,
						label = NULL,
						choices = NULL,
						search = TRUE),
					shinyWidgets::actionBttn(
						ns("step2_4_load"), "Load",
				        style = "gradient",
				        icon = icon("box"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					), 
					br(),
					verbatimTextOutput(ns("step2_4_text")),
				)
			),
			# 分析/绘图/下载
			column(
				6,
				wellPanel(
					h2("S3: Analyze & Visualize", align = "center") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Analyze & Visualize", 
					                   content = "cross_gene"), 
					style = "height:1100px",
					# h4(strong("S3.1 Set analysis parameters")), 
					# h4(strong("S3.2 Set visualization parameters")), 
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


server.modules_pancan_cross_gene_o2m = function(input, output, session) {
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


	check_omics = reactiveValues(mRNA=TRUE,mutation=TRUE,cnv=TRUE)

	updateVirtualSelect(
		"gene_id",
		choices = tcga_id_option[["Molecular profile"]][["mRNA Expression"]]$all,
		selected = tcga_id_option[["Molecular profile"]][["mRNA Expression"]]$default
	)

	notify <- function(msg, id = NULL) {
		showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
	}
	observeEvent(input$step2_2_load, {
		id <- notify(h3("[1/3] Caching mRNA data..."))
		on.exit(removeNotification(id), add = TRUE)

		dat_tmp = query_pancan_value(input$gene_id, "mRNA")
		if(all(is.na(dat_tmp$expression))){check_omics$mRNA=FALSE}
		Sys.sleep(0.5)

		notify(h3("[2/3] Caching mutation data..."), id = id)
		dat_tmp = query_pancan_value(input$gene_id, "mutation")
		if(all(is.na(dat_tmp))){check_omics$mutation=FALSE}
		Sys.sleep(0.5)

		notify(h3("[3/3] Caching CNV data..."), id = id)
		opt_pancan = .opt_pancan
		opt_pancan$toil_cnv$use_thresholded_data=T
		dat_tmp = query_pancan_value(input$gene_id, "cnv",opt_pancan = opt_pancan)
		if(all(is.na(dat_tmp$data))){check_omics$cnv=FALSE}
		Sys.sleep(0.5)

		output$step2_2_text = renderPrint({
			cat(paste0("Tips: \n(1) mRNA is ", 
					   ifelse(check_omics$mRNA,"OK; ","missing; "),
					   "\n(2) Mutation is ",
					   ifelse(check_omics$mutation,"OK; ","missing; "),
					   "\n(3) CNV is ",
					   ifelse(check_omics$cnv,"OK.","missing.")
			))
		})
	})


	candi_trans = reactive({
		# req(input$gene_id)
		load_data("v2_tpc_id_help")$tcga$id_trans %>% 
			dplyr::filter(Symbol == input$gene_id) %>% 
			dplyr::pull(Level3) %>% sort()
	})
	observe({
		updateVirtualSelect(
			"trans_id",
			choices = candi_trans(),
			selected = candi_trans()[1:5]
		)
	})
	observeEvent(input$step2_3_load, {
		withProgress(message = "Caching transcript... ", {
			valid_trans = rep(TRUE, length(input$trans_id))
			for (i in seq(input$trans_id)) {
				incProgress(1 / length(input$trans_id), detail = paste0("Now: ",i))
				# gene_trans_tmp = query_pancan_value(input$trans_id[i], "transcript")
				gene_trans_tmp = query_pancan_value(
					input$trans_id[i], "transcript"
				) %>% as.data.frame() %>% 
					dplyr::select(expression) %>% 
					dplyr::rename("Trans"="expression") %>% 
					tibble::rownames_to_column("sample") %>% 
					dplyr::inner_join(tcga_gtex) %>% 
					dplyr::filter(type2=="tumor") %>%
					dplyr::group_by(tissue) %>% 
					dplyr::summarise(Trans = median(Trans))
				if(sd(gene_trans_tmp$Trans)==0){
					valid_trans[i] = FALSE
				}
			}
			check_omics$valid_trans = input$trans_id[valid_trans]
		})
		output$step2_3_text = renderPrint({
			cat(paste0("Tip: ",length(check_omics$valid_trans)," valid transcript is OK."))
		})
	})


	candi_methy = reactive({
		load_data("v2_tpc_id_help")$tcga$id_M450 %>% 
			dplyr::filter(Level3 == input$gene_id) %>% 
			dplyr::pull(CpG) %>% sort()
	})
	observe({
		updateVirtualSelect(
			"methy_id",
			choices = candi_methy(),
			selected = candi_methy()[1:5]
		)
	})
	observeEvent(input$step2_4_load, {
		withProgress(message = "Caching methylation... ", {
			for (i in seq(input$methy_id)) {
				incProgress(1 / length(input$methy_id), detail = paste0("Now: ",i))
				opt_pancan = .opt_pancan
				opt_pancan$toil_methylation$rule_out = setdiff(candi_methy(), input$methy_id[i])
				opt_pancan$toil_methylation$aggr = "mean"
				gene_methy_tmp = query_pancan_value(input$gene_id, "methylation", opt_pancan=opt_pancan)
			}
		})
		output$step2_4_text = renderPrint({
			cat(paste0("Tip: Methylation is OK."))
		})
		check_omics$valid_methy = input$methy_id
	})

	plot_func = eventReactive(input$step3_plot,{
		shiny::validate(
			need(all(check_omics$mRNA,check_omics$mRNA,check_omics$mRNA), 
				"Please load valid mRNA/Mutation/CNV data in Step2.2"),
			need(try(length(check_omics$valid_trans)>0), 
				"Please load valid transcript data in Step2.3"),
			need(try(length(check_omics$valid_methy)>0), 
				"Please load valid methylation data in Step2.4"),
			need(try(length(check_omics$valid_trans)<10), 
				"Please select less than 10 transcript in Step2.3"),
			need(try(length(check_omics$valid_methy)<10), 
				"Please select less than 10 methylation CpG sites in Step2.4"),
		)
		shinyjs::disable("step3_plot")
		res = vis_gene_cross_omics(input$gene_id,
							 tumor_projects = cancer_choose$name,
							 tumor_samples = cancer_choose$filter_phe_id,
							 n_trans = check_omics$valid_trans,
							 n_methy = check_omics$valid_methy,
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
			p_stat = apply(p_stat,2,as.character) #list column
			write.csv(p_stat, file, row.names = FALSE)
		}
	)

}

