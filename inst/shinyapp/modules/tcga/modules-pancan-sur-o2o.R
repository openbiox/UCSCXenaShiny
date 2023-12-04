ui.modules_pancan_sur_o2o = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					h4("1. Choose cancer"),

					pickerInput(
						ns("choose_cancer"), NULL,
						choices = sort(tcga_cancer_choices)),
				    br(),br(),
					h4("2. Filter samples[opt]") %>% 
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
					filter_samples_UI(ns("filter_samples2sur")),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),br(),

					h4("3. Upload metadata[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Upload sample info", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2sur")),
					br(),br(),

					h4("4. Modify datasets[opt]") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "Set molecular profile origin", 
					                   content = "data_origin"),
					mol_origin_UI(ns("mol_origin2sur"))
				)
			),
			# 选择生存资料并设置分组
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),

				    shinyWidgets::prettyRadioButtons(
				        inputId = ns("endpoint_type"), label = "Endpoint type:",
				        choiceValues = c("OS", "DSS", "DFI", "PFI"),
				        choiceNames = c("OS (Overall Survial)", "DSS (Disease-Specific Survival)", 
				        				"DFI (Disease-Free Interval)", "PFI (Progression-Free Interval)"),
				        selected = "OS"
				    ),
				    br(),br(),
				    group_samples_UI(ns("group_samples2sur"))  
				)
			),
			# 分析/可视化/下载
			column(
				5,
				wellPanel(
					style = "height:1100px",
					h2("S3: Analyze", align = "center"),
				    # 绘图按钮
					shinyWidgets::actionBttn(
						ns("sur_analysis_bt_single"), "Go/Update",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					selectInput(ns("sur_method"), "1. Survival metohd",
						choices = c("Log-rank test", "Univariate Cox regression")),

				    materialSwitch(ns("use_origin"), 
				    	"2. Whether use initial data before grouping") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "About the initial phenotype", 
					                   content = "sur_initial_group"),


					br(),

			      	uiOutput(ns("one_params.ui")),

					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("sur_plot_one")}, height = "500px") 
						)
					),



				    br(),
				    fluidRow(
				    	column(3, downloadButton(ns("save_plot_bt"), "Figure")),
				    	column(3, offset = 0, downloadButton(ns("save_data_raw"), "Raw data(.csv)")),
				    	column(3, offset = 1, downloadButton(ns("save_data_res"), "Analyzed data(.csv)")),
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

server.modules_pancan_sur_o2o = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", filter_phe_id=NULL,
		phe_primary=query_tcga_group(cancer = "BRCA", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancer
		cancer_choose$phe_primary <- query_tcga_group(cancer = cancer_choose$name, return_all = T)
	})

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2sur")

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2sur")



	## 过滤样本
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2sur",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta()),
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


	# 生存资料
	sur_dat_v1 = reactive({
		sur_dat_raw = load_data("tcga_surv") 
		cli_dat_raw = load_data("tcga_clinical") 
		sur_dat_sub = sur_dat_raw %>%
			dplyr::filter(sample %in% cancer_choose$filter_phe_id) %>%
			dplyr::select("sample",contains(input$endpoint_type)) %>%
			dplyr::mutate(cancer = cli_dat_raw$type[match(sample,cli_dat_raw$sample)],.before = 1) %>%
			na.omit()
		colnames(sur_dat_sub)[3:4] = c("status","time")
		sur_dat_sub
	})

	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2sur",
						   cancers=reactive(cancer_choose$name),
						   # samples=reactive(cancer_choose$filter_phe_sur_id),
						   samples=reactive(sur_dat_v1()$sample),
						   custom_metadata=reactive(custom_meta()),
						   opt_pancan = reactive(opt_pancan())
						   )

	# 合并分组与生存
	sur_res_one = reactiveValues(sur_dat = NULL, cutoff=NULL, sur_res = NULL)

	group_sur_final = reactive({
		dat = dplyr::inner_join(group_final(),sur_dat_v1()[,-1],by=c("Sample"="sample"))
		## 验证是否只有一组分组的癌症
		dat = dat %>%
			dplyr::filter(Cancer %in% sort(unique(dat$Cancer))[
				apply(table(dat$Cancer,dat$group),1,function(x) {min(x)>=1})])
		dat
	})

	output$one_params.ui = renderUI(
		if(input$sur_method=="Log-rank test"){
		  	fluidRow(
		  		column(4,colourpicker::colourInput(ns("one_log_color1"), "Curve color1", "#E7B800")),
		  		column(4,colourpicker::colourInput(ns("one_log_color2"), "Curve color2", "#2E9FDF")),
		  	)
		} else if(input$sur_method=="Univariate Cox regression") {
		}
	)

	# 生存分析的输入（供绘图）与输出结果（供下载）
	observeEvent(input$sur_analysis_bt_single, {
		sur_res_one$sur_dat = group_sur_final()

		if(input$sur_method=="Log-rank test"){
			if(!input$use_origin){ #是否使用分组前的原始值
				sur_res_one$sur_dat$Group = sur_res_one$sur_dat$group
			} else {
				if(class(group_sur_final()$origin) != "character"){ #若原始值为数值型，则寻找最佳阈值
					res.cut <- surv_cutpoint(sur_res_one$sur_dat, time = "time", event = "status", variables = "origin")
					sur_res_one$sur_dat$Group = surv_categorize(res.cut)$origin
				} else {
					sur_res_one$sur_dat$Group = sur_res_one$sur_dat$group
				}
			}
			# print(head(sur_res_one$sur_dat))
			surv_diff <- survdiff(Surv(time, status) ~ Group, data = sur_res_one$sur_dat)
			pval = 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
			sur_res_one$sur_res = summary(survfit(Surv(time, status) ~ Group, data = sur_res_one$sur_dat))$table %>% 
				    as.data.frame() %>% tibble::rownames_to_column("Group") %>% 
				    dplyr::mutate(Cancer = cancer_choose$name, .before = 1) %>% 
				    dplyr::mutate(p.value = pval)

		} else if (input$sur_method=="Univariate Cox regression"){
			if(!input$use_origin){
				sur_res_one$sur_dat$Group = sur_res_one$sur_dat$group
			} else {
				sur_res_one$sur_dat$Group = sur_res_one$sur_dat$origin

			}
			fit <- coxph(Surv(time, status) ~ Group, data = sur_res_one$sur_dat)
			# sur_res_one$pval = summary(fit)$coefficients[1,5]
			sur_res_one$sur_res = summary(fit)$coefficients %>% as.data.frame()
		}
	})

	sur_plot_one = eventReactive(input$sur_analysis_bt_single,{
		shiny::validate(
			need(try(nrow(sur_res_one$sur_dat)>0), 
				"Please inspect whether to set valid groups in S3 step."),
		)
		dat = sur_res_one$sur_dat
		if(input$sur_method=="Log-rank test"){	
			fit <- survfit(Surv(time, status) ~ Group, data = dat)

			p <- ggsurvplot(fit, data = dat,#data = group_sur_final(), 
                           pval = TRUE, pval.method = TRUE, 
                           palette = c(input$one_log_color1, input$one_log_color2), 
                           size = 1.2, font.legend = c(14, "black"), 
                           font.x = c(14, "bold", "black"), 
                           font.y = c(14,  "bold", "black"), 
                           font.tickslab = c(12, "bold", "black"), 
                           xlab = paste(input$endpoint_type, "(days)"),
                           risk.table = TRUE, risk.table.col = "strata", risk.table.y.text = FALSE, 
                           ncensor.plot = FALSE, 
                           surv.plot.height = 0.7, 
                           risk.table.height = 0.3, 
                           ggtheme = ggplot2::theme_classic()) + 
			  ggplot2::guides(color = ggplot2::guide_legend(ncol = 3))
		}  else if (input$sur_method=="Univariate Cox regression"){
			fit = coxph(Surv(time, status) ~ Group , data = dat)
			p = ggforest(fit,data = dat,fontsize = 1)
		}
		p
	})

	output$sur_plot_one = renderPlot({sur_plot_one()})

	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("Curve", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = sur_plot_one()
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
			paste0("Survival_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = sur_res_one$sur_dat
			write.csv(p_raw, file, row.names = FALSE)
		}
	)

	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Survival_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = sur_res_one$sur_res
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
}