ui.modules_pcawg_sur_o2o = function(id) {
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
					mol_origin_UI(ns("mol_origin2sur"), database = "pcawg"),

					h4(strong("S1.2 Choose project")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "PCAWG projects", 
					                   content = "pcawg_projects"),
					pickerInput(
						ns("choose_cancer"),NULL,
						choices = pcawg_items),
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
					filter_samples_UI(ns("filter_samples2sur"), database = "pcawg"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),

					h4(strong("S1.4 Upload metadata"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Upload metadata", 
					                   content = "custom_metadata"),
					shinyFeedback::useShinyFeedback(),
					custom_meta_UI(ns("custom_meta2sur")),
					br(),

					h4(strong("S1.5 Add signature"),"[opt]") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Add signature", 
					                   content = "add_signature"),
					add_signature_UI(ns("add_signature2sur"), database = "pcawg")
				)
			),	
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					h4(strong("S2.1 Select survival endpoint")), 
					p("Only OS (Overall Survial) is supported."),
					br(),br(),
					h4(strong("S2.2 Divide 2 groups by one condition")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Divide 2 groups", 
					                   content = "set_groups"),
				    group_samples_UI(ns("group_samples2sur"),database = "pcawg")  
				)
			),
			# 分析/可视化/下载
			column(
				5,
				wellPanel(
					style = "height:1100px",
					h2("S3: Analyze & Visualize", align = "center") %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Analyze & Visualize", 
					                   content = "analyze_sur_1"),  
					h4(strong("S3.1 Set analysis parameters")), 
					selectInput(ns("sur_method"), "Survival method:",
						choices = c("Log-rank test", "Univariate Cox regression")),
				    materialSwitch(ns("use_origin"), 
				    	"Whether use initial data before grouping?") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "About the initial phenotype", 
					                   content = "sur_initial_group"),
					h4(strong("S3.2 Set visualization parameters")), 
			      	uiOutput(ns("one_params.ui")),
					dropMenu(
						actionButton(ns("more_visu"), "Set more visualization params"),
						div(h3(strong("Params for Log-rank test")),style="width:500px;"),
						div(h4("1. Wheather to dislpay risk.table:"),style="width:500px;"),
						fluidRow(
							column(6, radioButtons(inputId = ns("plot_table"), label = NULL, 
								choices = c("NO", "YES"), selected="NO",inline = TRUE)),
						),
						div(h4("2. Wheather to dislpay ncensor.plot:"),style="width:500px;"),
						fluidRow(
							column(6, radioButtons(inputId = ns("plot_ncensor"), label = NULL, 
								choices = c("NO", "YES"), selected="NO",inline = TRUE)),
						),
						div(h4("3. Wheather to dislpay confidence interval:"),style="width:500px;"),
						fluidRow(
							column(12, radioButtons(inputId = ns("plot_CI"), label = NULL, 
								choices = c("NO", "YES(ribbon)", "YES(step)"), selected="NO",inline = TRUE)),
						),	
						div(h4("2. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size"), label = "Text size:", value = 14, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size"), label = "Title size:", value = 18, step = 0.5))
						),	
						div(h4("4. Adjust lab and title name:"),style="width:500px;"),
						fluidRow(
							column(4, textInput(inputId = ns("x_name"), label = "X-axis name:")),
							column(4, textInput(inputId = ns("title_name"), label = "Title name:"))
						),	
						div(h3(strong("Params for Cox regression")),style="width:500px;"),
						div(h4("1. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size_2"), label = "Font size:", value = 0.7, step = 0.1)),
						),	
						div(h4("2. Adjust lab and title name:"),style="width:500px;"),
						fluidRow(
							column(4, textInput(inputId = ns("title_name_2"), label = "Title name:", value = "Hazard ratio"))
						),	
						div(h5("Note: You can download the raw data and plot in local R environment for more detailed adjustment.")),
					),
					br(),
					shinyWidgets::actionBttn(
						ns("sur_analysis_bt_single"), "Run",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("sur_plot_one")}, height = "500px") 
						)
					),

					h4(strong("S3.3 Download results")), 
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



server.modules_pcawg_sur_o2o = function(input, output, session) {
	ns <- session$ns
	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BLCA-US", phe_primary="",
		filter_phe_id=query_tcga_group(database = "pcawg", cancer = "BLCA-US", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancer
		cancer_choose$phe_primary <- query_tcga_group(database = "pcawg",
			cancer = cancer_choose$name, return_all = T)
	})

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2sur", database = "pcawg")

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2sur", database = "pcawg")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2sur", database = "pcawg")
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
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2sur",
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

	# 生存资料
	sur_dat_v1 = reactive({
		sur_dat_raw = pcawg_info[,c("dcc_project_code","icgc_specimen_id","OS","OS.time")]
		colnames(sur_dat_raw) = c("cancer","Sample","status","time")
		sur_dat_sub = sur_dat_raw %>% 
		  dplyr::distinct()  %>% na.omit() %>% 
		  dplyr::filter(Sample %in% cancer_choose$filter_phe_id)
		sur_dat_sub
	})
	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2sur",
						   database = "pcawg", 
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(sur_dat_v1()$Sample),
						   custom_metadata=reactive(custom_meta_sig()),
						   opt_pancan = reactive(opt_pancan())
						   )
	sur_res_one = reactiveValues(sur_dat = NULL, cutoff=NULL, sur_res = NULL)
	group_sur_final = reactive({
		dat = dplyr::inner_join(group_final(),sur_dat_v1()[,-1],by=c("Sample"="Sample"))
		## 验证是否只有一组分组的癌症
		dat = dat %>%
			dplyr::filter(Cancer %in% sort(unique(dat$Cancer))[
				apply(table(dat$Cancer,dat$group),1,function(x) {min(x)>=2})])
		dat
	})
	output$one_params.ui = renderUI(
		if(input$sur_method=="Log-rank test"){
		  	fluidRow(
		  		column(4,colourpicker::colourInput(ns("one_log_color1"), "Color (Group 1):", "#E7B800")),
		  		column(4,colourpicker::colourInput(ns("one_log_color2"), "Color (Group 2):", "#2E9FDF")),
		  	)
		} else if(input$sur_method=="Univariate Cox regression") {
			fluidRow(
				column(4,numericInput(ns("text_c1"), "Position of text col-1", 0.02, step = 0.01)),
				column(4,numericInput(ns("text_c2"), "text col-2", 0.22, step = 0.01)),
				column(4,numericInput(ns("text_c3"), "text col-3", 0.4, step = 0.01))
			)
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
					groups_1_2 = sur_res_one$sur_dat %>% 
						  dplyr::group_by(group) %>% 
						  dplyr::summarise(mean = mean(origin)) %>% 
						  dplyr::arrange(mean) %>% 
						  dplyr::pull(group) %>% as.character()
					sur_res_one$sur_dat$Group = ifelse(surv_categorize(res.cut)$origin=="low", groups_1_2[1], groups_1_2[2])
					sur_res_one$sur_dat$Group = factor(sur_res_one$sur_dat$Group, levels=groups_1_2)
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

 	observe({
		updateTextInput(session, "x_name", value = paste(input$endpoint_type, "(days)"))
		updateTextInput(session, "title_name", value = NULL)
 	})

	sur_plot_one = eventReactive(input$sur_analysis_bt_single,{
		shiny::validate(
			need(try(nrow(sur_res_one$sur_dat)>0), 
				"Please inspect whether to set valid groups in S3 step."),
		)
		if(input$plot_CI == "NO"){
			conf.int = FALSE
			conf.int.style = "ribbon"
		} else if(input$plot_CI == "YES(ribbon)"){
			conf.int = TRUE
			conf.int.style = "ribbon"
		} else if(input$plot_CI == "YES(step)"){
			conf.int = TRUE
			conf.int.style = "step"
		}
		dat = sur_res_one$sur_dat

		custom_theme <- function(plot_size) {
		  theme_classic() %+replace%
		    theme(
		      plot.title=element_text(hjust=0.5, size = plot_size)
		    )
		}

		if(input$plot_table=="NO" & input$plot_ncensor=="NO"){
			surv.plot.height = 1
           	risk.table.height = 0 
           	ncensor.plot.height = 0 
           	risk.table = FALSE
           	ncensor.plot = FALSE
		} else if (input$plot_table=="YES" & input$plot_ncensor=="NO"){
			surv.plot.height = 0.7
           	risk.table.height = 0.3 
           	ncensor.plot.height = 0
           	risk.table = TRUE
           	ncensor.plot = FALSE
		} else if (input$plot_table=="NO" & input$plot_ncensor=="YES"){
			surv.plot.height = 0.7
           	risk.table.height = 0 
           	ncensor.plot.height = 0.3 
           	risk.table = FALSE
           	ncensor.plot = TRUE
		} else if (input$plot_table=="YES" & input$plot_ncensor=="YES"){
			surv.plot.height = 0.7
           	risk.table.height = 0.15
           	ncensor.plot.height = 0.15
           	risk.table = TRUE
           	ncensor.plot = TRUE
		}

		if(input$sur_method=="Log-rank test"){	
			fit <- survfit(Surv(time, status) ~ Group, data = dat)
			p <- ggsurvplot(fit, data = dat,#data = group_sur_final(), 
	                       pval = TRUE, pval.method = TRUE, 
	                       palette = c(input$one_log_color1, input$one_log_color2), 
	                       size = 1.2, font.legend = c(14, "black"), 
	                       font.x = c(input$axis_size, "bold", "black"), 
	                       font.y = c(input$axis_size,  "bold", "black"), 
	                       font.tickslab = c(12, "bold", "black"), 
	                       xlab = input$x_name,
	                       title = input$title_name,
	                       conf.int = conf.int,
	                       conf.int.style = conf.int.style,
	                       risk.table = risk.table, risk.table.col = "strata", risk.table.y.text = FALSE, 
	                       ncensor.plot = ncensor.plot, 
	                       surv.plot.height = surv.plot.height, 
	                       risk.table.height = risk.table.height, 
	                       ncensor.plot.height = ncensor.plot.height, 
	                       ggtheme = custom_theme(input$title_size))
		}  else if (input$sur_method=="Univariate Cox regression"){
			fit = coxph(Surv(time, status) ~ Group , data = dat)
			p = ggforest(fit,data = dat,#fontsize = 1,
				cpositions = c(input$text_c1, input$text_c2, input$text_c3),
				fontsize = input$axis_size_2, main = input$title_name_2
				)
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