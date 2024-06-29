ui.modules_pancan_sur_o2m = function(id) {
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
					mol_origin_UI(ns("mol_origin2sur"), database = "toil"),

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
					filter_samples_UI(ns("filter_samples2sur"), database = "toil"),
					br(),
					verbatimTextOutput(ns("filter_phe_id_info")),
					br(),br(),

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
					add_signature_UI(ns("add_signature2sur"), database = "toil"),
				)
			),
			# 选择生存资料
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S2: Get data", align = "center"),
					h4(strong("S2.1 Select survival endpoint")), 
				    shinyWidgets::prettyRadioButtons(
				        inputId = ns("endpoint_type"), label = NULL,
				        choiceValues = c("OS", "DSS", "DFI", "PFI"),
				        choiceNames = c("OS (Overall Survial)", "DSS (Disease-Specific Survival)", 
				        				"DFI (Disease-Free Interval)", "PFI (Progression-Free Interval)"),
				        selected = "OS"
				    ),
				    br(),br(),
					h4(strong("S2.2 Divide 2 groups by one condition")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "Divide 2 groups", 
					                   content = "set_groups"),
				    group_samples_UI(ns("group_samples2sur"), database = "toil") 
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
					                   content = "analyze_sur_2"),  
					h4(strong("S3.1 Set analysis parameters")), 
					selectInput(ns("sur_method"), "Survival method:",
						choices = c("Log-rank test", "Univariate Cox regression")),
				    materialSwitch(ns("use_origin"), 
				    	"Whether use initial data before grouping?"),
					h4(strong("S3.2 Set visualization parameters")), 
			      	uiOutput(ns("multi_params.ui")),

			      	dropMenu(
						actionBttn(ns("more_visu"), label = "Other options", style = "bordered",color = "success",icon = icon("bars")),		
						placement = "left",
						div(h3("Select ggplot theme:"),style="width:400px;"),
						fluidRow(
							column(6,
								selectInput(inputId = ns("theme"), label = NULL, 
											choices = names(themes_list), selected = "Minimal")
							)
						),
						div(h3(strong("Params for Log-rank test")),style="width:500px;"),
						div(h4("1. Add one vertical line:"),style="width:400px;"),
				      	fluidRow(
					  		column(6, numericInput(ns("multi_log_line"), "Add line(P):", 0.05)),
				      	),
						div(h4("2. Significance display:"),style="width:400px;"),
				      	fluidRow(
					  		column(6, selectInput(ns("multi_log_label"),"Add text:",
					  			choices = c("Signif.(symbol)", "Signif.(value)"),selected = "Signif.(symbol)"))	
				      	),
						div(h4("3. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size"), label = "Text size:", value = 18, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size"), label = "Title size:", value = 20, step = 0.5)),
							column(4, numericInput(inputId = ns("label_size"), label = "Label size:", value = 5, step = 0.5)),
						),				
						div(h4("4. Adjust lab and title name:"),style="width:400px;"),
						fluidRow(
							column(4, textInput(inputId = ns("x_name"), label = "X-axis name:", value = "-log10(P-value)")),
							column(4, textInput(inputId = ns("title_name"), label = "Title name:",
								value = NULL))
						),

						div(h3(strong("Params for Cox regression")),style="width:500px;"),
						div(h4("1. Add one vertical line:"),style="width:400px;"),
				      	fluidRow(
					  		column(6, numericInput(ns("multi_cox_line"), "Add line(P):", 0.05)),
				      	),
						div(h4("2. Significance display:"),style="width:400px;"),
				      	fluidRow(
					  		column(6, selectInput(ns("multi_cox_label"),"Add text:",
					  			choices = c("Signif.(symbol)", "Signif.(value)", "HR value"),selected = "HR value"))	
				      	),
						div(h4("3. Adjust text size:"),style="width:400px;"),
						fluidRow(
							column(4, numericInput(inputId = ns("axis_size_2"), label = "Text size:", value = 18, step = 0.5)),
							column(4, numericInput(inputId = ns("title_size_2"), label = "Title size:", value = 20, step = 0.5)),
							column(4, numericInput(inputId = ns("label_size_2"), label = "Label size:", value = 5, step = 0.5)),
						),				
						div(h4("4. Adjust lab and title name:"),style="width:400px;"),
						fluidRow(
							column(4, textInput(inputId = ns("x_name_2"), label = "X-axis name:", value = "-log10(P-value)")),
							column(4, textInput(inputId = ns("title_name_2"), label = "Title name:",
								value = NULL))
						),
						div(h5("Note: You can download the raw data and plot in local R environment for more detailed adjustment."))
			      	),
			      	br(),
					shinyWidgets::actionBttn(
						ns("sur_analysis_bt_multi"), "Run",
				        style = "gradient",
				        icon = icon("chart-line"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					fluidRow(
						column(10, offset = 1,
							   plotOutput({ns("sur_plot_multi")}, height = "500px") 
						)
					),
					br(),
					h4(strong("S3.3 Download results")), 
				    download_res_UI(ns("download_res2sur"))
				)
			)
		)
	)
}


server.modules_pancan_sur_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", filter_phe_id=NULL,
		phe_primary=query_tcga_group(database = "toil",cancer = "BRCA", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
		cancer_choose$phe_primary <- query_tcga_group(database = "toil",cancer = cancer_choose$name, return_all = T)
	})

	# 自定义上传metadata数据
	custom_meta = callModule(custom_meta_Server, "custom_meta2sur",database = "toil")
	# signature
	sig_dat = callModule(add_signature_Server, "add_signature2sur",database = "toil")

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

	# 数据源设置
	opt_pancan = callModule(mol_origin_Server, "mol_origin2sur", database = "toil")


	## 过滤样本
	# exact filter module
	filter_phe_id = callModule(filter_samples_Server, "filter_samples2sur",
					   database = "toil",
					   cancers=reactive(cancer_choose$name),
					   custom_metadata=reactive(custom_meta_sig()),
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
		sur_dat_raw = load_data("tcga_surv") %>% dplyr::rename("Sample"="sample")
		cli_dat_raw = load_data("tcga_clinical") %>% dplyr::rename("Sample"="sample")
		sur_dat_sub = sur_dat_raw %>%
			dplyr::filter(Sample %in% cancer_choose$filter_phe_id) %>%
			dplyr::select("Sample",contains(input$endpoint_type)) %>%
			dplyr::mutate(cancer = cli_dat_raw$type[match(Sample,cli_dat_raw$Sample)],.before = 1) %>%
			na.omit()
		colnames(sur_dat_sub)[3:4] = c("status","time")
		sur_dat_sub
	})


	# 设置分组
	group_final = callModule(group_samples_Server, "group_samples2sur",
					   	   database = "toil",
						   cancers=reactive(cancer_choose$name),
						   samples=reactive(sur_dat_v1()$Sample),
						   custom_metadata=reactive(custom_meta_sig()),
						   opt_pancan = reactive(opt_pancan())
						   )

	# 合并分组与生存
	sur_res_multi = reactiveValues(sur_dat = NULL, cutoff=NULL, sur_res = NULL)	

	group_sur_final = reactive({
		dat = dplyr::inner_join(group_final(),sur_dat_v1()[,-1],by=c("Sample"="Sample"))
		## 验证是否只有一组分组的癌症
		dat = dat %>%
			dplyr::filter(Cancer %in% sort(unique(dat$Cancer))[
				apply(table(dat$Cancer,dat$group),1,function(x) {min(x)>=2})])
		dat
	})



	# 根据选项，动态更新绘图参数
	output$multi_params.ui = renderUI(
		if(input$sur_method=="Log-rank test"){
		  	fluidRow(
		  		column(3,colourpicker::colourInput(ns("multi_log_color1"), "Color (Group 1):", "#d53e4f")),
		  		column(3,colourpicker::colourInput(ns("multi_log_color2"), "Color (Group 2):", "#3288bd")),
		  	)
		} else if(input$sur_method=="Univariate Cox regression") {
		  	fluidRow(
		  		column(5,colourpicker::colourInput(ns("cox_h_g1_color"), "Color(HR>1):", "#d53e4f")),
		  		column(5,colourpicker::colourInput(ns("cox_h_l1_color"), "Color(HR<1):", "#3288bd")),
		  	)
		}
	)

	observeEvent(input$sur_analysis_bt_multi, {
		sur_res_multi$sur_dat = group_sur_final()	

		if(input$sur_method=="Log-rank test"){
			if(!input$use_origin){ #是否使用分组前的原始值
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
			} else {
				if(class(sur_res_multi$sur_dat$origin) != "character"){ #若原始值为数值型，则寻找最佳阈值
					# res.cut <- surv_cutpoint(sur_res_multi$sur_dat, time = "time", event = "status", variables = "Group")
					# res.cat <- surv_categorize(res.cut)
					sur_res_multi$sur_dat = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x) {
						sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
						groups_1_2 = sur_dat_sub %>% 
							  dplyr::group_by(group) %>% 
							  dplyr::summarise(mean = mean(origin)) %>% 
							  dplyr::arrange(mean) %>% 
							  dplyr::pull(group) %>% as.character()
						res.cut <- surv_cutpoint(sur_dat_sub, time = "time", event = "status", variables = "origin")
						sur_dat_sub$Group = ifelse(surv_categorize(res.cut)$origin=="low", groups_1_2[1], groups_1_2[2])
						if(min(table(sur_dat_sub$Group))==0) return(NULL)
						sur_dat_sub$Group = factor(sur_dat_sub$Group, levels=groups_1_2)
						# print(table(sur_dat_sub$Group))
						sur_dat_sub
					}) %>% do.call(rbind, .) %>% as.data.frame()
					# print(table(sur_res_multi$sur_dat$Group, sur_res_multi$sur_dat$Cancer))
				} else {
					# sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
				}
			}
			sur_res_multi$sur_res = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x){
			  sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
			  
			  surv_diff <- survdiff(Surv(time, status) ~ Group, data = sur_dat_sub)
			  pval = 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
			  
			  summary(survfit(Surv(time, status) ~ Group, data = sur_dat_sub))$table %>% 
			    as.data.frame() %>% tibble::rownames_to_column("Group") %>% 
			    dplyr::mutate(Cancer = x, .before = 1) %>% 
			    dplyr::mutate(p.value = pval)
			}) %>% do.call(rbind, .) %>% as.data.frame()

		} else if (input$sur_method=="Univariate Cox regression"){
			if(!input$use_origin){
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
			} else {
				if(class(sur_res_multi$sur_dat$origin) != "character"){
					sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$origin
				} else {
					sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
				}
			}
			sur_res_multi$sur_res = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x){
			  sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
			  fit = coxph(Surv(time, status) ~ Group , data = sur_dat_sub)
			  summary(fit)$coefficients %>% as.data.frame() %>% 
			    tibble::rownames_to_column("Group") %>% 
			    dplyr::mutate(Cancer = x, .before = 1)
			}) %>% do.call(rbind, .) %>% as.data.frame()
		}
	})


	sur_plot_multi = eventReactive(input$sur_analysis_bt_multi,{
		shiny::validate(
			need(try(nrow(sur_res_multi$sur_dat)>0), 
				"Please inspect whether to set valid groups in S2 step."),
		)
		p = plot_sur_02m(
			sur_res_multi, sur_method=input$sur_method, 
			multi_log_color1=input$multi_log_color1, multi_log_color2=input$multi_log_color2, 
			x_name=input$x_name, title_name=input$title_name, multi_log_line=input$multi_log_line, 
			axis_size=input$axis_size, title_size=input$title_size, multi_log_label=input$multi_log_label,
        	label_size=input$label_size, cox_h_g1_color=input$cox_h_g1_color, cox_h_l1_color=input$cox_h_l1_color, 
			x_name_2=input$x_name_2, title_name_2=input$title_name_2, multi_cox_line=input$multi_cox_line, 
			axis_size_2=input$axis_size_2, title_size_2=input$title_size_2, 
			multi_cox_label=input$multi_cox_label, label_size_2=input$label_size_2,
			custom_theme=themes_list[[input$theme]]
		)
		p
	})

	# output$sur_plot_one = renderPlot({sur_plot_one()})
	output$sur_plot_multi = renderPlot({sur_plot_multi()})

	# Download results
	observeEvent(input$sur_analysis_bt_multi,{
		res1 = sur_plot_multi()
		res2 = sur_res_multi$sur_dat
		res3 = sur_res_multi$sur_res
		callModule(download_res_Server, "download_res2sur", res1, res2, res3)
	})
}