ui.modules_pancan_sur = function(id) {
	ns = NS(id)
	fluidPage(
		fluidRow(
			# 初始设置
			column(
				2,
				wellPanel(
					style = "height:1100px",
					h2("S1: Preset", align = "center"),

					h4("1. Choose cancer(s)"),
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
			# 选择生存资料
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S2: Select survival event", align = "center"),

				    shinyWidgets::prettyRadioButtons(
				        inputId = ns("endpoint_type"), label = "Endpoint type:",
				        choiceValues = c("OS", "DSS", "DFI", "PFI"),
				        choiceNames = c("OS (Overall Survial)", "DSS (Disease-Specific Survival)", 
				        				"DFI (Disease-Free Interval)", "PFI (Progression-Free Interval)"),
				        selected = "OS"
				    ),
				    br(),br(),
				    uiOutput(ns("sur_dat_v1.ui")),
				)
			),
			# 选择分组
			column(
				3,
				wellPanel(
					style = "height:1100px",
					h2("S3: Group samples", align = "center"),
					group_samples_UI(ns("group_samples2sur"))  

				)
			),
			# 分析/可视化/下载
			column(
				4,
				wellPanel(
					style = "height:1100px",
					h2("S4: Analyze", align = "center"),
				    # 绘图按钮
					uiOutput(ns("sur_analysis_bt.ui")),

					selectInput(ns("sur_method"), "1. Survival metohd",
						choices = c("Log-rank test", "Univariate Cox regression")),

				    materialSwitch(ns("use_origin"), 
				    	"2. Whether use initial data before grouping") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "About the initial phenotype", 
					                   content = "sur_initial_group"),


					br(),

				    tabsetPanel(id = ns("plot_layout"),
				      tabPanel("Curve(single cancer)",
				      	br(),
				      	uiOutput(ns("one_params.ui")),
				      	plotOutput({ns("sur_plot_one")}, height = "500px")
				      ),
				      tabPanel("Lineplot(multi-cancers)",
				      	br(),
				      	uiOutput(ns("multi_params.ui")),
				      	plotOutput({ns("sur_plot_multi")}, height = "500px"),
					    h4("Note:"),
					    p("For visualization, the maximum -log10(p.value) is limited to 10."),
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

server.modules_pancan_sur = function(input, output, session, cancer="BRCA") {
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
	output$sur_analysis_bt.ui = renderUI(
		if(input$overall_mode == "Single cancer"){
			shinyWidgets::actionBttn(
				ns("sur_analysis_bt_single"), "Go/Update Curve",
		        style = "gradient",
		        icon = icon("chart-line"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			)
		} else if(input$overall_mode == "Multi-cancers"){
			shinyWidgets::actionBttn(
				ns("sur_analysis_bt_multi"), "Go/Update Lineplot",
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
	  		`Single cancer`="Curve(single cancer)",
	  		`Multi-cancers`="Lineplot(multi-cancers)"))
	}) 



	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BRCA", filter_phe_id=NULL,
		phe_primary=query_tcga_group(cancer = "BRCA", return_all = T))
	observe({
		if(input$overall_mode == "Single cancer"){
			cancer_choose$name = input$choose_cancer
		} else if(input$overall_mode == "Multi-cancers"){
			cancer_choose$name = input$choose_cancers
		}
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



	output$sur_dat_v1.ui = renderUI({
		output$sur_dat_v1 = renderDataTable({
			sur_dat_v1_ = sur_dat_v1()
			colnames(sur_dat_v1_)[3:4] = paste0(input$endpoint_type,c(".status",".time"))
			datatable(sur_dat_v1_, 
				options = list(pageLength = 5,
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		}) 
		dataTableOutput(ns("sur_dat_v1"))
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
	sur_res_multi = reactiveValues(sur_dat = NULL, cutoff=NULL, sur_res = NULL)	

	group_sur_final = reactive({
		dat = dplyr::inner_join(group_final(),sur_dat_v1()[,-1],by=c("Sample"="sample"))
		## 验证是否只有一组分组的癌症
		dat = dat %>%
			dplyr::filter(Cancer %in% sort(unique(dat$Cancer))[
				apply(table(dat$Cancer,dat$group),1,function(x) {min(x)>=1})])
		dat
	})
	output$phe_sur_dat = renderPrint({head(group_sur_final())})



	# output$phe_sur_table.ui = renderUI({
	# 	shiny::validate(
	# 		need(try(nrow(group_sur_final())>0), 
	# 			"Please inspect whether to set valid groups in S3 step."),
	# 	)

	# 	output$phe_sur_table = renderDataTable({
	# 		datatable(group_sur_final()[,c("Cancer","Sample","phenotype","group")], 
	# 			options = list(pageLength = 5,
	# 				columnDefs = list(list(className = 'dt-center', targets="_all")))
	# 		)
	# 	}) 
	# 	dataTableOutput(ns("phe_sur_table"))
	# })

	# 根据选项，动态更新绘图参数
	output$multi_params.ui = renderUI(
		if(input$sur_method=="Log-rank test"){
		  	fluidRow(
		  		column(3,colourpicker::colourInput(ns("multi_log_color1"), "Bar color1", "#d53e4f")),
		  		column(3,colourpicker::colourInput(ns("multi_log_color2"), "Bar color2", "#3288bd")),
		  		column(3, numericInput(ns("multi_log_line"), "Add line(P)", 0.05)),
		  		column(3, selectInput(ns("multi_log_label"),"Add text",
		  			choices = c("Signif.(symbol)", "Signif.(value)"),selected = "Signif.(symbol)"))	
		  	)
		} else if(input$sur_method=="Univariate Cox regression") {
		  	fluidRow(
		  		column(4,colourpicker::colourInput(ns("multi_cox_color"), "Bar color", "grey")),
		  		column(4, numericInput(ns("multi_cox_line"), "Add line(P)", 0.05)),
		  		column(4, selectInput(ns("multi_cox_label"),"Add text",
		  			choices = c("HR value", "Signif.(symbol)", "Signif.(value)"),selected = "HR value"))	
		  	)
		}
	)

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
				sur_res_one$sur_dat$Group = sur_res_one$sur_dat$origin
				if(class(group_sur_final()$origin) != "character"){ #若原始值为数值型，则寻找最佳阈值
					res.cut <- surv_cutpoint(sur_res_one$sur_dat, time = "time", event = "status", variables = "Group")
					res.cat <- surv_categorize(res.cut)
					sur_res_one$sur_dat$Group = res.cat$Group
				}
			}
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
			p = ggforest(fit,data = dat)
		}
		p
	})

	observeEvent(input$sur_analysis_bt_multi, {
		sur_res_multi$sur_dat = group_sur_final()	

		if(input$sur_method=="Log-rank test"){
			if(!input$use_origin){ #是否使用分组前的原始值
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
			} else {
				if(class(sur_res_multi$sur_dat$origin) != "character"){ #若原始值为数值型，则寻找最佳阈值
					res.cut <- surv_cutpoint(sur_res_multi$sur_dat, time = "time", event = "status", variables = "Group")
					res.cat <- surv_categorize(res.cut)
					sur_res_multi$sur_dat = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x) {
						sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
						res.cut <- surv_cutpoint(sur_dat_sub, time = "time", event = "status", variables = "origin")
						sur_dat_sub$Group = surv_categorize(res.cut)$origin
					}) %>% do.call(rbind, .)
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
			}) %>% do.call(rbind, .)

		} else if (input$sur_method=="Univariate Cox regression"){
			if(!input$use_origin){
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
			} else {
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$origin
			}
			sur_res_multi$sur_res = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x){
			  sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
			  fit = coxph(Surv(time, status) ~ Group , data = sur_dat_sub)
			  summary(fit)$coefficients %>% as.data.frame() %>% 
			    tibble::rownames_to_column("Group") %>% 
			    dplyr::mutate(Cancer = x, .before = 1)
			}) %>% do.call(rbind, .)
		}
	})

	sur_plot_multi = eventReactive(input$sur_analysis_bt_multi,{
		shiny::validate(
			need(try(nrow(sur_res_multi$sur_dat)>0), 
				"Please inspect whether to set valid groups in S3 step."),
		)
		if(input$sur_method=="Log-rank test"){
			dat = sur_res_multi$sur_res
			pval_df = dat %>%
			  dplyr::select(Cancer, Group, rmean, p.value) %>% 
			  dplyr::group_by(Cancer) %>% 
			  dplyr::mutate(Risk = ifelse(rmean[1]>rmean[2],
			                              paste0("Low risk(", Group[1],")"), 
			                              paste0("High risk(", Group[1],")"))) %>% 
			  dplyr::distinct(Cancer, p.value, Risk) %>% as.data.frame() %>% 
			  dplyr::mutate(Cancer = factor(Cancer, levels = rev(sort(Cancer))))
			fill_cols = c(input$multi_log_color1, input$multi_log_color2)
			names(fill_cols) = c(
				paste0("Low risk(Group=",levels(sur_res_multi$sur_dat$Group)[1], ")"),
				paste0("High risk(Group=",levels(sur_res_multi$sur_dat$Group)[1], ")"))
			p1 = pval_df %>% 
			  dplyr::mutate(pval_log = -log10(p.value)) %>% 
			  dplyr::mutate(pval_log = ifelse(pval_log<10,pval_log, 10)) %>% 
			  ggplot(aes(x = Cancer, y = pval_log, fill = Risk)) + 
			  geom_col() +
			  scale_fill_manual(values = fill_cols) + 
			  xlab(NULL) + ylab("- log10(P-value)") +
			  geom_hline(yintercept = -log10(input$multi_log_line), color = "red") +
			  coord_flip() +
			  theme_minimal() +
			  theme(legend.position = "top",
			        plot.margin = margin(0,0,0,0),
			        text = element_text(size=15))  
			if(input$multi_log_label=="Signif.(value)"){
				p2 =  ggplot(pval_df) +
				  geom_text(aes(label=formatC(p.value, format = "e", digits = 2),
				                x=Cancer,y=1),) +
				  coord_flip()
			} else if (input$multi_log_label=="Signif.(symbol)"){
				p2 = pval_df %>%
				  dplyr::mutate(p.label=case_when(
				    p.value  < 0.001 ~ "***",
				    p.value  < 0.01 ~ "**",
				    p.value  < 0.05 ~ "*",
				    TRUE ~ "ns"
				  )) %>% ggplot() +
				  geom_text(aes(label=p.label,x=Cancer,y=1)) + coord_flip()
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
			p = p1 + p2 + plot_layout(widths = c(5,1))
			p
		} else if (input$sur_method=="Univariate Cox regression") {
			dat = sur_res_multi$sur_dat
			sur_res = lapply(sort(unique(dat$Cancer)), function(x){
			  sur_dat_sub = subset(dat, Cancer==x)
			  fit = coxph(Surv(time, status) ~ Group , data = sur_dat_sub)
			  summary(fit)$coefficients %>% as.data.frame() %>% 
			    tibble::rownames_to_column("Group") %>% 
			    dplyr::mutate(Cancer = x, .before = 1)
			}) %>% do.call(rbind, .)
			fill_cols = c(input$multi_cox_color)
			names(fill_cols) = c(
				paste0("Group",levels(sur_res_multi$sur_dat$Group)[2]))
			p1 = sur_res %>%
			  dplyr::rename(`p.value`=`Pr(>|z|)`, `HR` = `exp(coef)`) %>% 
			  dplyr::select(Cancer, Group, HR, p.value) %>% 
			  dplyr::mutate(Cancer = factor(Cancer, levels = rev(sort(Cancer)))) %>%
			  dplyr::mutate(HR.label = paste0("HR=",formatC(HR, format = "e", digits = 2))) %>%
			  dplyr::mutate(p.value_label = formatC(p.value, format = "e", digits = 2)) %>% 
			  dplyr::mutate(p.value_symbol=case_when(
			    p.value  < 0.001 ~ "***",
			    p.value  < 0.01 ~ "**",
			    p.value  < 0.05 ~ "*",
			    TRUE ~ "ns"
			  )) %>% 
			  dplyr::mutate(pval_log = -log10(p.value)) %>% 
			  dplyr::mutate(pval_log = ifelse(pval_log<10,pval_log, 10)) %>% 
			  dplyr::mutate(Direct = ifelse(HR>1,"HR>1","HR<1")) %>% 
			  ggplot(aes(x = Cancer, y = pval_log, fill = Group)) + 
			  geom_col(position="dodge") + 
			  scale_fill_manual(values = fill_cols) + 
			  xlab(NULL) + ylab("- log10(P-value)") +
			  geom_hline(yintercept = -log10(input$multi_cox_line), color = "red") +
			  coord_flip() +
			  theme_minimal() +
			  theme(legend.position = "top",
			        text = element_text(size=15))  
			if(input$multi_cox_label=="HR value"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = HR.label),
				          position=position_dodge(width=0.9), hjust = 1)
			} else if (input$multi_cox_label=="Signif.(symbol)"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = p.value_symbol),
				          position=position_dodge(width=0.9), hjust = 1)
			} else if (input$multi_cox_label=="Signif.(value)"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = p.value_label),
				          position=position_dodge(width=0.9), hjust = 1)
			}
		}
		p
	})

	output$sur_plot_one = renderPlot({sur_plot_one()})
	output$sur_plot_multi = renderPlot({sur_plot_multi()})


	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0(switch(input$overall_mode, `Single cancer` = "Curve", `Multi-cancers` = "Bar"),
						  "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = switch(input$overall_mode,
				`Single cancer` = sur_plot_one(),
				`Multi-cancers` = sur_plot_multi()
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
			paste0("Survival_rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = switch(input$overall_mode, 
				`Single cancer` = sur_res_one$sur_dat, `Multi-cancers` = sur_res_multi$sur_dat)
			write.csv(p_raw, file, row.names = FALSE)
		}
	)

	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Survival_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = switch(input$overall_mode, 
				`Single cancer` = sur_res_one$sur_res, `Multi-cancers` = sur_res_multi$sur_res)
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
}