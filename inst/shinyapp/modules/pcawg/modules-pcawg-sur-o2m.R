ui.modules_pcawg_sur_o2m = function(id) {
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

					h4(strong("S1.2 Choose projects")) %>% 
						helper(type = "markdown", size = "l", fade = TRUE, 
					                   title = "PCAWG projects", 
					                   content = "pcawg_projects"),
					pickerInput(
						ns("choose_cancers"),NULL,
						choices = pcawg_items,
						multiple = TRUE,
						selected = pcawg_items,
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
			# 选择生存资料
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
					                   content = "analyze_sur_2"),  
					h4(strong("S3.1 Set analysis parameters")), 
					selectInput(ns("sur_method"), "Survival method:",
						choices = c("Log-rank test", "Univariate Cox regression")),
				    materialSwitch(ns("use_origin"), 
				    	"Whether use initial data before grouping?") %>% 
						helper(type = "markdown", size = "m", fade = TRUE, 
					                   title = "About the initial phenotype", 
					                   content = "sur_initial_group"),
					h4(strong("S3.2 Set visualization parameters")), 
			      	uiOutput(ns("multi_params.ui")),

			      	dropMenu(
						actionButton(ns("more_visu"), "Set more visualization params"),
						placement = "left",
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

					h4(strong("S3.3 Download results")), 
				    br(),
				    fluidRow(
				    	column(3, downloadButton(ns("save_plot_bt"), "Figure")),
				    	column(3, offset = 0, downloadButton(ns("save_data_raw"), "Raw data(.csv)")),
				    	column(3, offset = 1, downloadButton(ns("save_data_res"), "Analyzed data(.csv)"))
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


server.modules_pcawg_sur_o2m = function(input, output, session) {
	ns <- session$ns

	# 记录选择癌症
	cancer_choose <- reactiveValues(name = "BLCA-US", phe_primary="",
		filter_phe_id=query_tcga_group(database = "pcawg", cancer = "BLCA-US", return_all = T))
	observe({
		cancer_choose$name = input$choose_cancers
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
		  		# column(4,colourpicker::colourInput(ns("multi_cox_color"), "Color:", "grey")),
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
						sur_dat_sub
					}) %>% do.call(rbind, .) %>% as.data.frame()
				} else {
					sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$group
				}

			}
			sur_res_multi$sur_res = lapply(sort(unique(sur_res_multi$sur_dat$Cancer)), function(x){
			  sur_dat_sub = subset(sur_res_multi$sur_dat, Cancer==x)
			  

			  #如果只有一个有效分组
			  if(length(unique(sur_dat_sub$Group))==1) return(NULL)

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
				sur_res_multi$sur_dat$Group = sur_res_multi$sur_dat$origin
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
		if(input$sur_method=="Log-rank test"){
			dat = sur_res_multi$sur_res
			print(head(dat))
			pval_df = dat %>%
			  dplyr::select(Cancer, Group, rmean, p.value) %>% 
			  dplyr::group_by(Cancer) %>% 
			  dplyr::mutate(Risk = ifelse(rmean[1]>rmean[2],
			                              paste0("Low risk(", Group[1],")"), 
			                              paste0("High risk(", Group[1],")"))) %>% 
			  dplyr::distinct(Cancer, p.value, Risk) %>% as.data.frame() %>% 
			  dplyr::mutate(Cancer = factor(Cancer, levels = rev(sort(Cancer))))
			# print(head(pval_df))
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
			  xlab(NULL) + ylab(isolate(input$x_name)) +
			  ggtitle(label = isolate(input$title_name)) +
			  geom_hline(yintercept = -log10(input$multi_log_line), color = "red") +
			  coord_flip() +
			  theme_minimal() +
			  theme(legend.position = "top",
			        plot.margin = margin(0,0,0,0),
			        text = element_text(size=input$axis_size),
					plot.title = element_text(size=isolate(input$title_size), hjust = 0.5)
			        )  
			if(input$multi_log_label=="Signif.(value)"){
				p2 =  ggplot(pval_df) +
				  geom_text(aes(label=formatC(p.value, format = "e", digits = 2),
				                x=Cancer,y=1),,size = isolate(input$label_size)) +
				  coord_flip()
			} else if (input$multi_log_label=="Signif.(symbol)"){
				p2 = pval_df %>%
				  dplyr::mutate(p.label=case_when(
				    p.value  < 0.001 ~ "***",
				    p.value  < 0.01 ~ "**",
				    p.value  < 0.05 ~ "*",
				    TRUE ~ "ns"
				  )) %>% ggplot() +
				  geom_text(aes(label=p.label,x=Cancer,y=1),size = isolate(input$label_size)) + 
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
			# fill_cols = c(input$multi_cox_color)
			# names(fill_cols) = c(
			# 	paste0("Group",levels(sur_res_multi$sur_dat$Group)[2]))
			fill_cols = c(input$cox_h_g1_color, input$cox_h_l1_color)
			names(fill_cols) = c("HR>1","HR<1")
			# print(head(sur_res,20))
			sur_res = sur_res %>% dplyr::filter(!is.na(coef))
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
			  ggplot(aes(x = Cancer, y = pval_log, fill = Direct)) + 
			  geom_col(position="dodge") + 
			  scale_fill_manual(values = fill_cols) + 
			  xlab(NULL) + ylab(isolate(input$x_name_2)) +
			  ggtitle(label = isolate(input$title_name_2)) +
			  geom_hline(yintercept = -log10(input$multi_cox_line), color = "red") +
			  coord_flip() +
			  theme_minimal() +
			  theme(legend.position = "top",
			        text = element_text(size=input$axis_size_2),
					plot.title = element_text(size=isolate(input$title_size_2), hjust = 0.5)
			        )  
			if(input$multi_cox_label=="HR value"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = HR.label), size = isolate(input$label_size_2),
				          position=position_dodge(width=0.9), hjust = 1)
			} else if (input$multi_cox_label=="Signif.(symbol)"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = p.value_symbol), size = isolate(input$label_size_2),
				          position=position_dodge(width=0.9), hjust = 1)
			} else if (input$multi_cox_label=="Signif.(value)"){
				p = p1 + 
				  geom_text(aes(y = max(pval_log),label = p.value_label), size = isolate(input$label_size_2),
				          position=position_dodge(width=0.9), hjust = 1)
			}
		}
		p
	})

	output$sur_plot_multi = renderPlot({sur_plot_multi()})


	# 3个下载按钮
	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("Bar", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",input$save_plot_F)
		},
		content = function(file){
			p = sur_plot_multi()
			
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
			p_raw = sur_res_multi$sur_dat
			write.csv(p_raw, file, row.names = FALSE)
		}
	)

	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Survival_result_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			p_raw = sur_res_multi$sur_res
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
}


