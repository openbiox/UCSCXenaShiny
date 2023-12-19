ui.modules_dim_dist = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				3,
				wellPanel(
		            div(actionButton(ns("toggleBtn"), "Modify datasets[opt]",icon = icon("folder-open")),
		                style = "margin-bottom: 5px;"),
		            conditionalPanel(
		              ns = ns,
		              condition = "input.toggleBtn % 2 == 1",
		              mol_origin_UI(ns("mol_origin2quick"), database = "toil")
		            ),
					selectInput(
						ns("profile"), "Select a genomic profile:",
						choices = c("mRNA Expression", "Transcript Expression", "DNA Methylation", 
									"Protein Expression", "miRNA Expression", "Copy Number Variation"),
						selected = "mRNA Expression"),
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("input_ways"), label = "Input the molecule ids (>=3) by ?",
		              choiceValues = c("Select", "Pathway", "File"),
		              choiceNames = c("Select", "Pathway", "File"),
		              animation = "jelly",
		              inline = TRUE
		            ),

					tabsetPanel(
					  id = ns("input_params"),
					  type = "hidden",
					  tabPanel("Select",
			        	selectizeInput(
						inputId = ns("ids_ways_1"),
						label = NULL,
						choice = NULL,
						width = "100%",
						multiple = TRUE, 
						options = list(
							create = TRUE,
							maxOption = 5,
							placeholder = "Enter the ids of selected profile",
							plugins = list("restore_on_backspace")))),
					  tabPanel("Pathway", 
			        	selectizeInput(
							inputId = ns("ids_ways_2"),
							label = NULL,
							choice = NULL,
							width = "100%",
							multiple = FALSE, 
							options = list(
								placeholder = "Choose the patwhay geneset(Symbol)"))),
						tabPanel("File",
							fileInput(ns("ids_ways_3"), NULL, placeholder = "One column molecule id file(.txt)" )
						)
					),  

					shinyWidgets::actionBttn(
						inputId = ns("query_data"),
						label = "STEP1: Cache matrix data",
						style = "gradient",
						color = "default",
						block = TRUE,
						size = "sm"
					)
				),
				wellPanel(

					pickerInput(
						ns("choose_cancer"), "Choose cancer(s)",
						choices = sort(tcga_cancer_choices),
						multiple = TRUE,
						selected = "BRCA",
						options = list(`actions-box` = TRUE)
					),
					materialSwitch(ns("add_phe"), "Add customized phenotype", inline = FALSE),

			        conditionalPanel(
			          condition = "input.add_phe",
			          fileInput(ns("add_phe_data"), NULL),
			          ns = ns
			        ),

					selectInput(ns("choose_group"), "Choose group",
						choices = NULL,
						selected = "Code"
					),
					shinyWidgets::actionBttn(
						inputId = ns("check_group"),
						label = "STEP2: Check or Handle group",
						style = "gradient",
						color = "default",
						block = TRUE,
						size = "sm"
					)
				),

				wellPanel(
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("method"), label = "Select Dimension-Reduction method:",
		              choiceValues = c("PCA", "UMAP", "tSNE"),
		              choiceNames = c("PCA", "UMAP", "tSNE"),
		              inline = TRUE,
		              animation = "jelly"
		            ),
		        	fluidRow(
		        		column(6,selectInput(ns("margin"), "Marginal plot",c('NONE', "density" ,"boxplot"))),
		        		column(6,selectInput(ns("palette"), "Color setting",c("Set1", "Set2", "Set3", "Paired", "Pastel1", "Pastel2", "Accent", "Dark2")))
		        	),
			        shinyWidgets::actionBttn(
			          inputId = ns("plot_bttn"),
			          label = "STEP3: 2D data visualization",
			          style = "gradient",
			          # icon = icon("search"),
			          color = "default",
			          block = TRUE,
			          size = "sm"
			        ),
			        textOutput(ns("vis_dim_data_stat"))

				),
		        wellPanel(
		        	fluidRow(
		        		column(6,numericInput(inputId = ns("height"), label = "Height", value = 8)),
		        		column(6,numericInput(inputId = ns("height"), label = "Height", value = 8))
		        	),
			        prettyRadioButtons(
			          inputId = ns("device"),
			          label = "Choose plot format",
			          choices = c("pdf", "png"),
			          selected = "pdf",
			          inline = TRUE,
			          icon = icon("check"),
			          animation = "jelly",
			          fill = TRUE
			        ),
			        downloadBttn(
			          outputId = ns("download"),
			          label = "STEP4: Download",
			          style = "gradient",
			          color = "default",
			          block = TRUE,
			          size = "sm"
			        )
			    )
			),
			column(
				9,
				plotOutput(ns("dim_plot"), height = "600px",width = "600px"),
				shinyjs::hidden(
					wellPanel(
						id = ns("out_tables"),
					    tabsetPanel(
					      tabPanel("DR", 
					      	dataTableOutput(ns("out_dr"))
					      ),
					      tabPanel("Exp", 
					      	dataTableOutput(ns("out_exp"))
					      )
					    ),
						fluidRow(
							column(2, downloadButton(ns("download_csv_dr"), "Save as csv(DR)")),
							column(2, downloadButton(ns("download_csv_exp"), "Save as csv(Exp)"))
							
						)
					)

				),

			    hr(),
			    h5("NOTEs:"),
			    p("1. When you want to upload custom gene set, one column .txt fle with \\n as seperator without column name is expected"),
			    p("----Too many genes (>100) may take long time to query and please be patient."),
			    p("2. When you want to upload custom grouping data, you shoud upload a two-column dataframe  with comma as seperator (.csv))" ),
			    p("----First column ('sample') is TCGA sample id (15 characters) and Second column ('group') is custom grouping information;"),
			    p("3. ", tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source")),
			    tags$br()
			)

		)
	)

}



server.modules_dim_dist = function(input, output, session){
	ns <- session$ns


	profile = reactive({
	  switch(input$profile,
	    `mRNA Expression`="mRNA",
	    `DNA Methylation` = "methylation",
	    `Protein Expression` = "protein",
	    `Transcript Expression` = "transcript",
	    `miRNA Expression` = "miRNA",
	    `Copy Number Variation` = "cnv",
	    list(all = "NONE", default = "NONE")
	  )
	})

	profile_choices <- reactive({
	  switch(profile(),
	    mRNA = list(all = pancan_identifiers$gene, default = c("TP53", "KRAS", "PTEN")),
	    methylation = list(all = pancan_identifiers$gene, default = c("TP53", "KRAS", "PTEN")),
	    protein = list(all = pancan_identifiers$protein, default = c("P53", "GATA3", "PTEN")),
	    transcript = list(all = load_data("transcript_identifier"), default = c("ENST00000269305","ENST00000311936","ENST00000371953")),
	    miRNA = list(all = pancan_identifiers$miRNA, default = c("hsa-miR-522-3p","hsa-miR-1271-5p","hsa-miR-518e-3p")),
	    cnv = list(all = pancan_identifiers$gene,default = c("TP53", "KRAS", "PTEN")),
	    list(all = "NONE", default = "NONE")
	  )
	})

	opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database = "toil")

	observe({
	  updateSelectizeInput(
	    session,
	    "ids_ways_1",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default,
	    server = TRUE
	  )
	  updateSelectizeInput(
	    session,
	    "ids_ways_2",
	    choices = split(PW_meta$display, PW_meta$Type),
	    selected = "SULFUR_METABOLISM (13)",
	    server = TRUE
	  )
	})

    observeEvent(input$reset_1, {
        reset("ids")
    })
    observeEvent(input$reset_2, {
        reset("upload")
    })

	observeEvent(input$input_ways, {
	  updateTabsetPanel(inputId = "input_params", selected = input$input_ways)
	}) 

    ids = eventReactive(input$query_data, {
    	ids = switch(input$input_ways,
    		"Select" = input$ids_ways_1,
    		"Pathway" = strsplit(PW_meta$Gene[PW_meta$display==input$ids_ways_2], "/", fixed = TRUE)[[1]],
    		"File" = read.table(input$ids_ways_3$datapath, sep = "\n")[,1]
    	)
	    unique(ids)
	})
	
	observeEvent(input$query_data, {
		withProgress(message=paste0("Query ", length(ids()), " molecules one by one"),{
		  purrr::map(ids(), function(x) {
		    # x = ids[1]
		    data <- query_pancan_value(x, data_type=profile())
		    data = data[[1]]
		    data <- dplyr::tibble(sample = names(data), y = as.numeric(data))
		    colnames(data)[2] <- x
		    incProgress(1 / length(ids()))
		    data
		  }) %>% purrr::reduce(dplyr::full_join, by = "sample")
	    })
	})


	group_add = reactive({
		# group_build_in = query_tcga_group(cancer = input$choose_cancer,return_all = T)
		if(!is.null(input$add_phe_data$datapath)){
			group_add = read.csv(input$add_phe_data$datapath)
		} else {
			group_add = NULL
		}
		group_add
	})

	group_general = reactive({
		query_tcga_group(cancer = input$choose_cancer,
			custom=group_add(),return_all = T)
	})

	observe({
		updateSelectizeInput(
			session,
			"choose_group",
			choices = colnames(group_general())[-1:-2],
			selected = "Code",
			server = TRUE
		)
	})


	observeEvent(input$check_group, {
	  message("Preprocess button is clicked by user.")

	  showModal(
	    modalDialog(
	      title = "Check and Handler grouping data",
	      footer = modalButton("Done!"),
	      size = "l",
	      fluidPage(
	          wellPanel(
	            h5("1. Phenotype distribution"),
				selectInput(ns("choose_group_2stat"), "Observe group",
					choices = colnames(group_general())[-1:-2], selected = input$choose_group
				),
				verbatimTextOutput(ns("choose_group_2stat_out"))
			  ),
	          wellPanel(
	            h5("2. Phenotype filtering"),
	            textInput(ns("filt_by_phe"),"(1) filter by which phenotypes:", value = "Stage_ajcc"),
	            textInput(ns("filt_by_cutoff"),"(2) filter by which cutoff:", value = "Stage I|Stage III"),
	            textInput(ns("filt_by_direct"),"(3) filter by which direction:", value = "+"),
	            fluidRow(
		            actionBttn(
		              inputId = ns("button_filter"),
		              label = "Run...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            ),
		            actionBttn(
		              inputId = ns("button_filter_reset"),
		              label = "Reset...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            )       
	            ),

				textOutput(ns("choose_group_2filt_out")),
				verbatimTextOutput(ns("choose_group_2filt_out2"))
			  ),
	          wellPanel(
	            h5("3. Main subgroups merging for group choosed"),
	            textInput(ns("spliting_item"),"(1) how to split items:", value = "NT||TM|TP"),
	            textInput(ns("splitting_name"),"(2) how to name new items:", value = "Normal||Tumor"),
	            fluidRow(
		            actionBttn(
		              inputId = ns("button_merge"),
		              label = "Run...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            ),
		            actionBttn(
		              inputId = ns("button_merge_reset"),
		              label = "Reset...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            ),
	            ),
	            verbatimTextOutput(ns("choose_group_2merge_out"))
	          ),
	          wellPanel(
	            h5("4. Group levels subtraction or order setting"),
	            # textInput(ns("spliting_item"),"(1) how to split items:", value = "NT||TM|TP"),
	            textInput(ns("levels_orders"),"", value = "Tumor||Normal"),
	            fluidRow(
		            actionBttn(
		              inputId = ns("button_levels"),
		              label = "Run...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            ),
		            actionBttn(
		              inputId = ns("button_levels_reset"),
		              label = "Reset...",
		              color = "primary",
		              style = "bordered", size = "sm",
		              block = F
		            )
	            ),
	            verbatimTextOutput(ns("choose_group_2levels_out"))
	          )
	      )
	    )
	  )
	})



	# group--step1
	output$choose_group_2stat_out = renderPrint({
		choose_group_2stat_tmp = group_general() %>%
			dplyr::filter(Cancer %in% input$choose_cancer)
		choose_group_2stat_tmp = choose_group_2stat_tmp[,input$choose_group_2stat]

		if(class(choose_group_2stat_tmp[,1,drop=TRUE])=="character"){
			choose_group_2stat_tmp[,1] = factor(choose_group_2stat_tmp[,1,drop=TRUE])
		}
		summary(choose_group_2stat_tmp)
	})

	# group--step2
	observeEvent(input$button_filter_reset, {
	  updateTextInput(session, "filt_by_phe", value = "")
	  updateTextInput(session, "filt_by_cutoff", value = "")
	  updateTextInput(session, "filt_by_direct", value = "")
	})

	filter_by = eventReactive(input$button_filter, {
		if(input$filt_by_phe==""){
			filter_by = NULL
		} else {
			filter_by = lapply(seq(length(strsplit(input$filt_by_phe,"||",fixed = T)[[1]])), function(i){
						  c(strsplit(input$filt_by_phe,"||",fixed = T)[[1]][i],
						    strsplit(input$filt_by_cutoff,"||",fixed = T)[[1]][i],
						    strsplit(input$filt_by_direct,"||",fixed = T)[[1]][i])
						})
		}
	})

	output$choose_group_2filt_out = renderText({
		filt_ids = query_tcga_group(cancer=input$choose_cancer,
			custom = group_add(),filter_by = filter_by())[["data"]]$Sample
		paste0(length(filt_ids), " samples were left.")
	})

	output$choose_group_2filt_out2 = renderPrint({
		choose_group_2filt_out2_tmp = query_tcga_group(cancer=input$choose_cancer,
			custom = group_add(),
			group = input$choose_group,filter_by = filter_by())[["data"]]
		choose_group_2filt_out2_tmp = choose_group_2filt_out2_tmp[,input$choose_group]
		if(class(choose_group_2filt_out2_tmp[,1,drop=TRUE])=="character"){
			choose_group_2filt_out2_tmp[,1] = factor(choose_group_2filt_out2_tmp[,1,drop=TRUE])
		}
		summary(choose_group_2filt_out2_tmp)
	})



	# group--step3
	observeEvent(input$button_merge_reset, {
	  updateTextInput(session, "spliting_item", value = "")
	  updateTextInput(session, "splitting_name", value = "")
	})
	merge_by = eventReactive(input$button_merge, {
		if(input$spliting_item==""){
			merge_by = NULL
		} else {
			merge_by = lapply(strsplit(input$spliting_item,"||",fixed = T)[[1]], function(x){
			    strsplit(x, "|", fixed = T)[[1]]
			})
			if(length(merge_by)<length(strsplit(input$splitting_name,"||",fixed = T)[[1]])){
			  merge_by = lapply(merge_by, as.numeric)
			  merge_by[[length(merge_by)+1]] = merge_by[[2]]
			}
			names(merge_by) = strsplit(input$splitting_name,"||",fixed = T)[[1]]
		}
		merge_by
	})

	output$choose_group_2merge_out = renderPrint({
		choose_group_2merge_tmp = query_tcga_group(cancer=input$choose_cancer,
			custom = group_add(), group = input$choose_group,
			filter_by = filter_by(), merge_by = merge_by())[['data']]
		choose_group_2merge_tmp = choose_group_2merge_tmp[,input$choose_group]
		choose_group_2merge_tmp[,1] = factor(choose_group_2merge_tmp[,input$choose_group,drop=TRUE])
		summary(choose_group_2merge_tmp)
	})

	# group -- step4

	observeEvent(input$button_levels_reset, {
	  updateTextInput(session, "levels_orders", value = "")
	})

	group_final = eventReactive(input$button_levels, {
		choose_group_2levels_tmp = query_tcga_group(cancer=input$choose_cancer,
			custom = group_add(), group = input$choose_group,
			filter_by = filter_by(), merge_by = merge_by())[['data']]

		if(length(input$levels_orders)==0){
			group_levels = sapply(strsplit(input$levels_orders,"||",fixed = T)[[1]],trimws,USE.NAMES = FALSE)
		} else {
			group_levels = unique(choose_group_2levels_tmp[,input$choose_group,drop=TRUE])
		}
		choose_group_2levels_tmp = choose_group_2levels_tmp[
			choose_group_2levels_tmp[,input$choose_group,drop=TRUE] %in% group_levels,]
		choose_group_2levels_tmp[,input$choose_group] = 
			factor(choose_group_2levels_tmp[,input$choose_group,drop=TRUE], levels=group_levels)  
		choose_group_2levels_tmp
	})


	output$choose_group_2levels_out = renderPrint({
		summary(group_final()[,input$choose_group])
		# head(group_final()[,-2:-3])
		# str(group_final())

	})





	vis_dim_data = eventReactive(input$plot_bttn,{
		vis_dim_dist(
			ids=ids(),
			data_type=profile(), 
			cancer =input$choose_cancer,
			custom = group_final()[,-2],
			group  = input$choose_group,
			group_levels = levels(group_final()[,3,drop=T]),
			return.data = TRUE,
			opt_pancan = opt_pancan()
		)
	})

	output$vis_dim_data_stat <- renderText({ 
		nids=ncol(vis_dim_data()$exp)-1
		nsps=nrow(vis_dim_data()$exp)
		ngroups=length(unique(vis_dim_data()$meta[,input$choose_group,drop=T]))

		inspect_msg = paste0(nids, " valid ids of ", nsps, " samples (", ngroups, 
	  						 " grouping levels) were queried!")
		inspect_msg

	})

	# output$vis_dim_data_stat <- renderPrint({ 
	# 	head(vis_dim_data()$meta)

	# })

	plot_func <- eventReactive(input$plot_bttn, {

	  p = vis_dim_dist(
		ids=ids(),
		data_type=profile(), 
		cancer =input$choose_cancer,
		custom = group_final()[,-2],
		group  = input$choose_group,
		group_levels = levels(group_final()[,3,drop=T]),
	  	DR_method = input$method,
	  	palette=input$palette,
	  	add_margin=if (input$margin == "NONE") NULL else input$margin,
	  	opt_pancan = opt_pancan()
	  )
	  return(p)
	})


	output$out_exp = renderDataTable({
		vis_dim_data()$exp
	})

	output$download <- downloadHandler(
	  filename = function() {
	    paste0(profile(),"_",input$method,".", input$device)
	  },
	  content = function(file) {
	    p <- plot_func()
	    if (input$device == "pdf") {
	      pdf(file, width = input$width, height = input$height)
	      print(p)
	      dev.off()
	    } else {
	      png(file, width = input$width, height = input$height, res = 600, units = "in")
	      print(p)
	      dev.off()
	    }
	  }
	)


	observeEvent(input$plot_bttn, {
		shinyjs::show(id = "out_tables")
	})

	output$dim_plot <- renderPlot({
	    plot_func()
	})
	output$out_dr = renderDataTable({
		plot_func()$data
	})
	output$download_csv_dr <- downloadHandler(
	  filename = function() {
	    paste0(profile(),"_",input$method,"_DR.csv")
	  },
	  content = function(file) {
	    write.csv(plot_func()$data, file, row.names = FALSE)
	  }
	)
	output$download_csv_exp <- downloadHandler(
	  filename = function() {
	    paste0(profile(),"_",input$method,"_Exp.csv")
	  },
	  content = function(file) {
	    write.csv(vis_dim_data()$exp, file, row.names = FALSE)
	  }
	)

}