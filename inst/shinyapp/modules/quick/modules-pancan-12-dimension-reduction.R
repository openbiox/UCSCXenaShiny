ui.modules_dim_dist = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				3,
				wellPanel(
            		h4("1. Data", align = "center"),
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
		              choiceValues = c("Selection", "Pathway", "File"),
		              choiceNames = c("Selection", "Pathway", "File"),
		              animation = "jelly",
		              inline = TRUE
		            ),

					tabsetPanel(
						id = ns("input_params"),
						type = "hidden",
						tabPanel("Selection",
				        	virtualSelectInput(
							inputId = ns("ids_ways_1"),
							label = NULL,
							choice = NULL,
							width = "100%",
							multiple = TRUE, 
							search = TRUE,
							allowNewOption = TRUE,
							dropboxWidth = "200%")
						),
						tabPanel("Pathway",
							fluidRow(
					            virtualSelectInput(
					              inputId = ns("msigdbr_cat"),
					              label = NULL,
					              choices = msigdbr_types$gs_subcat_label, 
					              selected =  msigdbr_types$gs_subcat_label[1],
					              dropboxWidth = "200%")
							),
							fluidRow(
					            virtualSelectInput(
					              inputId = ns("msigdbr_pw"),
					              label = NULL,
					              choices = NULL, 
					              selected =  NULL,
					              search = TRUE,
					              dropboxWidth = "200%"),
							)
						),
						tabPanel("File",
							fluidRow(
								column(8, fileInput(ns("ids_ways_3"),NULL, accept = ".txt")),
								column(4, downloadButton(ns("dw_L3_x"), "e.g."))
							),
							p("Keys: (1) TXT format; (2) One column without colname")
						)
					),  
			        shinyWidgets::actionBttn(
			          inputId = ns("query_data"),
			          label = "Cache data",
			          style = "gradient",
			          # icon = icon("search"),
			          color = "primary",
			          block = TRUE,
			          size = "sm"
			        ),
					br(),
					verbatimTextOutput(ns("tip_s1"))
				),
				wellPanel(
					tabsetPanel(
						id = ns("group_panel"),
						tabPanel(
							"Preset Group",
							pickerInput(
								ns("choose_cancer"), "Choose cancer(s)",
								choices = sort(tcga_cancer_choices),
								multiple = TRUE,
								selected = "BRCA",
								options = list(`actions-box` = TRUE)
							),
							selectInput(
								ns("choose_group"), "Choose one group",
								choices = colnames(tcga_clinical_fine)[-1],
								selected = "Code"
							)
						),
						tabPanel(
							"Custom Group",
							fluidRow(
								column(
									8,
									fileInput(ns("upload_file"), NULL, accept = ".csv"),
								),
								column(
									4,
									br(),#br(),
									downloadButton(ns("eg_file"), "e.g.")
								),
							),
							p("Keys: (1) CSV format; (2) Two column named `Sample` and `Group`")
						)
					),
					shinyWidgets::actionBttn(
						inputId = ns("check_group"),
						label = "Check group",
						style = "gradient",
						color = "primary",
						block = TRUE,
						size = "sm"
					),
					br(),
					verbatimTextOutput(ns("tip_s2"))
				),
			),
			column(
				3,
				wellPanel(
                	h4("2. Parameters", align = "center"),
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("method"), label = "Select Dimension-Reduction method:",
		              choiceValues = c("PCA", "UMAP", "tSNE"),
		              choiceNames = c("PCA", "UMAP", "tSNE"),
		              inline = TRUE,
		              animation = "jelly"
		            ),
		        	# fluidRow(
		        	# 	column(6,selectInput(ns("margin"), "Marginal plot",c('NONE', "density" ,"boxplot"))),
		        	# 	column(6,selectInput(ns("palette"), "Color setting",c("Set1", "Set2", "Set3", "Paired", "Pastel1", "Pastel2", "Accent", "Dark2")))
		        	# ),
		        	selectInput(ns("margin"), "Marginal plot",c('NONE', "density" ,"boxplot")),
		        	selectInput(ns("palette"), "Color setting",c("Set1", "Set2", "Set3", "Paired", "Pastel1", "Pastel2", "Accent", "Dark2")),
			        shinyWidgets::actionBttn(
			          inputId = ns("plot_bttn"),
			          label = "GO!",
			          style = "gradient",
			          icon = icon("search"),
			          color = "primary",
			          block = TRUE,
			          size = "sm"
			        ),
			        textOutput(ns("vis_dim_data_stat"))
				),
		        wellPanel(
		        	h4("3. Download", align = "center"),
		        	# fluidRow(
		        	# 	column(6,numericInput(inputId = ns("height"), label = "Height", value = 8)),
		        	# 	column(6,numericInput(inputId = ns("height"), label = "Height", value = 8))
		        	# ),
		        	numericInput(inputId = ns("height"), label = "Height", value = 8),
		        	numericInput(inputId = ns("width"), label = "Width", value = 8),
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
			          label = "Download",
			          style = "gradient",
			          color = "primary",
			          block = TRUE,
			          size = "sm"
			        )
			    )
			),
			column(
				6,
				# plotOutput(ns("dim_plot"), height = "600px"),
				fluidRow(
					column(8, offset = 2,
						plotOutput(ns("dim_plot"), height = "600px"))

				),
				# plotOutput(ns("dim_plot"), height = "600px"),
				shinyjs::hidden(
					wellPanel(
						id = ns("out_tables"),

						dataTableOutput(ns("out_dr")),
						fluidRow(
							column(2, downloadButton(ns("download_csv_dr"), "Save plot data(.csv)")),
							column(2, downloadButton(ns("download_csv_exp"), "Save raw data(.csv)"))
							
						)
					)
				),
			    hr(),
			    h5("NOTEs:"),
			    uiOutput(ns("msigdb_note.ui")),
			    p("2. Only the primary tumor samples are considered in Preset Group panel"),
			    p("3. Custom identifiers or grouping information can be uploaded by referring to example data."),
			    tags$br()
			)

		)
	)

}



server.modules_dim_dist = function(input, output, session){
	ns <- session$ns

	## ID
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
	  updateVirtualSelect(
	    "ids_ways_1",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default
	  )
	})


	observeEvent(input$input_ways, {
	  updateTabsetPanel(inputId = "input_params", selected = input$input_ways)
	}) 

	msigdbr_query = reactive({
		category = msigdbr_types$gs_cat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		subcategory = msigdbr_types$gs_subcat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		if(length(category)!=0){
			msigdbr_query = msigdbr(species = "Homo sapiens", 
			                      category = category, 
			                      subcategory = subcategory)
			msigdbr_query
		}
	})
	observe({
		if(!is.null(msigdbr_query())){
			msigdbr_term_stat = as.data.frame(table(msigdbr_query()$gs_name)) %>% 
			  dplyr::rename(term=Var1, size=Freq) %>% 
			  dplyr::arrange(term) %>% 
			  dplyr::mutate(term_size = paste0(term," (", size,")"))
			updateVirtualSelect(
			  "msigdbr_pw",
			  choices = msigdbr_term_stat$term_size,
			  selected = msigdbr_term_stat$term_size[1]
			)
		}
	})
	output$msigdb_note.ui = renderUI({
		# pw_sle = ifelse(is.null(input$msigdbr_pw),"HALLMARK_ADIPOGENESIS",input$msigdbr_pw)
		pw_sle = str_split(input$msigdbr_pw," ")[[1]][1]
		term_link = sprintf("https://www.gsea-msigdb.org/gsea/msigdb/human/%s.html",
                    		ifelse(is.null(pw_sle),"HALLMARK_ADIPOGENESIS",pw_sle))
		msigdb_link = "https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp"

		p("1. For Molecular profile except for miRNA/Protein Expression, select ids in ",
			  a("one pathway", href = term_link)," from ",
			  a("MSigDB database", href = msigdb_link), ":")

	})

	output$dw_L3_x = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			all_ids = tcga_id_option[["Molecular profile"]][[input$profile]]$all
			sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
			write.table(sample_ids, file,
				quote = F, row.names = F, col.names = F)
		}
	)


    ids = eventReactive(input$query_data, {
    	if(input$input_ways=="Selection"){
    		ids = input$ids_ways_1
    	} else if (input$input_ways=="File") {
			file = input$ids_ways_3
			if(is.null(file$datapath)){  # 如果空文件
				ids = NULL
			} else {
				ids = read.table(file$datapath)[,1]
				ids = ids[ids %in% tcga_id_option[["Molecular profile"]][[input$profile]]$all]
				if(length(ids)>1000){
					L3s_x = L3s_x[1:1000]
				} 
			}
    	} else if (input$input_ways=="Pathway") {
    		ids = tcga_id_option[["Molecular profile"]][[input$profile]]$all
			pw_genes = msigdbr_query() %>% 
			  dplyr::filter(gs_name %in% str_split(input$msigdbr_pw," ")[[1]][1]) %>% 
			  dplyr::pull(gene_symbol)
			if(input$profile %in% 
				c("mRNA Expression","DNA Methylation","Mutation status","Copy Number Variation")){
				ids = ids[ids %in% pw_genes]
			} else if(L2_x() %in% c("Transcript Expression")){
				ids = ids[ids %in% tcga_id_referrence[[1]][[5]]$Level3[tcga_id_referrence[[1]][[5]]$Symbol %in% pw_genes]]
			}
    	}
	    unique(ids)
	})


	cache_dat = eventReactive(input$query_data, {
		withProgress(message=paste0("Query ", length(ids()), " molecules one by one"),{
		  data = purrr::map(ids(), function(x) {
		    # x = ids[1]
		    data <- query_pancan_value(x, data_type=profile())
		    data = data[[1]]
		    data <- dplyr::tibble(sample = names(data), y = as.numeric(data))
		    colnames(data)[2] <- x
		    incProgress(1 / length(ids()))
		    data
		  }) %>% purrr::reduce(dplyr::full_join, by = "sample")
	    })
	    data = data[, apply(data, 2, function(m) {!all(is.na(m))})]
	    return(data)
	})

	output$tip_s1 = renderPrint({
		cat(paste0("Tips: ", ncol(cache_dat())-1, " valid identifiers are cached."))
	})




	## Group
	output$eg_file = downloadHandler(
		filename = function(){
			"example_group.csv"
		},
		content = function(file){
			group_info = tcga_clinical_fine %>% 
			  dplyr::filter(Cancer == "BRCA") %>% 
			  dplyr::filter(Code %in% c("TP","NT")) %>% 
			  dplyr::select(Sample, Code)
			colnames(group_info)[2] = "Group"
			write.csv(group_info, file, quote = F, row.names = F)
		}
	)



	group_info = eventReactive(input$check_group,{
		if(input$group_panel=="Preset Group"){
			group_info = tcga_clinical_fine %>% 
			  dplyr::filter(Code %in% c("NT", "TP")) %>%
			  dplyr::mutate(Code = ifelse(Code=="TP","tumor","normal")) %>%
			  dplyr::mutate(Age = ifelse(Age>60, "Old(>60)", "Young(<60)")) %>%
			  dplyr::filter(Cancer == input$choose_cancer)
			# only tumor samples when non-code group
			if(input$choose_group!="Code"){
				group_info = group_info %>%
					dplyr::filter(Code=="tumor")
			}
			group_info = group_info[,c("Sample", input$choose_group)] %>% na.omit()
			colnames(group_info)[2] = "Group"


		} else if (input$group_panel=="Custom Group"){
			if(!is.null(input$upload_file$datapath)){
				group_info = read.csv(input$upload_file$datapath) %>% na.omit()
			} else {
				group_info = data.frame("Sample"=NULL, "Group"=NULL)
			}
		}
		shiny::validate(
			need(try(nrow(group_info)>0), 
				"Error: No group infomation is detected.")
		)
		groups = length(unique(group_info$Group))
		shiny::validate(
			need(try(groups>=2 & groups<=10), 
				"Error: Too few (<2) or much (>10) groups are detected.")
		)
		group_info
	})

	output$tip_s2 = renderPrint({
		# head((group_info()))
		cat(paste0("Tips: ", nrow(group_info())," samples with ", 
			length(unique(group_info()$Group))," groups are detected."))
	})


	plot_func <- eventReactive(input$plot_bttn, {
	  p = vis_dim_dist(
		ids=ids(),
		data_type=profile(), 
		group_info = group_info(),
	  	DR_method = input$method,
	  	palette=input$palette,
	  	add_margin=if (input$margin == "NONE") NULL else input$margin,
	  	opt_pancan = opt_pancan()
	  )
	  return(p)
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
		plot_func()$data[,1:4]
	})
	output$download_csv_dr <- downloadHandler(
	  filename = function() {
	    paste0(profile(),"_",input$method,"_DR.csv")

	  },
	  content = function(file) {
	    write.csv(plot_func()$data[,1:4], file, row.names = FALSE)
	  }
	)
	output$download_csv_exp <- downloadHandler(
	  filename = function() {
	    paste0(profile(),"_",input$method,"_Exp.csv")
	  },
	  content = function(file) {
	    write.csv(plot_func()$data[,-1:-4], file, row.names = FALSE)
	  }
	)

}
