ui.modules_dim_dist = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				3,
				wellPanel(
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("profile"), label = "Select a genomic profile:",
		              choiceValues = c("mRNA", "transcript", "methylation", "protein", "mutation", "miRNA", "cnv_gistic2", "cnv"),
		              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "Protein Expression", "Gene Mutation", "miRNA Expression", "Copy Number Variation", "Copy Number Variation (thresholded)"),
		              animation = "jelly"
		            ),

			        div(style="display: inline-block;vertical-align:middle; width: 250px;",
			        	selectizeInput(
						inputId = ns("ids"),
						label = "Input or Upload the genes (>=3)",
						choice = NULL,
						width = "100%",
						multiple = TRUE, 
						options = list(
							create = TRUE,
							maxOption = 5,
							placeholder = "Enter the ids of selected profile",
							plugins = list("restore_on_backspace")
						)
					)),
			        div(style="display: inline-block;vertical-align:middle; width: 30;",
			        	actionButton(ns("reset_1"), NULL,icon("arrows-rotate"),style='padding:8px;'),),

			        div(style="display: inline-block;vertical-align:top; width: 250px;",
			        	fileInput(ns("upload"), NULL, accept = c(".txt"))),
			        div(style="display: inline-block;vertical-align:top; width: 30;",
			        	actionButton(ns("reset_2"), NULL,icon("arrows-rotate"),style='padding:8px;'),),

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
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("Mode"), label = "Select grouping mode:",
		              choiceValues = c("TumorNomral", "Clinical", "Cancer", "Custom"),
		              choiceNames = c("Tumor and normal", "Clinical phenotype", "Pan-cancer", "Custome grouping"),
		              animation = "jelly"
		            ),
					tabsetPanel(
					  id = ns("group_params"),
					  type = "hidden",
					  tabPanel("TumorNomral",
					  	selectInput(ns("Cancer"), "Filter Cancer",sort(tcga_cancer_choices)),
					  	materialSwitch(ns("tcga_only"), "TCGA Dataset only")
					  ),
					  tabPanel("Clinical", 
					  	selectInput(ns("Cancer"), "Filter Cancer",sort(tcga_cancer_choices)),
					    selectInput(ns("Phe"), "Choose phenotype",
					    	c('age','gender','race','stage_ajcc','stage_clinical','histological_grade'))
					  ),
					  tabPanel("Cancer",
					  	selectInput(ns("Cancer"), "Filter Cancers(>=2)",sort(tcga_cancer_choices),multiple = TRUE)
					  ),
					  tabPanel("Custom", 
					  	selectInput(ns("Cancer"), "Filter Cancer",sort(tcga_cancer_choices)),
					    fileInput(ns("custom_meta"), NULL)
					  )
					),  
					shinyWidgets::actionBttn(
						inputId = ns("inspect_data"),
						label = "STEP2: Inspect input data",
						style = "gradient",
						color = "default",
						block = TRUE,
						size = "sm"
					),
					textOutput(ns("inspect_msg"))

				),


				wellPanel(
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("method"), label = "Select Dimension-Reduction method:",
		              choiceValues = c("PCA", "UMAP", "tSNE"),
		              choiceNames = c("PCA", "UMAP", "tSNE"),
		              animation = "jelly"
		            ),
		            selectInput(ns("margin"), "Marginal plot",c('NONE', "density" ,"boxplot")),
		            selectInput(ns("palette"), "Color setting",c("Set1", "Set2", "Set3", "Paired", "Pastel1", "Pastel2", "Accent", "Dark2")),
			        shinyWidgets::actionBttn(
			          inputId = ns("plot_bttn"),
			          label = "STEP3: 2D data visualization",
			          style = "gradient",
			          # icon = icon("search"),
			          color = "default",
			          block = TRUE,
			          size = "sm"
			        )

				),
		        wellPanel(
			        numericInput(inputId = ns("height"), label = "Height", value = 5),
			        numericInput(inputId = ns("width"), label = "Width", value = 12),
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
			    hr(),
			    h5("NOTEs:"),
			    p("1. When you want to upload the prepared gene set, one column .txt format with \\n as seperator without column name is expected"),
			    p("----Too many genes (>100) may take long time to query and please be patient."),
			    p("2. Under the Cinical mode, age grouping are determined by the median value. For other phenotypes (such as stage_ajcc), the primary level were substracted."),
			    p("3. Under the Custom mode, you shoud upload a two-column dataframe (.txt format with \\t as seperator)" ),
			    p("----First column ('sample') is TCGA sample id (15 characters) and Second column ('group') is custom grouping information;"),
			    p("----Color mapping is consistent with unique(custom_meta$group)."),
			    p("4. ", tags$a(href = "https://pancanatlas.xenahubs.net/", "Genomic profile data source")),
			    tags$br(),
				DT::DTOutput(outputId = ns("data_report")),
		        shinyjs::hidden(
		          wellPanel(
		            id = ns("save_csv"),
		            downloadButton(ns("downloadTable"), "Save as csv")
		          )
		        )
			)

		)

	)

}



server.modules_dim_dist = function(input, output, session){
	ns <- session$ns

	profile_choices <- reactive({
	  switch(input$profile,
	    mRNA = list(all = pancan_identifiers$gene, default = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A")),
	    methylation = list(all = pancan_identifiers$gene),
	    protein = list(all = pancan_identifiers$protein),
	    transcript = list(all = load_data("transcript_identifier")),
	    mutation = list(all = pancan_identifiers$gene),
	    miRNA = list(all = pancan_identifiers$miRNA),
	    cnv_gistic2 = list(all = pancan_identifiers$gene),
	    cnv = list(all = pancan_identifiers$gene),
	    list(all = "NONE", default = "NONE")
	  )
	})

	observe({
	  updateSelectizeInput(
	    session,
	    "ids",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default,
	    server = TRUE
	  )
	})

    observeEvent(input$reset_1, {
        reset("ids")
    })
    observeEvent(input$reset_2, {
        reset("upload")
    })

	ids = reactive({
	  if(!is.null(input$ids)){
	  	ids = input$ids
	  } else {
	  	ids = read.table(input$upload$datapath, sep = "\n")[,1]
	  }
	  ids
    })


	observeEvent(input$query_data,{
		withProgress(message="Query genes one by one",{
		  purrr::map(ids(), function(x) {
		    # x = ids[1]
		    data <- query_pancan_value(x, data_type=input$profile)
		    data = data[[1]]
		    data <- dplyr::tibble(sample = names(data), y = as.numeric(data))
		    colnames(data)[2] <- x
		    incProgress(1 / length(ids))
		    data
		  }) %>% purrr::reduce(dplyr::full_join, by = "sample")
	
		}) 
	})


	observeEvent(input$Mode, {
	  updateTabsetPanel(inputId = "group_params", selected = input$Mode)
	}) 

	inspect_msg = eventReactive(input$inspect_data,{
			data.list = vis_dim_dist(
				ids=ids(),
				data_type=input$profile, 
				cancer_type=input$Cancer,
				Mode = input$Mode,
				group_name = input$Phe,
				custom_meta = read.table(input$custom_meta$datapath, sep = "\t", header = T),
				TCGA.only = input$tcga_only,
				return.data = TRUE
			)
			nids=ncol(data.list$exp)-2
			nsps=nrow(data.list$exp)
			ngroups=length(unique(data.list$meta$group))

			inspect_msg = paste0(nids, " valid ids of ", nsps, " samples (", ngroups, 
		  						 " grouping levels) were queried!")
			inspect_msg

	})

	output$inspect_msg <- renderText({ 
	  inspect_msg()
	})

	plot_func <- eventReactive(input$plot_bttn, {

	  p = vis_dim_dist(
	  	ids=ids(),
	  	data_type=input$profile, 
	  	cancer_type=input$Cancer,
	  	Mode = input$Mode,
	  	group_name = input$Phe,
	  	custom_meta = read.table(input$custom_meta$datapath, sep = "\t", header = T),
	  	TCGA.only = input$tcga_only,
	  	DR_method = input$method,
	  	palette=input$palette,
	  	add_margin=if (input$margin == "NONE") NULL else input$margin
	  )
	  return(p)
	})

	output$dim_plot <- renderPlot({
	    plot_func()
	})
	output$data_report = renderDT({
		plot_data = plot_func()$data
		plot_data = cbind(plot_data[,c("sample","group")],plot_data[,1:2])
		plot_data
	})

	output$download <- downloadHandler(
	  filename = function() {
	    paste0(input$profile,"_", input$Mode,"_",input$method,".", input$device)
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
		shinyjs::show(id = "save_csv")
	})
	output$downloadTable <- downloadHandler(
	  filename = function() {
	    paste0(input$profile,"_", 
	    		input$Mode,"_",input$method,".csv")
	  },
	  content = function(file) {
	    write.csv(plot_func()$data, file, row.names = FALSE)
	  }
	)



}