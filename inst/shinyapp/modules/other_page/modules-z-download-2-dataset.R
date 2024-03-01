ui.modules_download_dataset = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				4,
				wellPanel(
					style = "height:1000px",
					h3("1. Select one data hub"),
					selectInput(ns("one_datahub"),NULL,
						choices = sort(unique(XenaData$XenaHostNames)),
						selected = "toilHub"),
					uiOutput(ns("hub_link")),
					# br(),
					h3("2. Select one dataset"),
					fluidRow(
						column(
							6,
							h4("(1) Format type"),
							selectInput(ns("format_type"), NULL,
								choices = NULL, selected = NULL)
						),
						column(
							6,
							h4("(2) Profile type"),
							selectInput(ns("profile_type"), NULL,
								choices = NULL, selected = NULL)
						),
					),
					virtualSelectInput(ns("one_dataset"), NULL,choices=NULL,selected=NULL,
					                   search = TRUE,
					                   allowNewOption = FALSE,
					                   dropboxWidth = "200%"),
					uiOutput(ns("dataset_link")),
					# br(),
					h3("3. Select multiple ids"),

					# h4("(1) Load ids"),
					fluidRow(
						column(
							4,
							actionButton(ns("load_id"), "Load", icon = icon("circle-down"),
								style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
						),
						column(
							8,
							radioGroupButtons(
							   inputId = ns("id_type"),
							   label = NULL,
							   choices = "NULL",
							   justified = TRUE
							)
						)
					),

					tabsetPanel(
						id = ns("upload_type"),
						tabPanel("Selection",
							uiOutput(ns("multi_ids.ui")) %>% withSpinner(),
							# virtualSelectInput(
							# 	inputId = ns("multi_ids"),
					  #           label = "Identifier:",
					  #           choices = NULL,
					  #           multiple = TRUE,
					  #           search = TRUE,
					  #           allowNewOption = FALSE,
					  #           dropboxWidth = "200%")
						),
						tabPanel("File",
							fluidRow(
								column(9, fileInput(ns("fl_ids"),NULL, accept = ".txt",
									placeholder = "Please upload an id list(.txt)")),
								column(3, downloadButton(ns("fl_eg"), "e.g."))
							),
						),
					),

					verbatimTextOutput(ns("query_tip1")),
					shinyWidgets::actionBttn(
						ns("inspect_data_x"), "Query",
				        style = "gradient",
				        icon = icon("search"),
				        color = "primary",
				        block = TRUE,
				        size = "sm"
					),
					br(),
					verbatimTextOutput(ns("query_tip2")),
					h3("3. Download results"),
				    fluidRow(
				    	column(3, downloadButton(ns("save_csv"), "Download table(.csv)")),
				    	column(3, offset = 2, downloadButton(ns("save_rda"), "Download table(.rda)"))
				    ),
				)
			),
			column(
				5,
				offset = 1,
				br(),br(),br(),br(),
				dataTableOutput(ns("query_data_df")),
				br(),br(),
				strong(h3("NOTEs:")),
				h5("1. To get the whole dataset, please click 'Respository' page and download derictly from UCSC website."),
				h5("2. Queried data in long format is for easy display and it is downloaded as the wide format. "),
			)

		)

	)

}



server.modules_download_dataset = function(input, output, session){
	ns = session$ns


	output$hub_link = renderUI({
		link = unique(XenaData$XenaHosts[XenaData$XenaHostNames==input$one_datahub])
		p("See more info about the hub from Respository page or",
			a("Xena site.",href = link))

	})
	output$dataset_link = renderUI({
		hub_link = unique(XenaData$XenaHosts[XenaData$XenaHostNames==input$one_datahub])
		set_link = paste0("https://xenabrowser.net/datapages/?dataset=",
						  input$one_dataset, "&host=", hub_link)
		p("See more info about the dataset from Respository page or",
			a("Xena site.",href = set_link))

	})

	observe({
		format_types = sort(unique(XenaData$Type[XenaData$XenaHostNames==input$one_datahub]))
	    updateSelectInput(
	      session,
	      "format_type",
	      choices = format_types,
	      selected = "genomicMatrix"
	    )
	})

	observe({
	    profile_types = sort(unique(XenaData$DataSubtype[
	    							XenaData$XenaHostNames==input$one_datahub & 
	    							XenaData$Type==input$format_type]))
	    updateSelectInput(
	      session,
	      "profile_type",
	      choices = profile_types,
	      selected = profile_types[1]
	    )
	})

	observe({
	    datasets = sort(unique(XenaData$XenaDatasets[
	    							XenaData$XenaHostNames==input$one_datahub & 
	    							XenaData$Type==input$format_type &
	    							XenaData$DataSubtype==input$profile_type]))
	    updateVirtualSelect(
	      "one_dataset",
	      choices = datasets,
	      selected = datasets[1]
	    )
	})


	observe({
		hub_link = unique(XenaData$XenaHosts[XenaData$XenaHostNames==input$one_datahub])
		url <- has_probeMap(host = hub_link,dataset = input$one_dataset)
		if(length(url)!=0){
			if(url){
				choices = c("Primary ID", "ProbeMap ID")
			} else {
				choices = c("Primary ID")
			}
			updateRadioGroupButtons(
				session,
				"id_type",
				choices = choices,
				selected = "Primary ID"
			)
		}

	})

	candi_ids = eventReactive(input$load_id,{
		hub_link = unique(XenaData$XenaHosts[XenaData$XenaHostNames==input$one_datahub])
		url <- has_probeMap(host = hub_link,dataset = input$one_dataset, return_url = T)
		if(input$id_type=="Primary ID"){
			candi_ids = fetch_dataset_identifiers(host = hub_link,dataset = input$one_dataset)
		} else {
			candi_ids <- UCSCXenaTools:::use_cache(url, op = "readr::read_tsv(url,\n            col_types = readr::cols()\n        )[[2]]")
		}
		candi_ids
	})


	output$multi_ids.ui = renderUI({
		input$load_id
		Sys.sleep(1.5)
		virtualSelectInput(
			inputId = ns("multi_ids"),
            label = "Identifier:",
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            allowNewOption = FALSE,
            dropboxWidth = "200%")

	})


	observe({
	    updateVirtualSelect(
	      "multi_ids",
	      choices = sort(candi_ids()),
	      selected = sort(candi_ids())[1]
	    )	
	})

	output$fl_eg = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			sample_ids = sample(candi_ids(),ifelse(length(candi_ids())>10,10,length(candi_ids())))
			write.table(sample_ids, file,
				quote = F, row.names = F, col.names = F)
		}
	)


	query_ids = reactive({
		shiny::validate(
			need(try(length(candi_ids())>0), 
				"Please load ids firstly!")
		)
		if(input$upload_type=="Selection"){
			query_ids = unique(input$multi_ids)
		} else {
			file = input$fl_ids
			if(is.null(file$datapath)){  # 如果空文件
				query_ids = NULL
			} else {
				query_ids = read.table(file$datapath)[,1]
				query_ids = query_ids[query_ids %in% candi_ids()]
				if(length(query_ids)>500){
					query_ids = query_ids[1:500]
				}
			}
		}
		query_ids
		
	})


	output$query_tip1 = renderPrint({
		ids_num = length(query_ids())
		cat(paste0("Tip: ", ids_num, " unique ids are selected."))
	})
	query_data = eventReactive(input$inspect_data_x, {
		withProgress(message = "Please wait for a while.",{
			data = lapply(seq(query_ids()), function(i){
				id = query_ids()[i]
				data_tmp = get_data(dataset = input$one_dataset,
				                    identifier = id,
				                    host = input$one_datahub)
				# 进度提醒
			    incProgress(1 / length(query_ids()), detail = paste0("(Run ",i,"/",length(query_ids()),")"))

				return(data_tmp)
			}) %>% do.call(cbind, .) %>% as.data.frame()
			colnames(data) = query_ids()
			data = data[,apply(data, 2, function(x){!all(is.na(x))}),drop=FALSE]
			data = data %>%
				tibble::rownames_to_column("Sample")
			data
		})
	})

	observeEvent(input$inspect_data_x,{
		shiny::validate(
			need(try(nrow(query_data())>0), 
				"No sample data were available. Please inspect operations in Preset step."),
		)
		output$query_tip2 = renderPrint({
			ids_num = ncol(query_data())-1
			cat(paste0("Tip: ", ids_num, " ids are queried successfully!"))
		})
		verbatimTextOutput(ns("query_tip2"))

		query_data_ = query_data() %>%
						reshape2::melt("Sample") %>% 
						dplyr::rename("id"="variable")

		output$query_data_df = renderDataTable({
			datatable(query_data_[,c("id","Sample","value")],
				# class = "nowrap row-border",
				options = list(pageLength = 10, 
					columnDefs = list(list(className = 'dt-center', targets="_all")))
			)
		})
	})

	output$save_csv = downloadHandler(
		filename = function(){
			paste0("Batch_query_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			write.csv(query_data(), file, row.names = FALSE)
		}
	)
	output$save_rda = downloadHandler(
		filename = function(){
			paste0("Batch_query_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rda")
		},
		content = function(file){
			query_data = query_data()
			save(query_data, file = file)
		}
	)

}
