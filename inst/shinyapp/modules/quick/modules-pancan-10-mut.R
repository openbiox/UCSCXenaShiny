ui.modules_pancan_mut = function(id){
	ns = NS(id)
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
	            ), br(),
	            virtualSelectInput(
	              inputId = ns("mut_Gene"),
	              label = "Grouping by gene mutation",
	              choices = NULL,
	              width = "100%",
	              search = TRUE,
	              allowNewOption = TRUE,
	              dropboxWidth = "200%"
	            ), br(),
	            shinyWidgets::prettyRadioButtons(
	              inputId = ns("profile"), label = "Select affected genomic profile:",
	              choiceValues = c("mRNA", "transcript", "methylation", "miRNA"),
	              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "miRNA Expression"),
	              animation = "jelly"
	            ),
	            virtualSelectInput(
	              inputId = ns("Pancan_search"),
	              label = "Input affected gene or formula (as signature)",
	              choices = NULL,
	              width = "100%",
	              search = TRUE,
	              allowNewOption = TRUE,
	              dropboxWidth = "200%"
	        	),
	            shinyWidgets::prettyRadioButtons(
	              inputId = ns("Mode"), label = "Select analysis cancer(s):",
	              choiceValues = c("Pan-cancer", "Single-cancer"),
	              choiceNames = c("Pan-cancer", "Single-cancer"),
	              animation = "jelly"
	            ),
				tabsetPanel(
				  id = ns("Mode_params"),
				  type = "hidden",
				  tabPanel("Single-cancer",
				  	selectInput(ns("Cancer"), "Select one cancer",sort(tcga_cancer_choices))
				  ),
				  tabPanel("Pan-cancer")
				),
		        tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
		        shinyWidgets::actionBttn(
		          inputId = ns("check_bttn"),
		          label = "Check",
		          style = "gradient",
		          # icon = icon("search"),
		          color = "primary",
		          block = TRUE,
		          size = "sm"
		        ),
		        br(),
		        verbatimTextOutput(ns("mut_tip"))
		        # numericInput(inputId = ns("size_cutoff"), label = "Minimum group size", value = 3),
			),
		),
		column(
			3,
			wellPanel(
                h4("2. Parameters", align = "center"),
		        materialSwitch(ns("pdist_mode"), "Show violin plot", inline = FALSE),
		        materialSwitch(ns("pdist_show_p_value"), "Show P value", inline = TRUE),
		        materialSwitch(ns("pdist_show_p_label"), "Show P label", inline = TRUE),
		        colourpicker::colourInput(inputId = ns("mut_col"), "Mutated sample color", "#DF2020"),
		        colourpicker::colourInput(inputId = ns("wild_col"), "Wild sample color", "#DDDF21"),
		        selectInput(inputId = ns("theme"), label = "Select theme for plot", choices = names(themes_list), selected = "cowplot"),
		        tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
		        shinyWidgets::actionBttn(
		          inputId = ns("plot_bttn"),
		          label = "Go!",
		          style = "gradient",
		          icon = icon("search"),
		          color = "primary",
		          block = TRUE,
		          size = "sm"
		        )
			),
			wellPanel(
                h4("3. Download", align = "center"),
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
		        tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
		        downloadBttn(
		          outputId = ns("download"),
		          style = "gradient",
		          color = "primary",
		          block = TRUE,
		          size = "sm"
		        )
			)
		),
		column(
		    6,
		    plotOutput(ns("mut_plot"), height = "600px"),
		    hr(),
		    # h5("NOTEs:"),
		    # p("1. 500 common patwhay genesets from 3 resources(50 HALLMARK, 186 KEGG, 264 IOBR) were collected."),
		    # p("2. Pathway scores of TCGA(toil) tumor patients were calculated using ssGSEA method."),
		    DT::DTOutput(ns("mut_data")),
	        shinyjs::hidden(
	          wellPanel(
	            id = ns("save_csv"),
	            downloadButton(ns("downloadTable"), "Save as csv")))


		)
	)

}

server.modules_pancan_mut = function(input, output, session){
	ns = session$ns

	observe({
	  updateVirtualSelect(
	    "mut_Gene",
	    choices = pancan_identifiers$gene,
	    selected = "TP53"
	  )
	})

	observeEvent(input$Mode, {
	  updateTabsetPanel(inputId = "Mode_params", selected = input$Mode)
	})

	profile_choices <- reactive({
	  switch(input$profile,
	    mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
	    methylation = list(all = pancan_identifiers$gene, default = "TP53"),
	    transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"),
	    miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
	    list(all = "NONE", default = "NONE")
	  )
	})

	observe({
	  updateVirtualSelect(
	    "Pancan_search",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default
	  )
	})

	opt_pancan = callModule(mol_origin_Server, "mol_origin2quick", database = "toil")


	colors <- reactive({
	  c(input$mut_col, input$wild_col)
	})

	plot_theme <- reactive({
	  themes_list[[input$theme]]
	})

	mut_tip = eventReactive(input$check_bttn,{
		mut_dat_raw <- query_pancan_value(input$mut_Gene, data_type = "mutation")
		# print(str(mut_dat_raw))
		shiny::validate(
			need(try(!all(is.na(mut_dat_raw))), 
				"No mutate information for the gene."),
		)
		tcga_gtex <- load_data("tcga_gtex")
		mut_dat <- mut_dat_raw %>%
		  as.data.frame() %>%
		  tibble::rownames_to_column("Sample") %>%
		  dplyr::rename("mut" = ".") %>% 
		  dplyr::inner_join(., tcga_gtex, by = c("Sample"="sample")) %>% 
		  dplyr::filter(.data$type2 == "tumor") %>%
		  dplyr::select("Sample", "tissue", "mut")

		exp_dat_raw <- query_pancan_value(input$Pancan_search, data_type = input$profile, opt_pancan = opt_pancan())
		exp_dat <- exp_dat_raw$expression %>%
		  as.data.frame() %>%
		  tibble::rownames_to_column("Sample") %>%
		  dplyr::rename("values" = ".") %>%
		  dplyr::inner_join(., tcga_gtex, by = c("Sample"="sample")) %>%
		  dplyr::select("Sample", "values")
		merge_dat <- dplyr::inner_join(mut_dat, exp_dat) %>%
		  dplyr::mutate(
		    tissue = as.character(.data$tissue),
		    mut = factor(.data$mut, levels = c(1, 0), labels  = c("Mutation", "Wild"))
		  )
		if(input$Mode=="Single-cancer"){
			merge_dat_sub = subset(merge_dat, tissue==input$Cancer)
			subtest = subset(merge_dat_sub, mut=="Mutation")
			if(nrow(subtest)<=3){
				mut_tips = paste0("Warning: Less than 3 valid samples in Mutation group.")
			} else {
				mut_tips = paste0("Note: ", nrow(subtest)," valid samples in Mutation group.")
			}
		} else {
			subtest = subset(merge_dat, mut=="Mutation")
			subtest_stat = names(table(subtest$tissue)[table(subtest$tissue)>3])
			mut_tips = paste0("Note: ",length(subtest_stat)," cancers with Mutation group above 3.")
		}
		mut_tips
	})
	output$mut_tip = renderPrint({
		cat(mut_tip())
	})




	w <- waiter::Waiter$new(id = ns("mut_plot"), html = waiter::spin_hexdots(), color = "white")

	plot_func = eventReactive(input$plot_bttn, {
		p = switch(input$Mode,
			`Pan-cancer`=vis_toil_Mut(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, Mode =ifelse(input$pdist_mode,"Violinplot", "Boxplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), opt_pancan = opt_pancan()
				),
			`Single-cancer`=vis_toil_Mut_cancer(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, Mode =ifelse(input$pdist_mode,"Violinplot", "Dotplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), Cancer=input$Cancer, opt_pancan = opt_pancan()
				)
		)
	  return(p)
	})


	output$mut_plot <- renderPlot({
	  w$show() # Waiter add-ins
	  plot_func()
	})
	output$mut_data <- renderDT({
	  plot_func()$data %>%
	  	dplyr::rename('Cancer'='tissue', 'Sample'='sample',
	  		'Group'='mut', 'Expression'='expression') %>%
		dplyr::arrange(Sample)
	})
	output$download <- downloadHandler(
	  filename = function() {
	    paste0(input$Pancan_search,"_", input$profile,"_",input$mut_Gene,"_mutation.", input$device)
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
	    paste0(input$Pancan_search,"_", input$profile,"_",input$mut_Gene,"_mutation.csv")
	  },
	  content = function(file) {
	  	data = plot_func()$data %>%
		  	dplyr::rename('Cancer'='tissue', 'Sample'='sample',
		  		'Group'='mut', 'Expression'='expression') %>%
		  	dplyr::arrange(Sample)
	    write.csv(data, file, row.names = FALSE)
	  }
	)

}

