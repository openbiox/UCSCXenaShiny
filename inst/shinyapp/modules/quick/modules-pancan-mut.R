ui.modules_pancan_mut = function(id){
	ns = NS(id)
	fluidRow(
		column(
			3,
			wellPanel(
	            selectizeInput(
	              inputId = ns("mut_Gene"),
	              label = "Input a gene with mutation ralated grouping",
	              choices = NULL,
	              width = "100%",
	              options = list(
	              	create = TRUE,
	              	maxOptions = 5,
	              	placeholder = "Enter a gene symbol, e.g. TP53",
	              	plugins = list("restore_on_backspace")
     		      )
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
		        numericInput(inputId = ns("size_cutoff"), label = "Minimum group size", value = 3),
			),
			wellPanel(
	            shinyWidgets::prettyRadioButtons(
	              inputId = ns("profile"), label = "Select affected genomic profile:",
	              choiceValues = c("mRNA", "transcript", "methylation", "miRNA"),
	              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "miRNA Expression"),
	              animation = "jelly"
	            ),
		        actionButton(ns("toggleBtn"), "Modify datasets[opt]",icon = icon("folder-open")),
		        conditionalPanel(
		          ns = ns,
		          condition = "input.toggleBtn % 2 == 1",
		          mol_origin_UI(ns("mol_origin2quick"))
		        ),
	            selectizeInput(
	              inputId = ns("Pancan_search"),
	              label = "Input affected gene or formula (as signature)",
	              choices = NULL,
	              width = "100%",
	              options = list(
	                create = TRUE,
	                maxOptions = 5,
	                placeholder = "Enter a gene symbol, e.g. TP53",
	                plugins = list("restore_on_backspace")
	              )
	        	),
			),
			wellPanel(
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
		          style = "gradient",
		          color = "default",
		          block = TRUE,
		          size = "sm"
		        )
			)
		),
		column(
		    9,
		    plotOutput(ns("mut_plot"), height = "600px",width = "600px"),
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
	  updateSelectizeInput(
	    session,
	    "mut_Gene",
	    choices = pancan_identifiers$gene,
	    selected = "TP53",
	    server = TRUE
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
	  updateSelectizeInput(
	    session,
	    "Pancan_search",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default,
	    server = TRUE
	  )
	})

	opt_pancan = callModule(mol_origin_Server, "mol_origin2quick")


	colors <- reactive({
	  c(input$mut_col, input$wild_col)
	})

	plot_theme <- reactive({
	  themes_list[[input$theme]]
	})

	w <- waiter::Waiter$new(id = ns("mut_plot"), html = waiter::spin_hexdots(), color = "white")

	plot_func = eventReactive(input$plot_bttn, {
		p = switch(input$Mode,
			`Pan-cancer`=vis_toil_Mut(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, size_cutoff=input$size_cutoff, Mode =ifelse(input$pdist_mode,"Violinplot", "Boxplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), opt_pancan = opt_pancan()
				),
			`Single-cancer`=vis_toil_Mut_cancer(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, size_cutoff=input$size_cutoff, Mode =ifelse(input$pdist_mode,"Violinplot", "Dotplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), Cancer=input$Cancer, opt_pancan = opt_pancan()
				)
		)
	  return(p)
	})

	data_func = eventReactive(input$plot_bttn, {
		p_dat = switch(input$Mode,
			`Pan-cancer`=vis_toil_Mut(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, size_cutoff=input$size_cutoff, Mode =ifelse(input$pdist_mode,"Violinplot", "Boxplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), plot = FALSE, opt_pancan = opt_pancan()
				),
			`Single-cancer`=vis_toil_Mut_cancer(mut_Gene = input$mut_Gene, Gene = input$Pancan_search,
				data_type=input$profile, size_cutoff=input$size_cutoff, Mode =ifelse(input$pdist_mode,"Violinplot", "Boxplot"),
				Show.P.value = input$pdist_show_p_value, Show.P.label = input$pdist_show_p_label,
				values = colors(), Cancer=input$Cancer, plot = FALSE, opt_pancan = opt_pancan()
				)
		)
	  return(p_dat)
	})

	output$mut_plot <- renderPlot({
	  w$show() # Waiter add-ins
	  plot_func()
	})
	output$mut_data <- renderDT({
	  data_func()
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
	    write.csv(data_func(), file, row.names = FALSE)
	  }
	)

}

