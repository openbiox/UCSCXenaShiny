ui.modules_pw_cor = function(id){
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
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("profile"), label = "Select a genomic profile:",
		              choiceValues = c("mRNA", "transcript", "methylation", "protein", "miRNA", "cnv"),
		              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "Protein Expression", "miRNA Expression", "Copy Number Variation"),
		              animation = "jelly"
		            ),
		            virtualSelectInput(
		              inputId = ns("Pancan_search"),
		              label = "Input a gene or formula (as signature)",
		              choices = NULL,
		              width = "100%",
		              search = TRUE,
		              allowNewOption = TRUE,
		              dropboxWidth = "200%"
		          	),
		          	virtualSelectInput(
		          		inputId = ns("pw_name"), 
		          		label = "Select one pathway",
		          		choices = sort(PW_meta$ID),
		          		selected = "HALLMARK_ADIPOGENESIS", 
		                width = "100%",
		          		search = TRUE,
		          		dropboxWidth = "200%"
		          	),
		        ),
			),
			column(
				3,
		        wellPanel(
          			h4("2. Parameters", align = "center"),
			        selectInput(inputId = ns("use_all"), label = "Use All Cancer Types", choices = c("TRUE", "FALSE"), selected = "FALSE"),
			        selectInput(
			          inputId = ns("Cancer"), label = "Filter Cancer",
			          choices = tcga_cancer_choices,
			          selected = "ACC", multiple = TRUE
			        ),
			        materialSwitch(ns("use_regline"), "Use regression line", inline = TRUE),
			        selectInput(
			          inputId = ns("cor_method"),
			          label = "Select Correlation method",
			          choices = c("spearman", "pearson"),
			          selected = "spearman"
			        ),
			        sliderTextInput(
			          inputId = ns("alpha"),
			          label = "Choose a transparent value",
			          choices = seq(
			            from = 0,
			            to = 1,
			            by = 0.1
			          ),
			          selected = "0.5",
			          grid = TRUE
			        ),
			        colourpicker::colourInput(inputId = ns("color"), "Point color", "#000000"),
			        tags$hr(style = "border:none; border-top:2px solid #5E81AC;"),
			        shinyWidgets::actionBttn(
			          inputId = ns("search_bttn"),
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
			        numericInput(inputId = ns("height"), label = "Height", value = 6),
			        numericInput(inputId = ns("width"), label = "Width", value = 6),
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
			    # fluidRow(
			    # 	column(6, offset = 3,
			    # 		plotOutput(ns("pw_plot"), height = "600px",width = "600px"))
			    # ),
			    plotOutput(ns("pw_plot"), height = "600px"),
			    hr(),
			    h5("NOTEs:"),
			    p("1. 500 common pathway genesets from 3 resources(50 HALLMARK, 186 KEGG, 264 IOBR) were collected."),
			    p("2. Pathway scores of TCGA(toil) tumor patients were calculated using ssGSEA method."),
			    DT::DTOutput(ns("tbl")),
		        shinyjs::hidden(
		          wellPanel(
		            id = ns("save_csv"),
		            downloadButton(ns("downloadTable"), "Save as csv")
		          )
		        )
			),

		)
	)
}

server.modules_pw_cor = function(input, output, session){
	ns <- session$ns
	profile_choices <- reactive({
	  switch(input$profile,
	    mRNA = list(all = pancan_identifiers$gene, default = "TP53"),
	    methylation = list(all = pancan_identifiers$gene, default = "TP53"),
	    protein = list(all = pancan_identifiers$protein, default = "P53"),
	    transcript = list(all = load_data("transcript_identifier"), default = "ENST00000000233"),
	    miRNA = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
	    cnv = list(all = pancan_identifiers$gene, default = "TP53"),
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


	observeEvent(input$Mode, {
	  updateTabsetPanel(inputId = "mode_params", selected = input$Mode)
	}) 

    observeEvent(input$reset, {
        reset("mode_params")
    })

	# Show waiter for plot
	w <- waiter::Waiter$new(id = ns("pw_plot"), html = waiter::spin_hexdots(), color = "white")


	plot_func <- eventReactive(input$search_bttn, {
	  if (nchar(input$Pancan_search)>=1) {
	    p <- vis_gene_pw_cor(
	      Gene = input$Pancan_search,
	      data_type = input$profile,
          pw_name = input$pw_name,
	      cancer_choose = input$Cancer,
	      cor_method = input$cor_method,
	      use_regline = input$use_regline,
	      color = input$color,
	      alpha = input$alpha,
	      use_all = as.logical(input$use_all),
	      opt_pancan = opt_pancan()
	    )
	  }
	  p <- p + theme_classic(base_size = 20) +
	    ggplot2::theme(legend.position = "none")

	  return(p)
	})

	## downloadTable
	output$downloadTable <- downloadHandler(
	  filename = function() {
	    paste0(input$Pancan_search1, "_", input$profile1, "_", input$Pancan_search2, "_", input$profile2, "_pancan_gene_cor.csv")
	  },
	  content = function(file) {
	    write.csv(plot_func()$data, file, row.names = FALSE)
	  }
	)

	output$pw_plot <- renderPlot({
	  w$show() # Waiter add-ins
	  plot_func()
	})


	# download module
	output$download <- downloadHandler(
	  filename = function() {
	    paste0(input$Pancan_search, "_", input$profile, "_", input$pw_name , "_cor.", input$device)
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


	## return data
	observeEvent(input$search_bttn, {
	  if (nchar(input$Pancan_search) >= 1) {
	    shinyjs::show(id = "save_csv")
	  } else {
	    shinyjs::hide(id = "save_csv")
	  }
	})


	output$tbl <- renderDT(
	  plot_func()$data %>%
	  	dplyr::select(Cancer, Sample, identifier, values, pw_name, pw_score) %>%
	  	dplyr::rename("Molecule"="identifier", "Expression"="values",
	  				  "Pathway"="pw_name", "ssGSEA"="pw_score"),
	  options = list(lengthChange = FALSE)
	)

}
