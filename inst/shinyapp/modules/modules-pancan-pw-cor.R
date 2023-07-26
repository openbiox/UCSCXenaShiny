ui.modules_pw_cor = function(id){
	ns = NS(id)
	fluidPage(
		fluidRow(
			column(
				3,
				wellPanel(
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("profile"), label = "Select a genomic profile:",
		              choiceValues = c("mRNA", "transcript", "methylation", "protein", "miRNA", "cnv_gistic2"),
		              choiceNames = c("mRNA Expression", "Transcript Expression", "DNA Methylation", "Protein Expression", "miRNA Expression", "Copy Number Variation"),
		              animation = "jelly"
		            ),
		            selectizeInput(
		              inputId = ns("Pancan_search"),
		              label = "Input a gene or formula (as signature)",
		              choices = NULL,
		              width = "100%",
		              options = list(
		                create = TRUE,
		                maxOptions = 5,
		                placeholder = "Enter a gene symbol, e.g. TP53",
		                plugins = list("restore_on_backspace")
		              )
		          	)
		        ),
		        wellPanel(
		            shinyWidgets::prettyRadioButtons(
		              inputId = ns("Mode"), label = "Select analysis mode:",
		              choiceValues = c("one_many", "one_one", "many_one"),
		              choiceNames = c("One caner--Many pathways", "One cancer--One pathway", "Many cancers--One pathway"),
		              animation = "jelly"
		            ),
					tabsetPanel(
					  id = ns("mode_params"),
					  type = "hidden",
					  tabPanel("one_many", 
					  	selectInput(ns("Cancer1"), "(1) Select one cancer",sort(tcga_cancer_choices)),
					  	selectInput(ns("cor_method1"), "(2) Select correlation method",c('pearson','spearman')),
					    numericInput(inputId = ns("cor_coef1"), label = "(3) Set absolute coefficient cutoff", value = 0.3, min=0, max=1),
				        numericInput(inputId = ns("cor_pval1"), label = "(4) Set P value cutoff", value = 0.01, min=0, max=1),
					  ),
					  tabPanel("one_one",
					  	selectInput(ns("Cancer2"), "(1) Select one cancer",sort(tcga_cancer_choices)),
					  	selectInput(ns("pw_name2"), "(2) Select one pathway",PW_signatures),
					  	selectInput(ns("cor_method2"), "(3) Select correlation method",c('pearson','spearman')),
					  ),
					  tabPanel("many_one",
					  	selectInput(ns("Cancer3"), "(1) Select multiple cancers",c("Overall",sort(tcga_cancer_choices))),
					  	selectInput(ns("pw_name3"), "(2) Select one pathway",PW_signatures),
					  	selectInput(ns("cor_method3"), "(3) Select correlation method",c('pearson','spearman')),
					  )
					),
				    shinyWidgets::actionBttn(
				      inputId = ns("plot_bttn"),
				      label = "Plot",
				      style = "gradient",
				      # icon = icon("search"),
				      color = "default",
				      block = TRUE,
				      size = "sm"
					)  
				),
		        wellPanel(
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
			        downloadBttn(
			          outputId = ns("download"),
			          label = "Download",
			          style = "gradient",
			          color = "default",
			          block = TRUE,
			          size = "sm"
			        )
			    )

			),
			column(
			    9,
			    plotOutput(ns("pw_plot"), height = "600px",width = "600px"),
			    hr(),
			    h5("NOTEs:"),
			    p("1. 500 common patwhay genesets from 3 resources(50 HALLMARK, 186 KEGG, 264 IOBR) were collected."),
			    p("2. Pathway scores of TCGA(toil) tumor patients were calculated using ssGSEA method."),
			    DT::DTOutput(ns("pw_data")),
		        shinyjs::hidden(
		          wellPanel(
		            id = ns("save_csv"),
		            downloadButton(ns("downloadTable"), "Save as csv")))
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
	    cnv_gistic2 = list(all = pancan_identifiers$gene, default = "TP53"),
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
	observeEvent(input$Mode, {
	  updateTabsetPanel(inputId = "mode_params", selected = input$Mode)
	}) 

    observeEvent(input$reset, {
        reset("mode_params")
    })

	# Show waiter for plot
	w <- waiter::Waiter$new(id = ns("pw_plot"), html = waiter::spin_hexdots(), color = "white")


	plot_func <- eventReactive(input$plot_bttn, {
		p = switch(input$Mode,
			one_many=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer1,
									 cor_cutoff = list(r=input$cor_coef1, p=input$cor_pval1),cor_method = input$cor_method1),
			one_one=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer2,
									pw_name=input$pw_name2,cor_method = input$cor_method2),
			many_one=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer3,
									pw_name=input$pw_name3,cor_method = input$cor_method3)
		)
	  return(p)
	})

	data_func <- eventReactive(input$plot_bttn, {
		p_dat = switch(input$Mode,
			one_many=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer1,
									 cor_cutoff = list(r=input$cor_coef1, p=input$cor_pval1),cor_method = input$cor_method1, plot=FALSE),
			one_one=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer2,
									pw_name=input$pw_name2,cor_method = input$cor_method2, plot=FALSE),
			many_one=vis_gene_pw_cor(Gene=input$Pancan_search,data_type=input$profile,Cancer=input$Cancer3,
									pw_name=input$pw_name3,cor_method = input$cor_method3, plot=FALSE)
		)
	  return(p_dat)
	})

	output$pw_plot <- renderPlot({
	  w$show() # Waiter add-ins
	  plot_func()
	})
	output$pw_data <- renderDT({
	  data_func()
	})

	output$download <- downloadHandler(
	  filename = function() {
	    paste0(input$Pancan_search,"_", input$profile,"_",input$Mode,".", input$device)
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
	    paste0(input$Pancan_search,"_", input$profile,"_",input$Mode,".csv")
	  },
	  content = function(file) {
	    write.csv(data_func(), file, row.names = FALSE)
	  }
	)

}