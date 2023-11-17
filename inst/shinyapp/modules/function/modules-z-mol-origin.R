mol_origin_UI = function(id, button_name="Multi-conditions filters"){
	ns = NS(id)
	tagList(
		selectInput(
			ns("data_origin"),NULL,
			choices = c("mRNA Expression","Transcript Expression","DNA Methylation",
				"Protein Expression","miRNA Expression","Mutation status","Copy Number Variation"),
			selected = "mRNA Expression"
		),
		tabsetPanel(id = ns("data_origin_type"),
			type = "hidden",
			tabPanel("mRNA Expression",
				p("It cannot be modified at the moment.")

			),
			tabPanel("Transcript Expression",
				p("It cannot be modified at the moment.")

			),
			tabPanel("DNA Methylation",
				fluidRow(
					column(
						6,
						selectInput(ns("L2_3_methy_1"),"(1)Type",
							choices = c("450K","27K"), selected = TRUE)
					),
					column(
						6,
						selectInput(ns("L2_3_methy_2"),"(2)Aggregation",
							choices = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"), 
							selected = "mean")
					)
				),
				## relu_out
				fluidRow(
					column(
						6,
						selectizeInput(ns("L2_3_methy_3_gene"),"(3)Pinpoint CpG by Gene",
							choices = NULL, options = list(create = TRUE, maxOptions = 5))
					),
					column(
						6,
			            selectizeInput(
			              inputId = ns("L2_3_methy_3_cpg"),
			              label = "CpG sites",
			              choices = NULL,
			              multiple = TRUE,
			              options = list(create = TRUE, maxOptions = 5))

					)
				)
			),
			tabPanel("Protein Expression",
				p("It cannot be modified at the moment.")

			),
			tabPanel("miRNA Expression",
				p("miRNA Expression")

			),
			tabPanel("Mutation status",
				p("It cannot be modified at the moment.")
			),
			tabPanel("Copy Number Variation",
				selectInput(ns("L2_7_cnv_1"),"(1)Thresholded data",
					choices = c(TRUE, FALSE), selected = TRUE)
			)
		)
	)
}



mol_origin_Server = function(input, output, session, cancers=NULL, custom_metadata=NULL, opt_pancan=NULL){
	ns <- session$ns

	observeEvent(input$data_origin, {
	  updateTabsetPanel(inputId = "data_origin_type", 
	  	selected = input$data_origin)
	}) 

    updateSelectizeInput(
      session,
      "L2_3_methy_3_gene",
      choices = pancan_identifiers$gene,
      selected = "TP53",
      server = TRUE
    )

	observe({
		cpg_type = reactive({
			if(is.null(input$L2_3_methy_1)){
				L2_3_methy_1 = "450K"
			} else {
				L2_3_methy_1 = input$L2_3_methy_1
			}
			switch(L2_3_methy_1,
				`450K` = id_merge$id_molecule$id_M450[,c("Level3","CpG")],
				`27K` = id_merge$id_molecule$id_M27K[,c("Level3","CpG")]
			)
		})
		cpg_ids = cpg_type() %>% 
			dplyr::filter(Level3 %in% input$L2_3_methy_3_gene) %>% 
			dplyr::pull(CpG)
	    updateSelectizeInput(
	      session,
	      "L2_3_methy_3_cpg",
	      choices = cpg_ids,
	      selected = NULL,
	      server = TRUE
	    )
	})

	opt_pancan = reactive({
		cpg_type = reactive({
			if(is.null(input$L2_3_methy_1)){
				L2_3_methy_1 = "450K"
			} else {
				L2_3_methy_1 = input$L2_3_methy_1
			}
			switch(L2_3_methy_1,
				`450K` = id_merge$id_molecule$id_M450[,c("Level3","CpG")],
				`27K` = id_merge$id_molecule$id_M27K[,c("Level3","CpG")]
			)
		})
		cpg_ids = cpg_type() %>% 
			dplyr::filter(Level3 %in% input$L2_3_methy_3_gene) %>% 
			dplyr::pull(CpG)
		if(is.null(input$L2_3_methy_3_cpg)){
			cpg_ids_retain = NULL
		} else {
			cpg_ids_retain = setdiff(cpg_ids, input$L2_3_methy_3_cpg)
		}

		list(
			toil_mRNA = list(),
			toil_transcript = list(),
			toil_protein = list(),
			toil_mutation = list(),
			toil_cnv = list(use_thresholded_data = ifelse(is.null(input$L2_7_cnv_1),TRUE,as.logical(input$L2_7_cnv_1))),
			toil_methylation = list(type = ifelse(is.null(input$L2_3_methy_1),"450K",input$L2_3_methy_1), 
									aggr = ifelse(is.null(input$L2_3_methy_2),"NA",input$L2_3_methy_2),
									rule_out = cpg_ids_retain),
			toil_miRNA = list()
		)
	})
	return(opt_pancan)
}