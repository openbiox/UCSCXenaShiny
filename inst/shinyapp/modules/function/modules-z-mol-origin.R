mol_origin_UI = function(id, button_name="Multi-conditions filters", profile = NULL, source = NULL){
  ns = NS(id)
  
  if (!is.null(profile) & !is.null(source)) {
    if (source == "toil") {
      choices = switch(profile,
                       mRNA = "TOIL-mRNA Expression",
                       transcript = "TOIL-Transcript Expression",
                       methylation = "TOIL-DNA Methylation",
                       cnv = "TOIL-Copy Number Variation")
    } else if (source == "pcawg") {
      choices = switch(profile,
                       miRNA = "PCAWG-miRNA Expression",
                       promoter = "PCAWG-Promoter Activity")
    } else if (source == "ccle") {
      choices = switch(profile,
                       mRNA = "CCLE-mRNA Expression")
    }
  } else {
    choices = c("TOIL-mRNA Expression","TOIL-Transcript Expression",
                "TOIL-DNA Methylation","TOIL-Copy Number Variation",
                "PCAWG-miRNA Expression","PCAWG-Promoter Activity",
                "CCLE-mRNA Expression")
  }
  selected = choices[1]
  
	tagList(
		selectInput(
			ns("data_origin"),NULL,
			choices = choices,
			selected = selected
		),
		tabsetPanel(id = ns("data_origin_type"),
			type = "hidden",
			tabPanel("TOIL-mRNA Expression",
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("toil_gene_norm"), label = "(1)Normalization method",
		        choiceValues = c("tpm", "fpkm", "nc"),
		        choiceNames = c("RSEM TPM", "RSEM FPKM", "RSEM Norm_Count"),
		        selected = "tpm"
		    )
			),
			tabPanel("TOIL-Transcript Expression",
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("toil_trans_norm"), label = "(1)Normalization method",
		        choiceValues = c("tpm", "fpkm", "isopct"),
		        choiceNames = c("RSEM TPM", "RSEM FPKM", "RSEM Isoform Percentage"),
		        selected = "tpm"
		    ),

			),
			tabPanel("TOIL-DNA Methylation",
				fluidRow(
					column(
						6,
						selectInput(ns("toil_L2_3_methy_1"),"(1)Type",
							choices = c("450K","27K"), selected = TRUE)
					),
					column(
						6,
						selectizeInput(ns("toil_L2_3_methy_3_gene"),"(2)Gene",
							choices = NULL, options = list(create = TRUE, maxOptions = 5)),
					)
				),
				fluidRow(
					column(6,numericInput(ns("chr_min"),"Min coord",value = NULL)),
					column(6,numericInput(ns("chr_max"),"Max coord",value = NULL))
				),

				fluidRow(
					column(
						4,
						selectInput(ns("toil_L2_3_methy_2"),"(3)Aggregation",
							choices = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"), 
							selected = "mean")
					),
					column(
						8,
		        selectizeInput(
		          inputId = ns("toil_L2_3_methy_3_cpg"),
		          label = "(4)CpG sites",
		          choices = NULL,
		          multiple = TRUE,
		          options = list(create = TRUE, maxOptions = 5))
					)
				),
				verbatimTextOutput(ns("cpgs_sle_text"))
			),
			tabPanel("TOIL-Copy Number Variation",
				selectInput(ns("toil_L2_7_cnv_1"),"(1)GISTIC2 data",
					choices = c(TRUE, FALSE), selected = TRUE),
				selectInput(ns("toil_L2_7_cnv_2"),"(2)Thresholded data",
					choices = c(TRUE, FALSE), selected = FALSE)				
			),
			tabPanel("PCAWG-miRNA Expression",
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("pcawg_mi_norm"), label = "(1)Normalization method",
		        choiceValues = c("TMM", "UQ"),
		        choiceNames = c("TMM", "UQ"),
		        selected = "TMM"
		    )
			),
			tabPanel("PCAWG-Promoter Activity",
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("pcawg_pro_type"), label = "(1)Type",
		        choiceValues = c("raw", "relative", "outlier"),
		        choiceNames = c("Raw", "Relative", "Outlier"),
		        selected = "raw"
		    )
			),
			tabPanel("CCLE-mRNA Expression",
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("ccle_gene_norm"), label = "(1)Type",
		        choiceValues = c("rpkm", "nc"),
		        choiceNames = c("RPKM", "log2(count+1)"),
		        selected = "rpkm"
		    )
			),
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
    "toil_L2_3_methy_3_gene",
    choices = pancan_identifiers$gene,
    selected = "TP53",
    server = TRUE
  )

  # 特定基因下所包含的所有CpG位点
	candi_cpg = reactive({
			if(is.null(input$toil_L2_3_methy_1)){
				toil_L2_3_methy_1 = "450K"
			} else {
				toil_L2_3_methy_1 = input$toil_L2_3_methy_1
			}
			candi_cpg = switch(toil_L2_3_methy_1,
				`450K` = id_referrence$id_molecule$id_M450,
				`27K` = id_referrence$id_molecule$id_M27K
			) %>%
				dplyr::filter(Level3 %in% input$toil_L2_3_methy_3_gene)
	})

	observe({
		cpg_ids = candi_cpg() %>% 
			dplyr::filter(chromStart >= input$chr_min) %>% 
			dplyr::filter(chromEnd  <= input$chr_max) %>% 
			dplyr::pull(CpG)
    updateSelectizeInput(
      session,
      "toil_L2_3_methy_3_cpg",
      choices = cpg_ids,
      selected = NULL,
      server = TRUE
    )
	})

	observe({
		if(nrow(candi_cpg())>0){
			updateNumericInput(
				session,
				"chr_min",
				label = paste("Min coord ","(",unique(candi_cpg()$chrom),")"),
				value = min(candi_cpg()$chromStart)
			)
			updateNumericInput(
				session,
				"chr_max",
				label = paste("Max coord ","(",unique(candi_cpg()$chrom),")"),
				value = max(candi_cpg()$chromEnd)
			)
		}
	})

	cpg_ids_retain = reactive({
		shiny::validate(
			need(try(input$chr_min>=min(candi_cpg()$chromStart)), 
				paste0("Min coord cannot be less than ",min(candi_cpg()$chromStart),".")),
			need(try(input$chr_max<=max(candi_cpg()$chromEnd)), 
				paste0("Max coord cannot be greater than ",max(candi_cpg()$chromEnd),"."))
		)
		cpg_ids = candi_cpg() %>% 
			dplyr::filter(chromStart >= input$chr_min) %>% 
			dplyr::filter(chromEnd  <= input$chr_max) %>% 
			dplyr::pull(CpG)
		if(is.null(input$toil_L2_3_methy_3_cpg)){
			cpg_ids_retain = cpg_ids
		} else {
			cpg_ids_retain = input$toil_L2_3_methy_3_cpg
		}
		cpg_ids_retain
	})
	# 提示包含多少个CpG位点
	output$cpgs_sle_text = renderPrint({
		cat(paste0("Tip: ",length(cpg_ids_retain())," CpG sites are included."))
	})





	opt_pancan = reactive({

		list(
			toil_mRNA = list(norm = input$toil_gene_norm),
			toil_transcript = list(norm = input$toil_trans_norm),
			toil_protein = list(),
			toil_mutation = list(),
			toil_cnv = list(gistic2 = ifelse(is.null(input$toil_L2_7_cnv_1),TRUE,as.logical(input$toil_L2_7_cnv_1)),
											use_thresholded_data = ifelse(is.null(input$toil_L2_7_cnv_2),TRUE,as.logical(input$toil_L2_7_cnv_2))),
			toil_methylation = list(type = ifelse(is.null(input$toil_L2_3_methy_1),"450K",input$toil_L2_3_methy_1), 
									aggr = ifelse(is.null(input$toil_L2_3_methy_2),"NA",input$toil_L2_3_methy_2),
									rule_out = setdiff(candi_cpg()$CpG, cpg_ids_retain())),
			toil_miRNA = list(),

		  pcawg_mRNA = list(),
		  pcawg_fusion = list(),
		  pcawg_miRNA = list(norm_method = input$pcawg_mi_norm),
		  pcawg_promoter = list(type = input$pcawg_pro_type),
		  pcawg_APOBEC = list(),

		  ccle_mRNA = list(norm = input$ccle_gene_norm),
		  ccle_protein = list(),
		  ccle_mutation = list(),
		  ccle_cnv = list()
		)
	})
	return(opt_pancan)
}