mol_origin_UI = function(id, database = "toil"){
  ns = NS(id)
  if (!is.null(database)) {
    if (database == "toil") {
      choices = c("mRNA Expression","Transcript Expression","DNA Methylation",
      						"Protein Expression","miRNA Expression","Mutation status","Copy Number Variation")
    } else if (database == "pcawg") {
      choices = c("mRNA Expression","Gene Fusion", "Promoter Activity",
      						"miRNA Expression","APOBEC Mutagenesis")
    } else if (database == "ccle") {
      choices = c("mRNA Expression","Protein Expression",
      						"Copy Number Variation","Mutation status")
    }
  }
  selected = choices[1]

	wellPanel(
		style = "background: #f7f7f7",
		div(
			selectInput(
				ns("data_origin"),NULL,
				choices = choices,
				selected = selected
			), style = "margin-top: 0px;"
		),
		tabsetPanel(id = ns("data_origin_type"),
			type = "hidden",
			tabPanel("mRNA Expression",
				if(database == "toil"){
			    shinyWidgets::prettyRadioButtons(
			        inputId = ns("toil_gene_norm"), label = "(1)Normalization method",
			        choiceValues = c("tpm", "fpkm", "nc"),
			        choiceNames = c("RSEM TPM", "RSEM FPKM", "RSEM Norm_Count"),
			        selected = "tpm")
		    } else if(database == "ccle"){
			    shinyWidgets::prettyRadioButtons(
			        inputId = ns("ccle_gene_norm"), label = "(1)Type",
			        choiceValues = c("rpkm", "nc"),
			        choiceNames = c("RPKM", "log2(count+1)"),
			        selected = "rpkm"
			    )
		    } else if(database == "pcawg"){
			    p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
		    }
			),
			tabPanel("Transcript Expression",
				if(database == "toil"){
			    shinyWidgets::prettyRadioButtons(
			        inputId = ns("toil_trans_norm"), label = "(1)Normalization method",
			        choiceValues = c("tpm", "fpkm", "isopct"),
			        choiceNames = c("RSEM TPM", "RSEM FPKM", "RSEM Isoform Percentage"),
			        selected = "tpm"
			    )
				}
			),
			tabPanel("DNA Methylation",
				if(database == "toil"){
					tagList(
						fluidRow(
							column(
								6,
								selectInput(ns("toil_L2_3_methy_1"),"(1)Platform",
									choices = c("450K","27K"), selected = TRUE)
							),
							column(
								6,
								virtualSelectInput(ns("toil_L2_3_methy_3_gene"),"(2)Gene",
									choices = NULL, 
									search = TRUE,
									allowNewOption = TRUE,
									dropboxWidth = "200%"),
							)
						),
						fluidRow(
							column(6,numericInput(ns("chr_min"),"Min coord",value = NULL)),
							column(6,numericInput(ns("chr_max"),"Max coord",value = NULL))
						),
						fluidRow(
							column(
								8,
				        virtualSelectInput(
				          inputId = ns("toil_L2_3_methy_3_cpg"),
				          label = "(4)CpG sites",
				          choices = NULL,
				          multiple = TRUE,
				          allowNewOption = TRUE)
							),
							column(
								4,
								selectInput(ns("toil_L2_3_methy_2"),"(5)Aggregation",
									choices = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"), 
									selected = "mean")
							)
						),
						verbatimTextOutput(ns("cpgs_sle_text"))
					)
				}
			),
			tabPanel("Copy Number Variation",
				if(database == "toil"){
					tagList(
						selectInput(ns("toil_L2_7_cnv_1"),"(1)GISTIC2 data",
							choices = c(TRUE, FALSE), selected = TRUE),
						selectInput(ns("toil_L2_7_cnv_2"),"(2)Thresholded data",
							choices = c(TRUE, FALSE), selected = FALSE)	
					)
				} else if (database == "ccle"){
					p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
				}
			),
			tabPanel("miRNA Expression",
				if(database == "pcawg"){
			    shinyWidgets::prettyRadioButtons(
			        inputId = ns("pcawg_mi_norm"), label = "(1)Normalization method",
			        choiceValues = c("TMM", "UQ"),
			        choiceNames = c("TMM", "UQ"),
			        selected = "TMM"
			    )
				} else if (database=="toil"){
					p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
				}
			),
			tabPanel("Promoter Activity",
				# only PCAWG
		    shinyWidgets::prettyRadioButtons(
		        inputId = ns("pcawg_pro_type"), label = "(1)Type",
		        choiceValues = c("raw", "relative", "outlier"),
		        choiceNames = c("Raw", "Relative", "Outlier"),
		        selected = "raw"
		    )
			),
			tabPanel("APOBEC Mutagenesis",
					p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
			),
			tabPanel("Protein Expression",
					p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
			),
			tabPanel("Mutation status",
					p(strong(span("No alternative datasets can be chosen.", style="color:grey")))
			)
		),
		# hr(style = "border-top: 1px solid #000000;"),
		div(style = "border-top: 1px dashed #000; margin-top: 5px; margin-bottom: 5px;"),
		# p(strong("Active Data Hub: "), a("toilHub", href = "http://baidu.com"), style = "margin-top: 0px; margin-bottom: 0px;"),
		# p(strong("Active Data Set: "), a("tcga_RSEM_gene_fpkm", href = "http://baidu.com"), style = "margin-top: 0px; margin-bottom: 0px;"),
		uiOutput(ns("hub_set.ui")),
		# p("See Repository for more information."),
		div(style = "border-top: 1px solid #000; margin-top: 5px; margin-bottom: 5px;"),

		# verbatimTextOutput(ns("tmp123"))
	)
}


mol_origin_Server = function(input, output, session, database = "toil"){
	ns <- session$ns

	observeEvent(input$data_origin, {
	  updateTabsetPanel(inputId = "data_origin_type", 
	  	selected = input$data_origin)
	}) 

  updateVirtualSelect(
    inputId = "toil_L2_3_methy_3_gene",
    choices = pancan_identifiers$gene,
    selected = "TP53"
  )

  # 特定基因下所包含的所有CpG位点
	candi_cpg = reactive({
			if(is.null(input$toil_L2_3_methy_1)){
				toil_L2_3_methy_1 = "450K"
			} else {
				toil_L2_3_methy_1 = input$toil_L2_3_methy_1
			}
			candi_cpg = switch(toil_L2_3_methy_1,
				`450K` = tcga_id_referrence$id_molecule$id_M450,
				`27K` = tcga_id_referrence$id_molecule$id_M27K
			) %>%
				dplyr::filter(Level3 %in% input$toil_L2_3_methy_3_gene)
	})

	observe({
		cpg_ids = candi_cpg() %>% 
			dplyr::filter(chromStart >= input$chr_min) %>% 
			dplyr::filter(chromEnd  <= input$chr_max) %>% 
			dplyr::pull(CpG)
    updateVirtualSelect(
      "toil_L2_3_methy_3_cpg",
      choices = cpg_ids,
      selected = cpg_ids
      # selected = cpg_ids
    )
	})

	observe({
		if(nrow(candi_cpg())>0){
			updateNumericInput(
				session,
				"chr_min",
				label = paste("(3)Min coord ","(",unique(candi_cpg()$chrom),")"),
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
		if (is.data.frame(candi_cpg()) && nrow(candi_cpg()) > 0) {
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
		} else NULL
	})
	# 提示包含多少个CpG位点
	output$cpgs_sle_text = renderPrint({
		cat(paste0("Tip: ",length(cpg_ids_retain())," CpG sites are included."))
	})

	opt_pancan = reactive({
		opt_pancan = .opt_pancan
		if(database=="toil"){
			opt_pancan$toil_mRNA = list(norm = input$toil_gene_norm)
			opt_pancan$toil_transcript = list(norm = input$toil_trans_norm)
			opt_pancan$toil_cnv = list(gistic2 = ifelse(is.null(input$toil_L2_7_cnv_1),TRUE,as.logical(input$toil_L2_7_cnv_1)),
											use_thresholded_data = ifelse(is.null(input$toil_L2_7_cnv_2),TRUE,as.logical(input$toil_L2_7_cnv_2)))
			opt_pancan$toil_methylation = list(type = ifelse(is.null(input$toil_L2_3_methy_1),"450K",input$toil_L2_3_methy_1), 
									aggr = ifelse(is.null(input$toil_L2_3_methy_2),"NA",input$toil_L2_3_methy_2),
									rule_out = setdiff(candi_cpg()$CpG, cpg_ids_retain()))
		} else if (database=="pcawg"){
		  opt_pancan$pcawg_miRNA = list(norm = input$pcawg_mi_norm)
		  opt_pancan$pcawg_promoter = list(type = input$pcawg_pro_type)
		} else if (database=="ccle"){
		  opt_pancan$ccle_mRNA = list(norm = input$ccle_gene_norm)
		}
		opt_pancan
	})


	output$hub_set.ui = renderUI({

		active_hub = "toilHub"
		active_set = "tcga_RSEM_gene_fpkm"
		link_hub = "https://toil.xenahubs.net"
		link_set = "https://xenabrowser.net/datapages/?dataset=tcga_RSEM_gene_fpkm&host=https://toil.xenahubs.net"

		if(database=="toil"){
			active_hub = "toilHub"
			link_hub = "https://toil.xenahubs.net"
			if(input$data_origin == "mRNA Expression"){
				active_set = switch(input$toil_gene_norm,
					"tpm" = "TcgaTargetGtex_rsem_gene_tpm",
					"fpkm" = "TcgaTargetGtex_rsem_gene_fpkm",
					"nc" = "TcgaTargetGtex_RSEM_Hugo_norm_count"		
				)
				link_set = switch(input$toil_gene_norm,
					"tpm" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_gene_tpm&host=https://toil.xenahubs.net",
					"fpkm" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_gene_fpkm&host=https://toil.xenahubs.net",
					"nc" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_RSEM_Hugo_norm_count&host=https://toil.xenahubs.net"		
				)
			} else if(input$data_origin == "Transcript Expression"){
				active_set = switch(input$toil_trans_norm,
					"tpm" = "TcgaTargetGtex_rsem_isoform_tpm",
					"fpkm" = "TcgaTargetGtex_RSEM_isoform_fpkm",
					"isopct" = "TcgaTargetGtex_rsem_isopct"		
				)
				link_set = switch(input$toil_trans_norm,
					"tpm" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_isoform_tpm&host=https://toil.xenahubs.net",
					"fpkm" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_RSEM_isoform_fpkm&host=https://toil.xenahubs.net",
					"isopct" = "https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_isopct&host=https://toil.xenahubs.net"		
				)
			} else if(input$data_origin == "DNA Methylation"){
				# Not toil hub!
				active_hub = "gdcHub"
				link_hub = "https://gdc.xenahubs.net"
				active_set = switch(input$toil_L2_3_methy_1,
					"450K" = "GDC-PANCAN.methylation450.tsv",
					"27K" = "GDC-PANCAN.methylation27.tsv"	
				)
				link_set = switch(input$toil_L2_3_methy_1,
					"450K" = "https://xenabrowser.net/datapages/?dataset=GDC-PANCAN.methylation450.tsv&host=https://gdc.xenahubs.net",
					"27K" = "https://xenabrowser.net/datapages/?dataset=GDC-PANCAN.methylation27.tsv&host=https://gdc.xenahubs.net"
				)
			} else if(input$data_origin == "Protein Expression"){
				# Not toil hub!
				active_hub = "pancanAtlasHub"
				active_set = "TCGA-RPPA-pancan-clean.xena"
				link_hub = "https://pancanatlas.xenahubs.net"
				link_set = "https://xenabrowser.net/datapages/?dataset=TCGA-RPPA-pancan-clean.xena&host=https://pancanatlas.xenahubs.net"
			} else if(input$data_origin == "miRNA Expression"){
				# Not toil hub!
				active_hub = "pancanAtlasHub"
				active_set = "pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena"
				link_hub = "https://pancanatlas.xenahubs.net"
				link_set = "https://xenabrowser.net/datapages/?dataset=pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena&host=https://pancanatlas.xenahubs.net"
			} else if(input$data_origin == "Mutation status"){
				# Not toil hub!
				active_hub = "pancanAtlasHub"
				active_set = "mc3.v0.2.8.PUBLIC.nonsilentGene.xena"
				link_hub = "https://pancanatlas.xenahubs.net"
				link_set = "https://xenabrowser.net/datapages/?dataset=mc3.v0.2.8.PUBLIC.nonsilentGene.xena&host=https://pancanatlas.xenahubs.net"
			} else if(input$data_origin == "Copy Number Variation"){
				if(!as.logical(input$toil_L2_7_cnv_1)){
					# Not toil hub!
					active_hub = "pancanAtlasHub"
					active_set = "broad.mit.edu_PANCAN_Genome_Wide_SNP_6_whitelisted.gene.xena"
					link_hub = "https://pancanatlas.xenahubs.net"
					link_set = "https://xenabrowser.net/datapages/?dataset=broad.mit.edu_PANCAN_Genome_Wide_SNP_6_whitelisted.gene.xena&host=https://pancanatlas.xenahubs.net"
				} else if (as.logical(input$toil_L2_7_cnv_2)){
					active_hub = "tcgaHub"
					active_set = "TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes"
					link_hub = "https://tcga.xenahubs.net"
					link_set = "https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes&host=https://tcga.xenahubs.net"					
				} else if (!as.logical(input$toil_L2_7_cnv_2)){
					active_hub = "tcgaHub"
					active_set = "TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_data_by_genes"
					link_hub = "https://tcga.xenahubs.net"
					link_set = "https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_data_by_genes&host=https://tcga.xenahubs.net"					
				}
			} 
		} else if (database=="pcawg"){
			active_hub = "pcawgHub"
			link_hub = "https://pcawg.xenahubs.net"
			if(input$data_origin == "mRNA Expression"){
				active_set = "tophat_star_fpkm_uq.v2_aliquot_gl.sp.log"
				link_set = "https://xenabrowser.net/datapages/?dataset=tophat_star_fpkm_uq.v2_aliquot_gl.sp.log&host=https://pcawg.xenahubs.net"
			} else if(input$data_origin == "Gene Fusion"){
				active_set = "pcawg3_fusions_PKU_EBI.gene_centric.sp.xena"
				link_set = "https://xenabrowser.net/datapages/?dataset=pcawg3_fusions_PKU_EBI.gene_centric.sp.xena&host=https://pcawg.xenahubs.net"
			} else if(input$data_origin == "miRNA Expression"){
				active_set = switch(input$pcawg_mi_norm,
					"TMM" = "x3t2m1.mature.TMM.mirna.matrix.log",
					"UQ" = "x3t2m1.mature.UQ.mirna.matrix.log"
				)
				link_set = switch(input$pcawg_mi_norm,
					"TMM" = "https://xenabrowser.net/datapages/?dataset=x3t2m1.mature.UQ.mirna.matrix.log&host=https://pcawg.xenahubs.net",
					"UQ" = "https://xenabrowser.net/datapages/?dataset=promoterCentricTable_0.2_1.0.sp&host=https://pcawg.xenahubs.net"
				)
			} else if(input$data_origin == "Promoter Activity"){
				active_set = switch(input$pcawg_pro_type,
					"raw" = "rawPromoterActivity.sp",
					"relative" = "relativePromoterActivity.sp",
					"outlier" = "promoterCentricTable_0.2_1.0.sp"
				)
				link_set = switch(input$pcawg_pro_type,
					"raw" = "https://xenabrowser.net/datapages/?dataset=rawPromoterActivity.sp&host=https://pcawg.xenahubs.net",
					"relative" = "https://xenabrowser.net/datapages/?dataset=relativePromoterActivity.sp&host=https://pcawg.xenahubs.net",
					"outlier" = "https://xenabrowser.net/datapages/?dataset=promoterCentricTable_0.2_1.0.sp&host=https://pcawg.xenahubs.net"
				)
			} else if(input$data_origin == "APOBEC Mutagenesis"){
				active_set = "MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp"
				link_set = "https://xenabrowser.net/datapages/?dataset=MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp&host=https://pcawg.xenahubs.net"
			}  
		} else if (database=="ccle"){
			active_hub = "publicHub"
			link_hub = "https://ucscpublic.xenahubs.net"
			if(input$data_origin == "mRNA Expression"){
				active_set = switch(input$ccle_gene_norm,
					"rpkm" = "ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502",
					"nc" = "ccle/CCLE_DepMap_18Q2_RNAseq_reads_20180502.log2"		
				)
				link_set = switch(input$ccle_gene_norm,
					"rpkm" = "https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502&host=https://ucscpublic.xenahubs.net",
					"nc" = "https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_RNAseq_reads_20180502.log2&host=https://ucscpublic.xenahubs.net"		
				)
			} else if(input$data_origin == "Protein Expression"){
				active_set = "ccle/CCLE_RPPA_20180123"
				link_set = "https://xenabrowser.net/datapages/?dataset=ccle/CCLE_RPPA_20180123&host=https://ucscpublic.xenahubs.net"
			} else if(input$data_origin == "Mutation status"){
				active_set = "ccle/CCLE_DepMap_18Q2_maf_20180502"
				link_set = "https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_maf_20180502&host=https://ucscpublic.xenahubs.net"
			} else if(input$data_origin == "Copy Number Variation"){
				active_set = "ccle/CCLE_copynumber_byGene_2013-12-03"
				link_set = "https://xenabrowser.net/datapages/?dataset=ccle/CCLE_copynumber_byGene_2013-12-03&host=https://ucscpublic.xenahubs.net"
			}  
		}

		tagList(
			p(strong("Active Data Hub : "), a(active_hub, href = link_hub), 
					style = "margin-top: 0px; margin-bottom: 0px;"),
			p(strong("Active Data Set : "), a(active_set, href = link_set), 
					style = "margin-top: 0px; margin-bottom: 0px;"),
		)
	})



	# output$tmp123 = renderPrint({
	# 	opt_pancan()
	# })

	return(opt_pancan)

}
