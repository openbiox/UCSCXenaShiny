ui.modules_id_reference = function(id) {
	ns = NS(id)
	fluidPage(
		navlistPanel(
			"Omics molecule identifiers",
			tabPanel(
				"1. TCGA Molecular profile",
				tabsetPanel(
					tabPanel("Gene", dataTableOutput(ns("dt_1_1_gene"))),
					tabPanel("Protein", dataTableOutput(ns("dt_1_2_pro"))),
					tabPanel("Mutation", dataTableOutput(ns("dt_1_3_mut"))),
					tabPanel("CNV", dataTableOutput(ns("dt_1_4_cn"))),
					tabPanel("Transcript", dataTableOutput(ns("dt_1_5_trans"))),
					tabPanel("Methylation(450K)", dataTableOutput(ns("dt_1_6_450"))),
					tabPanel("Methylation(27K)", dataTableOutput(ns("dt_1_7_27k"))),
					tabPanel("miRNA", dataTableOutput(ns("dt_1_8_mi")))
				)  %>%
					helper(type = "inline", size = "m", fade = TRUE, title="Tip:" , 
						content = "The Chromosomal coordinates is based on GENCODE V23(hg38)"),
			),
			tabPanel(
				"2. PCAWG Molecular profile",
				tabsetPanel(
					tabPanel("Gene", dataTableOutput(ns("dt_5_1_gene"))),
					tabPanel("Promoter", dataTableOutput(ns("dt_5_2_pro"))),
					tabPanel("Fusion", dataTableOutput(ns("dt_5_3_fu"))),
					tabPanel("miRNA", dataTableOutput(ns("dt_5_4_mi"))),
					tabPanel("APOBEC", dataTableOutput(ns("dt_5_5_apo")))
				)  %>%
					helper(type = "inline", size = "m", fade = TRUE, title="Tip:" , 
						content = "The Chromosomal coordinates is based on GENCODE 19(hg19)"),
			),
			tabPanel(
				"3. CCLE Molecular profile",
				tabsetPanel(
					tabPanel("Gene", dataTableOutput(ns("dt_6_1_gene"))),
					tabPanel("Protein", dataTableOutput(ns("dt_6_2_pro"))),
					tabPanel("CNV", dataTableOutput(ns("dt_6_3_cn"))),
					tabPanel("Mutation", dataTableOutput(ns("dt_6_4_mu")))
				)  %>%
					helper(type = "inline", size = "m", fade = TRUE, title="Tip:" , 
						content = "The Chromosomal coordinates is based on GENCODE 19(hg19)"),
			),
			"Other non-omics identifiers",
			tabPanel(
				"4. Tumor index",
				tabsetPanel(
					tabPanel("Purity", dataTableOutput(ns("dt_2_1_purity"))),
					tabPanel("Stemness", dataTableOutput(ns("dt_2_2_stemness"))),
					tabPanel("TMB", dataTableOutput(ns("dt_2_3_tmb"))),
					tabPanel("MSI", dataTableOutput(ns("dt_2_4_msi"))),
					tabPanel("Genome instability", dataTableOutput(ns("dt_2_5_instability"))),
				)
			),
			tabPanel(
				"5. Immune Infiltration",
				tabsetPanel(
					tabPanel("CIBERSORT", dataTableOutput(ns("dt_3_1_ciber"))),
					tabPanel("CIBERSORT-ABS", dataTableOutput(ns("dt_3_2_ciberABS"))),
					tabPanel("EPIC", dataTableOutput(ns("dt_3_3_epic"))),
					tabPanel("MCPCOUNTER", dataTableOutput(ns("dt_3_4_mcp"))),
					tabPanel("QUANTISEQ", dataTableOutput(ns("dt_3_5_quan"))),
					tabPanel("TIMER", dataTableOutput(ns("dt_3_6_timer"))),
					tabPanel("XCELL", dataTableOutput(ns("dt_3_7_xcell"))),
				)
			),
			tabPanel(
				"6. Pathway activity",
				tabsetPanel(
					tabPanel("HALLMARK", dataTableOutput(ns("dt_4_1_hm"))),
					tabPanel("KEGG", dataTableOutput(ns("dt_4_2_kegg"))),
					tabPanel("IOBR", dataTableOutput(ns("dt_4_3_iobr")))
				)
			),
			widths = c(3, 8)
		)
	)
}


server.modules_id_reference = function(input, output, session){
	ns = session$ns
	
	## Molecular profile(TCGA)
	output$dt_1_1_gene = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_gene,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_2_pro = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_pro,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_3_mut = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_mut,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_4_cn = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_cn,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE)))  
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_5_trans = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_trans,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_6_450 = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_M450,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_7_27k = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_M27K,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_1_8_mi = renderDataTable({
		dt = datatable(tcga_id_referrence$id_molecule$id_mi,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})


	## Molecular profile(PCAWG)
	output$dt_5_1_gene = renderDataTable({
		dt = datatable(pcawg_id_referrence$id_gene,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_5_2_pro = renderDataTable({
		dt = datatable(pcawg_id_referrence$id_pro,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_5_3_fu = renderDataTable({
		dt = datatable(pcawg_id_referrence$id_fusion,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_5_4_mi = renderDataTable({
		dt = datatable(pcawg_id_referrence$id_mi,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_5_5_apo = renderDataTable({
		dt = datatable(pcawg_id_referrence$id_maf,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	## Molecular profile(CCLE)
	output$dt_6_1_gene = renderDataTable({
		dt = datatable(ccle_id_referrence$id_gene,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_6_2_pro = renderDataTable({
		dt = datatable(ccle_id_referrence$id_pro,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_6_3_cn = renderDataTable({
		dt = datatable(ccle_id_referrence$id_cnv,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_6_4_mu = renderDataTable({
		dt = datatable(ccle_id_referrence$id_mut,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)),
						search = list(regex = TRUE))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	
	## Tumor index
	output$dt_2_1_purity = renderDataTable({
		dt = datatable(tcga_id_referrence$id_tumor_index$tcga_purity ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_2_2_stemness = renderDataTable({
		dt = datatable(tcga_id_referrence$id_tumor_index$tcga_stemness,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_2_3_tmb = renderDataTable({
		dt = datatable(tcga_id_referrence$id_tumor_index$tcga_tmb,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_2_4_msi = renderDataTable({
		dt = datatable(tcga_id_referrence$id_tumor_index$tcga_msi,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_2_5_instability = renderDataTable({
		dt = datatable(tcga_id_referrence$id_tumor_index$tcga_genome_instability,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})

	## Immune infiltration
	output$dt_3_1_ciber = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$CIBERSORT ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_2_ciberABS = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$`CIBERSORT-ABS` ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_3_epic = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$EPIC ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_4_mcp = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$MCPCOUNTER ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_5_quan = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$QUANTISEQ ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_6_timer = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$TIMER ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_3_7_xcell = renderDataTable({
		dt = datatable(tcga_id_referrence$id_TIL$XCELL ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})

	## Pathway activity
	output$dt_4_1_hm = renderDataTable({
		dt = datatable(tcga_id_referrence$id_PW$HALLMARK ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_4_2_kegg = renderDataTable({
		dt = datatable(tcga_id_referrence$id_PW$KEGG ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
	output$dt_4_3_iobr = renderDataTable({
		dt = datatable(tcga_id_referrence$id_PW$IOBR ,
			options=list(columnDefs = list(list(orderable=TRUE, targets=0)))) 
		dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
		dt
	})
}
