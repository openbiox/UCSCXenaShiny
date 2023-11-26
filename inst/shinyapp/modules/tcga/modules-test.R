ui.modules_test = function(id) {
	ns = NS(id)
	fluidPage(
		p("ssssssssssssss"),
		fluidRow(
			column(
				3,
				wellPanel(
					multi_upload_UI(ns("multi_upload2test"))
				)
			)
		)
	)
}


server.modules_test = function(input, output, session) {
	ns <- session$ns

	opt_pancan = reactive({
		 list(
		  # Toil
		  toil_mRNA = list(),
		  toil_transcript = list(),
		  toil_protein = list(),
		  toil_mutation = list(),
		  toil_cnv = list(use_thresholded_data = TRUE),
		  toil_methylation = list(type = "450K", rule_out = NULL, aggr = "NA"),
		  toil_miRNA = list(),
		  # PCAWG
		  pcawg_mRNA = list(),
		  pcawg_fusion = list(),
		  pcawg_miRNA = list(norm_method = "TMM"),
		  pcawg_promoter = list(type = "relative"),
		  pcawg_APOBEC = list(),
		  # CCLE
		  ccle_mRNA = list(),
		  ccle_protein = list(),
		  ccle_mutation = list(),
		  ccle_cnv = list()
		)
	})

	callModule(multi_upload_Server, "multi_upload2test",
		opt_pancan = reactive(opt_pancan()))


}