ui.page_pancan_ccle <- function() {
	navbarMenu(
		title = "CCLE Analysis",
		icon = icon("buromobelexperte"),
		tabPanel(
			"CCLE Association Analysis",
		    h1("CCLE Association Analysis", align = "center"),
		    tabsetPanel(
		      tabPanel(
		        "Sole Analysis for Cell Lines",
		        br(),
		        ui.modules_ccle_cor_o2o("modules_ccle_cor_o2o")
		      ),
		      tabPanel(
		        "Batch Analysis for Cell Lines",
		        br(),
		        ui.modules_ccle_cor_m2o("modules_ccle_cor_m2o")
		      )
		    )	
		),
		tabPanel(
			"CCLE Comparison Analysis",
		    h1("CCLE Comparison Analysis", align = "center"),
		    tabsetPanel(
		      tabPanel(
		        "Sole Analysis for Cell Lines",
		        br(),
		        ui.modules_ccle_comp_o2o("modules_ccle_comp_o2o")
		      ),
		      tabPanel(
		        "Batch Analysis for Cell Lines",
		        br(),
		        ui.modules_ccle_comp_m2o("modules_ccle_comp_m2o")
		      )
		    )	
		)
	)
}
