ui.page_pancan_pcawg <- function() {
	navbarMenu(
		title = "PCAWG Analysis",
		icon = icon("buromobelexperte"),
	    tabPanel(
	      "PCAWG Association Analysis",
	      h1("PCAWG Association Analysis", align = "center"),
	      tabsetPanel(
	        tabPanel(
	          "Sole Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_cor_o2o("modules_pcawg_cor_o2o")
	        ),
	        tabPanel(
	          "Sole Analysis for Multiple Cancers",
	          br(),
	          ui.modules_pcawg_cor_o2m("modules_pcawg_cor_o2m")
	        ),
	        tabPanel(
	          "Batch Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_cor_m2o("modules_pcawg_cor_m2o")
	        )
	      )
	    ),
	    tabPanel(
	      "PCAWG Comparison Analysis",
	      h1("PCAWG Comparison Analysis", align = "center"),
	      tabsetPanel(
	        tabPanel(
	          "Sole Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_comp_o2o("modules_pcawg_comp_o2o")
	        ),
	        tabPanel(
	          "Sole Analysis for Multiple Cancers",
	          br(),
	          ui.modules_pcawg_comp_o2m("modules_pcawg_comp_o2m")
	        ),
	        tabPanel(
	          "Batch Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_comp_m2o("modules_pcawg_comp_m2o")
	        )
	      )
	    ),
	    tabPanel(
	      "PCAWG Survival Analysis",
	      h1("PCAWG Survival Analysis(OS)", align = "center"),
	      tabsetPanel(
	        tabPanel(
	          "Sole Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_sur_o2o("modules_pcawg_sur_o2o")
	        ),
	        tabPanel(
	          "Sole Analysis for Multiple Cancers",
	          br(),
	          ui.modules_pcawg_sur_o2m("modules_pcawg_sur_o2m")
	        ),
	        tabPanel(
	          "Batch Analysis for Single Cancer",
	          br(),
	          ui.modules_pcawg_sur_m2o("modules_pcawg_sur_m2o")
	        )
	      )
	    )
	)
}