ui.page_download <- function() {
	navbarMenu(
	    title = "Download",
	    icon = icon("cloud-arrow-down"),
	    tabPanel(
			"Based on TCGA/PCAWG/CCLE Analysis",
			ui.modules_download_pancan("modules_download_pancan")
	    ),
	    tabPanel(
			"Based on Repository Datasets",
			ui.modules_download_dataset("modules_download_dataset")
	    ),
	)
}



