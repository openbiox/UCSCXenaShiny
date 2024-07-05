ui.page_download <- function() {
	navbarMenu(
	    title = "Download",
	    icon = icon("cloud-arrow-down"),
	    tabPanel(
			"The integrated TPC data",
			ui.modules_download_pancan("modules_download_pancan")
	    ),
	    tabPanel(
			"The Repository Dataset",
			ui.modules_download_dataset("modules_download_dataset")
	    ),
	)
}



