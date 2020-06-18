ui.page_modules <- function() {
  navbarMenu(
    title = "Modules",
    icon = icon("buromobelexperte"),
    tabPanel(
      "Import & Export",
      ui.file_upload("module_file_upload", test = TRUE)),
    tabPanel("Data Tidy"),
    tabPanel("General Analysis"),
    tabPanel("Genomic Analysis"),
    tabPanel(
      "Visualization",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    tabPanel(
      "Survival analysis",
      ui.modules_sur_plot("modules_sur_plot")
    )
  )
}