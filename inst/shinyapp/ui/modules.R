ui.page_modules <- function() {
  navbarMenu(
    title = "Modules",
    icon = icon("buromobelexperte"),
    tabPanel("Import & Export"),
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
