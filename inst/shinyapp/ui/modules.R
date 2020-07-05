ui.page_modules <- function() {
  navbarMenu(
    title = "Modules",
    icon = icon("buromobelexperte"),
    tabPanel(
      "Upload File",
      ui.file_upload("module_file_upload", test = TRUE)),
    tabPanel(
      "Gene Pan-cancer Expression Distribution ",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    tabPanel(
      "Gene Expression Cox Analysis",
      ui.modules_pancan_unicox("modules_pancan_unicox")
    ),
    tabPanel(
      "Gene Expression vs Immune Correlation Analysis"
    ),
    tabPanel(
      "Survival Analysis",
      ui.modules_sur_plot("modules_sur_plot")
    )
  )
}