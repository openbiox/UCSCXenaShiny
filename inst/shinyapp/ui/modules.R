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
      "Gene Cancer Expression Distribution",
      ui.modules_cancer_dist("modules_cancer_dist")
    ),
    tabPanel(
      "Gene Pan-cancer Expression Anatomy",
      ui.modules_pancan_anatomy("modules_pancan_anatomy")
    ),
    tabPanel(
      "Gene Expression Cox Analysis",
      ui.modules_pancan_unicox("modules_pancan_unicox")
    ),
    tabPanel(
      "Gene Expression vs Immune Correlation Analysis",
      ui.modules_pancan_immune("modules_pancan_immune")
    ),
    tabPanel(
      "Survival Analysis",
      ui.modules_sur_plot("modules_sur_plot")
    ),
    tabPanel(
      "Gene CCLE Expression Distribution",
      ui.modules_ccle_dist("modules_ccle_dist")
    )
  )
}