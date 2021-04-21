ui.page_pancan <- function() {
  navbarMenu(
    title = "Quick PanCan Analysis",
    icon = icon("buromobelexperte"),
    # tabPanel(
    #   "Combo: Single Gene Analysis",
    #   ui.combo.sg.pancan.analysis("combo.sg.pancan.analysis")
    # ),
    tabPanel(
      "TCGA Gene Expression Distribution ",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    tabPanel(
      "TCGA Gene Cancer Expression Distribution",
      ui.modules_cancer_dist("modules_cancer_dist")
    ),
    tabPanel(
      "TCGA Gene Expression Anatomy",
      ui.modules_pancan_anatomy("modules_pancan_anatomy")
    ),
    tabPanel(
      "TCGA Gene Expression Cox Analysis",
      ui.modules_pancan_unicox("modules_pancan_unicox")
    ),
    tabPanel(
      "TCGA Gene Expression vs Immune Correlation",
      ui.modules_pancan_immune("modules_pancan_immune")
    ),
    tabPanel(
      "TCGA Gene Expression vs TIL",
      ui.modules_pancan_til("modules_pancan_til")
    ),
    tabPanel(
      "TCGA Survival Analysis",
      ui.modules_sur_plot("modules_sur_plot")
    ),
    tabPanel(
      "TCGA Gene-Gene Correlation",
      ui.modules_pancan_gene_cor("modules_pancan_gene_cor")
    ),
    tabPanel(
      "CCLE Gene Expression Distribution",
      ui.modules_ccle_dist("modules_ccle_dist")
    ),
    tabPanel(
      "CCLE Gene Expression Correlation",
      ui.modules_ccle_genecor("modules_ccle_genecor")
    )
  )
}
