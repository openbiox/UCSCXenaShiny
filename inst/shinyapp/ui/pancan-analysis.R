ui.page_pancan <- function() {
  navbarMenu(
    title = "Quick PanCan Analysis",
    icon = icon("buromobelexperte"),
    # tabPanel(
    #   "Combo: Single Gene Analysis",
    #   ui.combo.sg.pancan.analysis("combo.sg.pancan.analysis")
    # ),
    tabPanel(
      "TCGA: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    tabPanel(
      "TCGA: Molecular Profile Distribution (Tumor VS Normal)",
      ui.modules_cancer_dist("modules_cancer_dist")
    ),
    tabPanel(
      "TCGA: Molecule-Molecule Correlation",
      ui.modules_pancan_gene_cor("modules_pancan_gene_cor")
    ),
    tabPanel(
      "TCGA: Molecular Profile Anatomy",
      ui.modules_pancan_anatomy("modules_pancan_anatomy")
    ),
    tabPanel(
      "TCGA: Molecular Profile Cox Analysis",
      ui.modules_pancan_unicox("modules_pancan_unicox")
    ),
    tabPanel(
      "TCGA: Association Between Molecular Profile and Immune Signature",
      ui.modules_pancan_immune("modules_pancan_immune")
    ),
    tabPanel(
      "TCGA: Association Between Molecular Profile and Tumor Immune Infiltration",
      ui.modules_pancan_til("modules_pancan_til")
    ),
    tabPanel(
      "TCGA: Association Between Molecular Profile and TMB/Stemness/MSI (Radar Show)",
      ui.modules_pancan_radar("modules_pancan_radar")
    ),
    tabPanel(
      "TCGA: Survival Analysis",
      ui.modules_sur_plot("modules_sur_plot")
    ),
    tabPanel(
      "CCLE: Molecular Profile Distribution Across Cancer Primary Sites",
      ui.modules_ccle_dist("modules_ccle_dist")
    ),
    tabPanel(
      "CCLE: Molecule-Molecule Correlation",
      ui.modules_ccle_genecor("modules_ccle_genecor")
    ),
    tabPanel(
      "CCLE: Drug-Target Correlation",
      ui.modules_ccle_drug_target_asso("modules_ccle_drug_target_asso")
    )
  )
}
