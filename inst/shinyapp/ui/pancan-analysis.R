ui.page_pancan <- function() {
  navbarMenu(
    title = "Quick PanCan Analysis",
    icon = icon("buromobelexperte"),
    # tabPanel(
    #   "Combo: Single Gene Analysis",
    #   ui.combo.sg.pancan.analysis("combo.sg.pancan.analysis")
    # ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)",
      ui.modules_cancer_dist("modules_cancer_dist")
    ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Anatomy",
      ui.modules_pancan_anatomy("modules_pancan_anatomy")
    ),
    tabPanel(
      "TCGA: Molecule-Molecule Correlation",
      ui.modules_pancan_gene_cor("modules_pancan_gene_cor")
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
      "CCLE: Drug Response Association",
      ui.modules_ccle_drug_target_asso("modules_ccle_drug_target_asso")
    ),
    tabPanel(
      "CCLE: Drug Response Difference",
      ui.modules_ccle_drug_response_diff("modules_ccle_drug_response_diff")
    ),
    tabPanel(
      "PCAWG: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)",
      ui.modules_pcawg_dist("modules_pcawg_dist")
    ),
    tabPanel(
      "PCAWG: Molecular Profile Cox Analysis",
      ui.modules_pcawg_unicox("modules_pcawg_unicox")
    ),
    tabPanel(
      "PCAWG: Molecule-Molecule Correlation",
      ui.modules_pcawg_gene_cor("modules_pcawg_gene_cor")
    ),
    tabPanel(
      "PCAWG: Survival Analysis",
      ui.modules_pcawg_sur_plot("modules_pcawg_sur_plot")
    )
  )
}
