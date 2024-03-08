ui.page_pancan_quick <- function() {
  navbarMenu(
    title = "Custom T·P·C Modules",
    icon = icon("buromobelexperte"),
    # tabPanel(
    #   "Combo: Single Gene Analysis",
    #   ui.combo.sg.pancan.analysis("combo.sg.pancan.analysis")
    # ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)",
      ui.modules_pancan_dist("module_gene_pancan_dist")
    ),
    # tabPanel(
    #   "TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)",
    #   ui.modules_cancer_dist("modules_cancer_dist")
    # ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Anatomy",
      ui.modules_pancan_anatomy("modules_pancan_anatomy")
    ),
    tabPanel(
      "TCGA: Molecule-Molecule Correlation",
      ui.modules_pancan_gene_cor("modules_pancan_gene_cor")
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
        "TCGA: Association Between Molecular Profile and Pathway Score",
        ui.modules_pw_cor("modules_pw_cor")
    ),
    tabPanel(
        "TCGA: Association Between Molecular Profile and Gene Mutation",
        ui.modules_pancan_mut("modules_pancan_mut")
    ),
    tabPanel(
      "TCGA: Molecular Profile Kaplan-Meier Analysis",
      ui.modules_sur_plot("modules_sur_plot")
    ),
    tabPanel(
      "TCGA: Molecular Profile Cox Regression Analysis",
      ui.modules_pancan_unicox("modules_pancan_unicox")
    ),
    tabPanel(
        "TCGA: Dimension Reduction Distribution",
        ui.modules_dim_dist("modules_dim_dist")
    ),

    tabPanel(
      "PCAWG: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)",
      ui.modules_pcawg_dist("modules_pcawg_dist")
    ),
    tabPanel(
      "PCAWG: Molecule-Molecule Correlation",
      ui.modules_pcawg_gene_cor("modules_pcawg_gene_cor")
    ),
    tabPanel(
      "PCAWG: Molecular Profile Kaplan-Meier Analysis",
      ui.modules_pcawg_sur_plot("modules_pcawg_sur_plot")
    ),
    tabPanel(
      "PCAWG: Molecular Profile Cox Regression Analysis",
      ui.modules_pcawg_unicox("modules_pcawg_unicox")
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
    )
  )
}
