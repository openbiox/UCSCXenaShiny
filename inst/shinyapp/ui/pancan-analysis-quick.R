ui.page_pancan_quick <- function() {
  navbarMenu(
    title = "Custom T·P·C Modules",
    icon = icon("buromobelexperte"),

    tabPanel(
      "TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)",
      ui.modules_1_tcga_01("modules_1_tcga_01")
    ),
    tabPanel(
      "TCGA+GTEx: Molecular Profile Anatomy",
      ui.modules_1_tcga_02("modules_1_tcga_02")
    ),
    tabPanel(
      "TCGA: Molecule-Molecule Correlation",
      ui.modules_1_tcga_03("modules_1_tcga_03")
    ),
    tabPanel(
      "TCGA: Association Between Molecular Profile and Tumor Immune Infiltration",
      ui.modules_1_tcga_04("modules_1_tcga_04")
    ),
    tabPanel(
      "TCGA: Association Between Molecular Profile and Immune Signature",
      ui.modules_1_tcga_05("modules_1_tcga_05")
    ),

    tabPanel(
      "TCGA: Association Between Molecular Profile and TMB/Stemness/MSI (Radar Show)",
      ui.modules_1_tcga_06("modules_1_tcga_06")
    ),

    tabPanel(
      "TCGA: Association Between Molecular Profile and Pathway Score",
      ui.modules_1_tcga_07("modules_1_tcga_07")
    ),

    tabPanel(
      "TCGA: Association Between Molecular Profile and Gene Mutation",
      ui.modules_1_tcga_08("modules_1_tcga_08")
    ),

    tabPanel(
      "TCGA: Molecular Profile Kaplan-Meier Analysis",
      ui.modules_1_tcga_09("modules_1_tcga_09")
    ),

    tabPanel(
      "TCGA: Molecular Profile Cox Regression Analysis",
      ui.modules_1_tcga_10("modules_1_tcga_10")
    ),

    tabPanel(
      "TCGA: Dimension Reduction Distribution",
      ui.modules_1_tcga_11("modules_1_tcga_11")
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
