ui.page_pancan_quick <- function() {
  navbarMenu(
    title = "T·P·C Modules",
    icon = icon("buromobelexperte"),

    tabPanel(
      "Introduction",
      fluidPage(
        includeMarkdown(set_md_path("intro_quick_mods.md"))
      )
    ),

    tabPanel("TCGA (GTEx): Molecular comparison",
      tabsetPanel(
        tabPanel(
          "Tumor VS Normal (Box plot)",
          ui.modules_1_tcga_01("modules_1_tcga_01")
        ),
        tabPanel(
          "Tumor VS Normal (Anatomy plot)",
          ui.modules_1_tcga_02("modules_1_tcga_02")
        ),
        tabPanel(
          "Mutation VS Wild",
          ui.modules_1_tcga_08("modules_1_tcga_08")
        ),
      )
    ),

    tabPanel("TCGA: Molecular correlation",
      tabsetPanel(
        tabPanel(
          "Molecule-Molecule",
          ui.modules_1_tcga_03("modules_1_tcga_03")
        ),
        tabPanel(
          "Molecule-Tumor Immune Infiltration",
          ui.modules_1_tcga_04("modules_1_tcga_04")
        ),
        tabPanel(
          "Molecule-Immune Signature",
          ui.modules_1_tcga_05("modules_1_tcga_05")
        ),
        tabPanel(
          "Molecule-TMB/Stemness/MSI",
          ui.modules_1_tcga_06("modules_1_tcga_06")
        ),
        tabPanel(
          "Molecule-Pathway",
          ui.modules_1_tcga_07("modules_1_tcga_07")
        ),
      )
    ),

    tabPanel("TCGA: Survival analysis",
      tabsetPanel(
        tabPanel(
          "Kaplan-Meier",
          ui.modules_1_tcga_09("modules_1_tcga_09")
        ),
        tabPanel(
          "Cox regression",
          ui.modules_1_tcga_10("modules_1_tcga_10")
        )
      )
    ),

    tabPanel("TCGA: Dimensionality reduction",
      tabsetPanel(
        tabPanel(
          "Dimensionality reduction",
          ui.modules_1_tcga_11("modules_1_tcga_11")
        )
      )
    ),

    tabPanel("PCAWG: Molecular comparison",
      tabsetPanel(
        tabPanel(
          "Tumor VS Normal",
          ui.modules_2_pcawg_01("modules_2_pcawg_01")
        )
      )
    ),

    tabPanel("PCAWG: Molecular correlation",
      tabsetPanel(
        tabPanel(
          "Molecule-Molecule",
          ui.modules_2_pcawg_02("modules_2_pcawg_02")
        )
      )
    ),

    tabPanel("PCAWG: Survival analysis",
      tabsetPanel(
        tabPanel(
          "Kaplan-Meier",
          ui.modules_2_pcawg_03("modules_2_pcawg_03")
        ),
        tabPanel(
          "Cox regression",
          ui.modules_2_pcawg_04("modules_2_pcawg_04")
        )
      )
    ),

    tabPanel("CCLE: Molecular comparison",
      tabsetPanel(
        tabPanel(
          "Cancer Primary Sites",
          ui.modules_3_ccle_01("modules_3_ccle_01")
        )
      )
    ),

    tabPanel("CCLE: Molecular correlation",
      tabsetPanel(
        tabPanel(
          "Molecule-Molecule",
          ui.modules_3_ccle_02("modules_3_ccle_02")
        )
      )
    ),

    tabPanel("CCLE: Drug analysis",
      tabsetPanel(
        tabPanel(
          "Drug Response Association",
          ui.modules_3_ccle_03("modules_3_ccle_03")
        ),
        tabPanel(
          "Drug Response Difference",
          ui.modules_3_ccle_04("modules_3_ccle_04")
        )
      )
    )
  )
}
