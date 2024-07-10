ui.page_pancan_tcga <- function() {
  navbarMenu(
    title = "T·P·C Pipelines",
    icon = icon("buromobelexperte"),
    tabPanel(
      "Introduction",
      fluidPage(
        includeMarkdown(set_md_path("intro_personal_pips.md"))
      )
    ),
    #### TCGA
    tabPanel(
      "TCGA: Correlation Analysis",
      h1("TCGA Correlation Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pancan_cor_o2o("modules_pancan_cor_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pancan_cor_o2m("modules_pancan_cor_o2m")
        ),
        tabPanel(
          # "Multiple items analysis(one cancer)",
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pancan_cor_m2o("modules_pancan_cor_m2o")
        )
      )
    ),
    tabPanel(
      "TCGA: Comparison Analysis",
      h1("TCGA Comparison Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pancan_comp_o2o("modules_pancan_comp_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pancan_comp_o2m("modules_pancan_comp_o2m")
        ),
        tabPanel(
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pancan_comp_m2o("modules_pancan_comp_m2o")
        )
      )
    ),
    tabPanel(
      "TCGA: Survival Analysis",
      h1("TCGA Survival Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pancan_sur_o2o("modules_pancan_sur_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pancan_sur_o2m("modules_pancan_sur_o2m")
        ),
        tabPanel(
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pancan_sur_m2o("modules_pancan_sur_m2o")
        )
      )
    ),
    tabPanel(
      "TCGA: Cross-Omics Analysis",
      h1("TCGA Cross-Omics Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Gene Cross-Omics Analysis",
          br(),
          ui.modules_pancan_cross_gene_o2m("modules_pancan_cross_gene_o2m")
        ),
        tabPanel(
          "Pathway Cross-Omics Analysis",
          br(),
          ui.modules_pancan_cross_pw_o2m("modules_pancan_cross_pw_o2m")
        ),
      )
    ),

    #### PCAWG
    tabPanel(
      "PCAWG: Correlation Analysis",
      h1("PCAWG Correlation Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_cor_o2o("modules_pcawg_cor_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pcawg_cor_o2m("modules_pcawg_cor_o2m")
        ),
        tabPanel(
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_cor_m2o("modules_pcawg_cor_m2o")
        )
      )
    ),
    tabPanel(
      "PCAWG: Comparison Analysis",
      h1("PCAWG Comparison Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_comp_o2o("modules_pcawg_comp_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pcawg_comp_o2m("modules_pcawg_comp_o2m")
        ),
        tabPanel(
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_comp_m2o("modules_pcawg_comp_m2o")
        )
      )
    ),
    tabPanel(
      "PCAWG: Survival Analysis",
      h1("PCAWG Survival Analysis(OS)", align = "center"),
      tabsetPanel(
        tabPanel(
          "Sole Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_sur_o2o("modules_pcawg_sur_o2o")
        ),
        tabPanel(
          "Sole Analysis for Multiple Cancers",
          br(),
          ui.modules_pcawg_sur_o2m("modules_pcawg_sur_o2m")
        ),
        tabPanel(
          "Batch Analysis for Single Cancer",
          br(),
          ui.modules_pcawg_sur_m2o("modules_pcawg_sur_m2o")
        )
      )
    ),
    #### CCLE
    tabPanel(
      "CCLE: Correlation Analysis",
        h1("CCLE Correlation Analysis", align = "center"),
        tabsetPanel(
          tabPanel(
            "Sole Analysis for Cell Lines",
            br(),
            ui.modules_ccle_cor_o2o("modules_ccle_cor_o2o")
          ),
          tabPanel(
            "Batch Analysis for Cell Lines",
            br(),
            ui.modules_ccle_cor_m2o("modules_ccle_cor_m2o")
          )
        ) 
    ),
    tabPanel(
      "CCLE: Comparison Analysis",
        h1("CCLE Comparison Analysis", align = "center"),
        tabsetPanel(
          tabPanel(
            "Sole Analysis for Cell Lines",
            br(),
            ui.modules_ccle_comp_o2o("modules_ccle_comp_o2o")
          ),
          tabPanel(
            "Batch Analysis for Cell Lines",
            br(),
            ui.modules_ccle_comp_m2o("modules_ccle_comp_m2o")
          )
        ) 
    )
  )
}
