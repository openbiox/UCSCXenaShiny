ui.page_pancan_tcga <- function() {
  navbarMenu(
    title = "Personalized T·P·C Pipelines",
    icon = icon("buromobelexperte"),
    #### TCGA
    tabPanel(
      "TCGA: Association Analysis",
      h1("TCGA Association Analysis", align = "center"),
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
    #### PCAWG
    tabPanel(
      "PCAWG: Association Analysis",
      h1("PCAWG Association Analysis", align = "center"),
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
      "CCLE: Association Analysis",
        h1("CCLE Association Analysis", align = "center"),
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
