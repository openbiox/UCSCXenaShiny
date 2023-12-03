ui.page_pancan_tcga <- function() {
  navbarMenu(
    title = "TCGA Analysis",
    icon = icon("buromobelexperte"),
    tabPanel(
      "TCGA Association Analysis",
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
      "TCGA Comparison Analysis",
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
      "TCGA Survival Analysis",
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
    )
  )
}
