ui.page_pancan_tcga <- function() {
  navbarMenu(
    tags$style(
      '[data-value = "One analysis for one cancer"] {
        width: 400px;
       background-color: #bdbdbd;
      }
       [data-value = "One analysis for many cancers"] {
        width: 400px;
       background-color: #525252;
      }
       [data-value = "Batch analysis for one cancer"] {
        width: 400px;
       background-color: #525252;
      }
      .tab-pane {
        background-color: transparent;
        width: 100%;
        }
      '
    ),
    title = "TCGA Analysis",
    icon = icon("buromobelexperte"),
    tabPanel(
      "TCGA Association Analysis",
      h1("TCGA Association Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "One analysis for one cancer",
          br(),
          ui.modules_pancan_cor_o2o("modules_pancan_cor_o2o")
        ),
        tabPanel(
          "One analysis for many cancers",
          br(),
          ui.modules_pancan_cor_o2m("modules_pancan_cor_o2m")
        ),
        tabPanel(
          # "Multiple items analysis(one cancer)",
          "Batch analysis for one cancer",
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
          "One analysis for one cancer",
          br(),
          ui.modules_pancan_comp_o2o("modules_pancan_comp_o2o")
        ),
        tabPanel(
          "One analysis for many cancers",
          br(),
          ui.modules_pancan_comp_o2m("modules_pancan_comp_o2m")
        ),
        tabPanel(
          "Batch analysis for one cancer",
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
          "One analysis for one cancer",
          br(),
          ui.modules_pancan_sur_o2o("modules_pancan_sur_o2o")
        ),
        tabPanel(
          "One analysis for many cancers",
          br(),
          ui.modules_pancan_sur_o2m("modules_pancan_sur_o2m")
        ),
        tabPanel(
          "Batch analysis for one cancer",
          br(),
          ui.modules_pancan_sur_m2o("modules_pancan_sur_m2o")
        )
      )
    )
  )
}
