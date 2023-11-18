ui.page_pancan_tcga <- function() {
  navbarMenu(
    title = "TCGA Analysis",
    icon = icon("buromobelexperte"),
    tags$style(
      '[data-value = "Single item analysis(one/pan-cancers)"] {
        width: 500px;
       background-color: #bdbdbd;
      }

       [data-value = "Multiple items analysis(one cancer)"] {
        width: 500px;
       background-color: #525252;
      }
      .tab-pane {
        background-color: transparent;
        width: 100%;
        }
      '
    ),
    tabPanel(
      "TCGA Association Analysis",
      h1("TCGA Association Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Single item analysis(one/pan-cancers)",
          br(),
          ui.modules_pancan_cor("modules_pancan_cor")
        ),
        tabPanel(
          "Multiple items analysis(one cancer)",
          br(),
          ui.modules_pancan_cor_batch("modules_pancan_cor_batch")
        )
      )
    ),
    tabPanel(
      "TCGA Comparison Analysis",
      h1("TCGA Comparison Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Single item analysis(one/pan-cancers)",
          br(),
          ui.modules_pancan_comp("modules_pancan_comp")
        ),
        tabPanel(
          "Multiple items analysis(one cancer)",
          br(),
          ui.modules_pancan_comp_batch("modules_pancan_comp_batch")
        )
      )
    ),
    tabPanel(
      "TCGA Survival Analysis",
      h1("TCGA Survival Analysis", align = "center"),
      tabsetPanel(
        tabPanel(
          "Single item analysis(one/pan-cancers)",
          br(),
          ui.modules_pancan_sur("modules_pancan_sur")
        ),
        tabPanel(
          "Multiple items analysis(one cancer)",
          br(),
          ui.modules_pancan_sur_batch("modules_pancan_sur_batch")
        )
      )
    )
  )
}
