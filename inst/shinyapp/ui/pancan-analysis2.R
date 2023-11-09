ui.page_pancan2 <- function() {
  navbarMenu(
    title = "Pan-Can Analysis",
    icon = icon("buromobelexperte"),
    tabPanel(
      "TCGA Association Analysis",
      ui.modules_pancan_cor("modules_pancan_cor")
    ),
    tabPanel(
      "TCGA Comparison Analysis",
      ui.modules_pancan_comp("modules_pancan_comp")
    ),
    tabPanel(
      "TCGA Survival Analysis",
      ui.modules_pancan_sur("modules_pancan_sur")
    )
  )
}
