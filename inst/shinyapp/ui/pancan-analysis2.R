ui.page_pancan2 <- function() {
  navbarMenu(
    title = "Pan-Can Analysis",
    icon = icon("buromobelexperte"),
    tabPanel(
      "Correlation Analysis",
      ui.modules_pancan_cor("modules_pancan_cor")
    ),
    tabPanel(
      "Grouping Comparison",
      ui.modules_pancan_comp("modules_pancan_comp")
    ),
    tabPanel(
      "Survival Analysis",
      ui.modules_pancan_sur("modules_pancan_sur")
    ),
    tabPanel(
      "Test Analysis",
      ui.modules_pancan_test("modules_pancan_test")
    )
  )
}
