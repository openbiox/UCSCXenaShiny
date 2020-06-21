ui.page_pipelines <- function() {
  navbarMenu(
    title = "Pipelines",
    icon = icon("angle-double-down"),
    tabPanel("Single Gene Pan-cancer Analysis",
             ui.sg.pancan.analysis("sg.pancan.analysis"))
  )
}
