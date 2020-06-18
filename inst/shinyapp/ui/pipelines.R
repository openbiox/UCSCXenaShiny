ui.page_pipelines <- function() {
  navbarMenu(
    title = "Pipelines",
    icon = icon("angle-double-down"),
    tabPanel("single gene pan-cancer analysis",
             ui.sg.pancan.analysis("sg.pancan.analysis"))
  )
}
