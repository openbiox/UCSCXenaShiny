ui.page_help <- function() {
  navbarMenu(
    title = "Help",
    icon = icon("question-circle"),
    tabPanel(
      "News",
      fluidPage(
        includeMarkdown(system.file("NEWS.md", package = "UCSCXenaShiny", mustWork = TRUE))
      )
    ),
    tabPanel("Usages"),
    tabPanel("Term List")
  )
}
