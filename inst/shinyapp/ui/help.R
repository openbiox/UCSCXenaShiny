ui.page_help <- function() {
  navbarMenu(
    title = "Help",
    # icon = icon("question-circle"),
    # tabPanel(
    #   "News",
    #   fluidPage(
    #     includeMarkdown(system.file("NEWS.md", package = "UCSCXenaShiny", mustWork = TRUE))
    #   )
    # ),
    tabPanel(
      "Usages",
      fluidPage(
        includeMarkdown(system.file("shinyapp/shiny-doc", "usage.md", package = "UCSCXenaShiny", mustWork = TRUE))
      )),
    tabPanel(
      "Term List",
      fluidPage(
        includeMarkdown(system.file("shinyapp/shiny-doc", "terms.md", package = "UCSCXenaShiny", mustWork = TRUE))
      ))
  )
}
