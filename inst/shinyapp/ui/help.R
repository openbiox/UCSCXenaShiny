ui.page_help <- function() {
  md_prefix <- system.file("shinyapp/shiny-doc", package = "UCSCXenaShiny")
  set_md_path <- function(x) {
    file.path(md_prefix, x)
  }

  navbarMenu(
    title = "Help",
    icon = icon("question-circle"),
    tabPanel(
      "Usage",
      fluidPage(
        includeMarkdown(set_md_path("usage.md"))
      )
    ),
    tabPanel(
      "Term List",
      fluidPage(
        includeMarkdown(set_md_path("terms.md"))
      )
    ),
    tabPanel(a("Package doc", 
               href="https://openbiox.github.io/UCSCXenaShiny/index.html",
               target="_blank")),
    tabPanel(
      "Citation",
      fluidPage(
        includeMarkdown(set_md_path("citation.md"))
      )
    )
  )
}
