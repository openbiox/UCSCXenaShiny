ui.page_help <- function() {
  md_prefix <- system.file("shinyapp/shiny-doc",  package = "UCSCXenaShiny")
  set_md_path <- function(x) {
    file.path(md_prefix, x)
  }
  
  navbarMenu(
    title = "Help",
    icon = icon("question-circle"),
    tabPanel(
      "Citation",
      fluidPage(
        includeMarkdown(set_md_path("citation.md"))
      )),
    tabPanel(
      "Term List",
      fluidPage(
        includeMarkdown(set_md_path("terms.md"))
      )),
    tabPanel(
      "Home page",
      fluidPage(
        includeMarkdown(set_md_path("home-page.md"))
      )),
    tabPanel(
      "Repository page",
      fluidPage(
        includeMarkdown(set_md_path("repository-page.md"))
      )),
    tabPanel(
      "General analysis page",
      fluidPage(
        includeMarkdown(set_md_path("general-analysis-page.md"))
      )),
    tabPanel(
      "Pan-Cancer analysis page",
      fluidPage(
        includeMarkdown(set_md_path("pancan-analysis-page.md"))
      )),
    tabPanel(
      "Global setting page",
      fluidPage(
        includeMarkdown(set_md_path("global-setting-page.md"))
      ))
  )
}
