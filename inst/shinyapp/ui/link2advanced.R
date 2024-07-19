ui.link2advanced <- function() {
  navbarMenu(
    title = "Advanced analysis",
    icon = icon("question-circle"),

    tabPanel(a("Custom T·P·C Modules", 
               href="http://localhost:1498/",
               target="_blank")),
    # tabPanel(a("Personalized T·P·C Pipelines", 
    #            href="https://lishensuo.github.io/UCSCXenaShiny_Book/",
    #            target="_blank")),
    # tabPanel(a("PharmacoGenomics", 
    #            href="https://lishensuo.github.io/UCSCXenaShiny_Book/",
    #            target="_blank")),
  )
}
