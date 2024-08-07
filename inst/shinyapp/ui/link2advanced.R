ui.link2advanced <- function() {
  navbarMenu(
    title = "Advanced analysis",
    icon = icon("question-circle"),

    tabPanel(a("Whole Functionalities", 
               href="https://shiny.zhoulab.ac.cn/UCSCXenaShiny/",
               target="_blank")),    

    tabPanel("Go to T·P·C Modules", 
              "To be deployed"),
    tabPanel("Go to T·P·C Pipelinnes", 
              "To be deployed"),
    tabPanel("Go to Pharmcogenomics Modules", 
              "To be deployed"),
    # tabPanel(a("T·P·C Pipelines", 
    #            href="https://lishensuo.github.io/UCSCXenaShiny_Book/",
    #            target="_blank")),
    # tabPanel(a("PharmacoGenomics", 
    #            href="https://lishensuo.github.io/UCSCXenaShiny_Book/",
    #            target="_blank")),
  )
}
