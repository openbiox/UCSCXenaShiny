ui.page_help <- function() {

  navbarMenu(
    title = "Help",
    icon = icon("question-circle"),
    # tabPanel(
    #   "Usage",
    #   fluidPage(
    #     includeMarkdown(set_md_path("usage.md"))
    #   )
    # ),
    tabPanel(
      "Quick Guide",
      fluidPage(
        tags$iframe(style="height:1000px; width:100%", src="Quick_guide.pdf")
        # tags$iframe(style="height:1000px; width:100%", src="https://github.com/lishensuo/utils/blob/main/Quick_guide.pdf")
      )
    ),
    tabPanel(title = a("Tutorial Book", 
               href="https://lishensuo.github.io/UCSCXenaShiny_Book/",
               target="_blank")),
    tabPanel(
      "TPC ID Query",
      ui.modules_id_reference("modules_id_reference")
    ),

    # tabPanel(a("Package doc", 
    #            href="https://openbiox.github.io/UCSCXenaShiny/index.html",
    #            target="_blank")),
    tabPanel(
      "Citation",
      fluidPage(
        includeMarkdown(set_md_path("citation2.md"))
      )
    ),
    # tabPanel(
    #   "Extra Datasets",
    #   fluidPage(
    #     includeMarkdown(set_md_path("datasets.md"))
    #   )
    # ),
    tabPanel(a("UCSCXenaShiny v1", 
               href="https://shixiangwang.shinyapps.io/ucscxenashiny/",
               target="_blank")
    )
  )
}
