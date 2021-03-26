ui.page_global <- function() {
  tabPanel(
    title = "Global Setting",
    icon = icon("cogs"),
    fluidPage(
      h3("Global setting:"),
      switchInput("hiplot_switch", label = "Use hiplot mirror to query data?", labelWidth = "200px", inline = T),
      p(style = "height:500px"),
    )
  )
}
