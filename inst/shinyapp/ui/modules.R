ui.page_modules <- function() {
  navbarMenu(
    title = "Modules",
    icon = icon("buromobelexperte"),
    tabPanel("Import & Export"),
    tabPanel("Data Tidy"),
    tabPanel("General Analysis"),
    tabPanel("Genomic Analysis"),
    tabPanel(
      "Visualization"
      # ,
      # fluidPage(
      #   fluidRow(
      #     helpText("The data query may take a long time, please be patient..."),
      #     column(
      #       12,
      #       tabPanel("Gene Pan-cancer Expression",
      #                plotOutput("vis_toil_gene"))
      #     )
      #   )
      # )
    )
  )
}
