ui.page_home <- function() {
  tabPanel(
    title = "Home",
    icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
      ),
      fluidRow(
        column(
          6,
          tags$div(
            column(
              12,
              ui.home_search_box("homepage_pancan_search"),
              tags$h2("Data Portal Summary"),
              tags$b(paste0(
                "UCSCXenaShiny v", packageVersion("UCSCXenaShiny"),
                " based on ",
                "UCSCXenaTools v", packageVersion("UCSCXenaTools")
              )),
              tags$br(),
              tags$b("Data source: "),
              tags$a(href = "https://xenabrowser.net/datapages/", "UCSC Xena"),
              " or ",
              tags$a(href = "https://xena.hiplot.com.cn/datapages/", "Hiplot mirror"),
              tags$hr(),
              tags$div(
                class = "card-deck text-center block-center",
                tags$div(
                  class = "card",
                  tags$div(
                    class = "card-body",
                    tags$img(src = "host.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    tags$h5("DATA HUBS")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Data_hubs_number)
                  )
                ),
                tags$div(
                  class = "card",
                  tags$div(
                    tags$img(src = "cohort.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    class = "card-body",
                    tags$h5("COHORTS")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Cohorts_number)
                  )
                ),
                tags$div(
                  class = "card",
                  tags$div(
                    class = "card-body",
                    tags$img(src = "dataset.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    tags$h5("DATASETS")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Datasets_number)
                  )
                )
              ),
              tags$br(),
              tags$div(
                class = "card-deck text-center block-center",
                tags$div(
                  class = "card",
                  tags$div(
                    class = "card-body",
                    tags$img(src = "sample.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    tags$h5("SAMPLES")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Samples_number)
                  )
                ),
                tags$div(
                  class = "card",
                  tags$div(
                    class = "card-body",
                    tags$img(src = "site.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    tags$h5("PRIMARY SITES")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Primary_sites_number)
                  )
                ),
                tags$div(
                  class = "card",
                  tags$div(
                    class = "card-body",
                    tags$img(src = "file.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                    tags$h5("DATA SUBTYPES")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$h4(Data_subtypes_number)
                  )
                )
              )
            )
          )
        ),
        column(
          6,
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tabsetPanel(
            tabPanel(
              "Cohort number",
              tags$br(),
              plotly::plotlyOutput("Xenasummary1", height = "100%")
            ),
            tabPanel(
              "Dataset number",
              tags$br(),
              plotly::plotlyOutput("Xenasummary2", height = "100%")
            )
          )
        )
      ),
      tags$br()
      # tags$div(
      #   class = "text-center",
      #   tags$div(
      #     class = "bg-dark text-white",
      #     tags$p("The goal of XenaShiny is to provide a web app for downloading, analyzing and visualizing datasets from UCSC Xena hubs.")
      #   )
      # )
    )
  )
}
