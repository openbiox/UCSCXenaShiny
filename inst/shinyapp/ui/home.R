ui.page_home <- function() {
  tabPanel(
    title = "Home",
    icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
      ),
      fluidRow(
        column(5,
          tags$div(
            column(
              12,
              tags$h2("Welcome to UCSCXenaShiny v2!"),
              tags$p(paste0(
                "UCSCXenaShiny v", packageVersion("UCSCXenaShiny"),
                " based on ",
                "UCSCXenaTools v", packageVersion("UCSCXenaTools"),
                " (We are still working on XenaShiny v2, so the app may be not stable. Any question please report to the Github issue.)"
              ),style = "font-size: 20px;"),
              tags$br(),
              # tags$b("Data source: "),
              # tags$a(href = "https://xenabrowser.net/datapages/", "UCSC Xena"),
              # " or ",
              # tags$a(href = "https://xena.hiplot.com.cn/datapages/", "Hiplot mirror"),
              tags$p(paste0("Generally, UCSCXenaShiny v2.0 is a significant advancement that expands ",
                "upon the capabilities of its initial version, allowing for personalized ",
                "cancer omics analysis. We are confident that this update will address the ",
                "limitations of existing web tools for cancer-related omics analysis, ",
                "benefiting researchers and clinicians seeking a deeper understanding of molecular features."),
                style = "font-size: 18px;"),
              tags$hr(),
            )
          )
        ),
        column(4,
          wellPanel(
            style = "background: #a6cee3",
            h2("TCGA Pan-Cancer Query!", align = "center"),
            # br(),
            ui.home_search_box("homepage_pancan_search"),
          )
        ),
        column(3,
          wellPanel(
            style = "background: #b3cde3",
            h2("Latest Release Notes", align = "center"),
            br(),
            tags$ul(
              tags$li("2024-01-21: Adjust homepge with slick gallery to show basic page help.",style = "font-size: 14px;"),
              tags$li("2024-01-16: Introduce MSigDB genesets  for molecule batch analysis.",style = "font-size: 14px;"),
              tags$li("2023-12-20: Add  download modules that support data requisition.",style = "font-size: 14px;"),
              tags$li("See more update logs in", 
                a("Github", href="https://github.com/openbiox/UCSCXenaShiny"),
                style = "font-size: 16px;")
            )
          )
        )
      ),
      tags$br(),
      fluidRow(
        column(
          12,
          slickR::slickROutput("slick_output", width = "90%", height = "700px")
        )
      ),
      # fluidRow(
      #   column(
      #     6,
      #     tags$div(
      #       column(
      #         12,
      #         ui.home_search_box("homepage_pancan_search"),
      #         tags$h2("Data Portal Summary"),
      #         tags$b(paste0(
      #           "UCSCXenaShiny v", packageVersion("UCSCXenaShiny"),
      #           " based on ",
      #           "UCSCXenaTools v", packageVersion("UCSCXenaTools"),
      #           " (We are working on XenaShiny v2, so the app may be not stable. Any question please report to the Github issue.)"
      #         )),
      #         tags$br(),
      #         tags$b("Data source: "),
      #         tags$a(href = "https://xenabrowser.net/datapages/", "UCSC Xena"),
      #         " or ",
      #         tags$a(href = "https://xena.hiplot.com.cn/datapages/", "Hiplot mirror"),
      #         tags$hr(),
      #         tags$div(
      #           class = "card-deck text-center block-center",
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               class = "card-body",
      #               tags$img(src = "host.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               tags$h5("DATA HUBS")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Data_hubs_number)
      #             )
      #           ),
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               tags$img(src = "cohort.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               class = "card-body",
      #               tags$h5("COHORTS")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Cohorts_number)
      #             )
      #           ),
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               class = "card-body",
      #               tags$img(src = "dataset.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               tags$h5("DATASETS")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Datasets_number)
      #             )
      #           )
      #         ),
      #         tags$br(),
      #         tags$div(
      #           class = "card-deck text-center block-center",
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               class = "card-body",
      #               tags$img(src = "sample.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               tags$h5("SAMPLES")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Samples_number)
      #             )
      #           ),
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               class = "card-body",
      #               tags$img(src = "site.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               tags$h5("PRIMARY SITES")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Primary_sites_number)
      #             )
      #           ),
      #           tags$div(
      #             class = "card",
      #             tags$div(
      #               class = "card-body",
      #               tags$img(src = "file.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
      #               tags$h5("DATA SUBTYPES")
      #             ),
      #             tags$div(
      #               class = "card-footer",
      #               tags$h4(Data_subtypes_number)
      #             )
      #           )
      #         )
      #       )
      #     )
      #   ),
      #   column(
      #     6,
      #     tags$br(),
      #     tags$br(),
      #     tags$br(),
      #     tags$br(),
      #     tags$br(),
      #     tags$br(),
      #     tabsetPanel(
      #       tabPanel(
      #         "Cohort number",
      #         tags$br(),
      #         # plotly::plotlyOutput("Xenasummary1", height = "100%")
      #         plotOutput("Xenasummary1")

      #       ),
      #       tabPanel(
      #         "Dataset number",
      #         tags$br(),
      #         # plotly::plotlyOutput("Xenasummary2", height = "100%")
      #         plotOutput("Xenasummary2")

      #       )
      #     )
      #   )
      # ),

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
