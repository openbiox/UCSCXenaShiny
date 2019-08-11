ui.page_developers <- function() {
  tabPanel(
    title = "Developers",
    icon = icon("user-friends"),
    fluidPage(
      # titlePanel("Developers"),
      fluidRow(
        column(
          12,
          tags$div(
            class = "card-deck text-center block-center",
            tags$div(
              class = "card bg-info",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars3.githubusercontent.com/u/25057508?s=400&v=4.png",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Shixiang Wang"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/ShixiangWang", class = "card-link", "See Profile")
              )
            ),
            tags$div(
              class = "card bg-warning",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars3.githubusercontent.com/u/17489298?s=400&v=4",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Fei Zhao"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/fei0810", class = "card-link", "See Profile")
              )
            ),
            tags$div(
              class = "card bg-info",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars0.githubusercontent.com/u/28949856?s=400&v=4",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Yi Xiong"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/Byronxy", class = "card-link", "See Profile")
              )
            )
          ),
          tags$br(),
          tags$div(
            class = "card-deck text-center block-center",
            tags$div(
              class = "card bg-warning",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars2.githubusercontent.com/u/37660840?s=400&v=4",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Longfei Zhao"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/longfei8533", class = "card-link", "See Profile")
              )
            ),
            tags$div(
              class = "card bg-info",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars0.githubusercontent.com/u/38618580?s=400&v=4",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Yin Li"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/yinlisssss", class = "card-link", "See Profile")
              )
            ),
            tags$div(
              class = "card bg-warning",
              tags$div(
                class = "card-body",
                tags$img(
                  src = "https://avatars2.githubusercontent.com/u/22772592?s=400&v=4",
                  class = "img-circle",
                  alt = "logo", style = "width: 30% ; margin: 0 auto "
                ),
                tags$h4(class = "card-title", "Kai Gu"),
                tags$p(class = "card-text", "Core developer")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "https://github.com/kaigu1990", class = "card-link", "See Profile")
              )
            )
          )
        )
      )
    )
  )
}

# server.page_developers <- function(input, output, session) {
#
# }
