ui.footer <- function() {
  tags$footer(tags$hr(),
    HTML("Copyright &copy; 2019 - 2024 @"),
    tags$a(href = "https://github.com/openbiox", "Openbiox"),
    tags$br(),
    HTML("A community-driven bioinformatics innovation collaboration group in China |"),
    tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "GPLv3 License"),
    HTML("<br><a href='https://clustrmaps.com/site/1c2kd'  title='Visit tracker'><img src='//clustrmaps.com/map_v2.png?cl=ffffff&w=a&t=tt&d=09ckOL1huqv48licSGJ9K7oM8wHMiOO6WxNZDLesR10'/></a>"),
    align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
  )
}
