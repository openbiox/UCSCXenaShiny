ui.footer <- function() {
  tags$footer(tags$hr(),
    HTML("Copyright &copy; 2019 - "),
    tags$a(href = "https://github.com/openbiox", "Openbiox"),
    tags$br(),
    HTML("A community-driven bioinformatics innovation collaboration group in China |"),
    tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "GPLv3 License"),
    align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
  )
}
