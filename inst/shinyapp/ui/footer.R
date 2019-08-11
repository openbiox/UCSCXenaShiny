ui.footer <- function() {
  tags$footer(HTML("Copyright &copy; 2019&nbsp;&nbsp;&nbsp;&nbsp;"),
    tags$a(href = "https://github.com/openbiox", "Openbiox"),
    HTML("- A community-driven bioinformatics innovation collaboration group in China |"),
    tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "MIT License"),
    align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
  )
}
