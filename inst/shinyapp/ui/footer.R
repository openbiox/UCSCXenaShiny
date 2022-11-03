ui.footer <- function() {
  tags$footer(tags$hr(),
    HTML("Copyright &copy; 2019 - "),
    tags$a(href = "https://github.com/openbiox", "Openbiox"),
    tags$br(),
    HTML("A community-driven bioinformatics innovation collaboration group in China |"),
    tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "GPLv3 License"),
    HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/8.js?i=5arxfkv3006&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;f=lucida_console&amp;l=33" async="async"></script>'),
    align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
  )
}
