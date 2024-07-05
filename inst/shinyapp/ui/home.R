home_text.list = list(
  intro = paste0(
    "Thank you for using UCSCXenaShiny v",packageVersion("UCSCXenaShiny")," based on ","UCSCXenaTools v", packageVersion("UCSCXenaTools"),
    ". Our web tool aims to povide a user-friendly platform to explore UCSC Xena datasets for both general and personalized cancer molecular research.",
    " If you have any questions during use, please do not hesitate to contact us via Github issue.",
    " If the tool has faciliated your research, welcome to cite our work.  :)"
  )
)


ui.page_home <- function() {
  tabPanel(
    title = "Home",
    icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    fluidPage(
      # useWaiter(), 
      # waiterPreloader(html = tagList(
      #   spin_fading_circles(), 
      #   br(), br(),
      #   h1(strong("Welcome to use UCSCXenaShiny v2 application!")),
      #   br(),
      #   p("An interactive web tool with general and personalized modules to explore UCSC Xena datasets"
      #     ,style = "font-size: 25px;"),
      #   br(),br(),
      #   p("Notes:", "(1) The initiation could take about 10 seconds. (2) Please zoom in or up screen for better representation.",
      #     style = "font-size: 16px;")
      # ), color = "#2C3E50"),
      useShinydashboard2(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
      ),
      fluidRow(
        column(4,
          tags$div(
            column(
              12,
              tags$h2("Welcome to UCSCXenaShiny v2!"),
              tags$p(home_text.list$intro, style = "font-size: 19px;"),
              fluidRow(
                column(4, align = "center",
                  actionBttn("bt01","Github",
                    style = "bordered", color = "primary", icon = icon("github"),
                    onclick=paste0("window.open('https://github.com/openbiox/UCSCXenaShiny','_blank')"))
                ),
                column(4, align = "center",
                  actionBttn("bt02","Tutorial",
                    style = "bordered", color = "primary", icon = icon("book"),
                    onclick=paste0("window.open('https://lishensuo.github.io/UCSCXenaShiny_Book','_blank')"))
                ),
                column(4, align = "center",
                  actionBttn("bt03","Article",
                    style = "bordered", color = "primary", icon = icon("newspaper"))
                ),
              ),
              uiOutput("citation"),
              tags$hr(),
            )
          )
        ),
        column(4,
          wellPanel(
            style = "background: #b3cde3",
            h3("Daily Gene", icon = "dice", align = "center"),
            ui.home_daily_gene("homepage_daily_gene"),
          )
        ),
        column(4,
          wellPanel(
            style = "background: #a6cee3",
            h3("Pan-Cancer Query", align = "center"),
            # br(),
            ui.home_search_box("homepage_pancan_search"),
          )
        ),
      ),
      tags$br(),
      h2(strong("※ Shiny Page Gallery")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(
          12,
          slickR::slickROutput("slick_output", width = "90%", height = "700px")
        )
      ),
      br(),br(),
      h2(strong("※ Example TCGA Modules: Quick analysis with easy steps")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(
          4,
          shinydashboard::box(
            title = strong("Module: Tumor and Normal Comparison",style = "font-size: 23px;"), 
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Compare one multi-omics molecular value between ",strong("tumor and normal")," (including GTEx) samples.",
              style = "font-size: 25px;"),
            actionLink("link_to_q1_n", p("Go >>>",style = "font-size: 30px;")),
          ),
        ),
        column(
          4,
          shinydashboard::box(
            title = strong("Module: Molecule-Molecule Correlation", style = "font-size: 23px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation of ",strong("two multi-omics molecules")," acccording to their values in tumor samples.",
              style = "font-size: 25px;"),
            actionLink("link_to_q2_n", p("Go >>>",style = "font-size: 30px;")),
          ),
        ),
        column(
          4,
          shinydashboard::box(
            title = strong("Module: Kaplan-Meier Survival Analysis", style = "font-size: 23px;"),
            solidHeader = TRUE, status="info",
            width = 12, height = 250,
            p("Perform the ",strong("log-rank test")," analysis of one multi-omics molecule in tumor samples of one cancer type.",
              style = "font-size: 25px;"),
            actionLink("link_to_q3_n", p("Go >>>",style = "font-size: 30px;")),
          )
        ),
      ),
      br(),
      h2(strong("※ Example TCGA pipelines: In-depth analysis with personalized steps")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(
          4,
          shinydashboard::box(
            title = strong("Pipelines: Comparison Analysis", style = "font-size: 23px;"),
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Compare one of integrated identifiers (e.g. omis, non-omics, user-defined) based on two customizable groups of samples.",
              style = "font-size: 25px;"),
            actionLink("link_to_p1_n", p("Go >>>",style = "font-size: 30px;")),
          )
        ),
        column(
          4,
          shinydashboard::box(
            title = strong("Pipelines: Correlation Analysis", style = "font-size: 23px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation between any two of integrated identifiers (e.g. omis, non-omics, user-defined) among custom samples",
              style = "font-size: 25px;"),
            actionLink("link_to_p2_n", p("Go >>>",style = "font-size: 30px;")),
          )
        ),
        column(
          4,
          shinydashboard::box(
            title = strong("Pipelines: Survival Analysis", style = "font-size: 23px;"),
            solidHeader = TRUE, status="info",
            width = 12, height = 250,
            p("Perform log-rank test analysis for one of integrated identifiers based on two customizable groups of samples.",
              style = "font-size: 25px;"),
            actionLink("link_to_p3_n", p("Go >>>",style = "font-size: 30px;")),
          )
        )
      ),
      
      br(),
      h2(strong("※ Latest significant release notes")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(12, #offset = 1,
          tags$ul(
            tags$li("2024-06-25: Design gene and pathway cross-omics analysis",style = "font-size: 20px;"),
            tags$li("2024-02-14: Incorporate the PharmacoGenomics analysis modules",style = "font-size: 20px;"),
            tags$li("2024-01-21: Adjust homepge with slick gallery to show basic page help.",style = "font-size: 20px;"),
            tags$li("2024-01-16: Introduce MSigDB genesets for molecule batch analysis.",style = "font-size: 20px;"),
            tags$li("2023-12-20: Add download modules that support data requisition.",style = "font-size: 20px;"),
            tags$li("See more update logs in our", 
              a("Github", href="https://github.com/openbiox/UCSCXenaShiny"), ".",
              style = "font-size: 20px;")
          )  
        )
      )
    )
  )
}


