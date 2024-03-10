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
      useWaiter(), 
      waiterPreloader(html = tagList(
        spin_fading_circles(), 
        br(), br(),
        h1(strong("Welcome to use UCSCXenaShiny v2 application!")),
        br(),
        p("An interactive web tool with general and personalized modules to explore UCSC Xena datasets"
          ,style = "font-size: 25px;"),
        br(),br(),
        p("Notes:", "(1) The initiation could take about 10 seconds. (2) Please zoom in or up screen for better representation.",
          style = "font-size: 16px;")
      ), color = "#2C3E50"),
      useShinydashboard(),
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
            h3("❤ Daily Gene", icon = "dice", align = "center"),
            ui.home_daily_gene("homepage_daily_gene"),
          )
        ),
        column(4,
          wellPanel(
            style = "background: #a6cee3",
            h3("❤ Pan-Cancer Query", align = "center"),
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
      h2(strong("※ Custom TCGA modules with specific functions")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Comparison",style = "font-size: 20px;"), 
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Compare one multi-omics molecular value between ",strong("tumor and normal")," (including GTEx) samples.",
              style = "font-size: 18px;"),
            actionLink("link_to_q1", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Comparison",style = "font-size: 20px;"), 
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Observe molecular value between tumor and normal (including GTEx) samples via ",strong("anatomy plot"),".",
              style = "font-size: 18px;"),
            actionLink("link_to_q2", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Comparison",style = "font-size: 20px;"),
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Compare one multi-omics molecular value between gene ",strong("mutation and wild")," of tumor samples.",
              style = "font-size: 18px;"),
            actionLink("link_to_q9", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation of ",strong("two multi-omics molecules")," acccording to their expression values.",
              style = "font-size: 18px;"),
            actionLink("link_to_q3", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation between one multi-omics molecule and ",strong("tumor immune infiltration")," across cancers",
              style = "font-size: 18px;"),
            actionLink("link_to_q6", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation between one multi-omics molecule and ",strong("immune signature scores")," across cancers",
              style = "font-size: 18px;"),
            actionLink("link_to_q5", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
      ),
      fluidRow(
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation between one multi-omics molecule and ",strong("TMB/Stemness/MSI")," across cancers",
              style = "font-size: 18px;"),
            actionLink("link_to_q7", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Calculate the correlation between one multi-omics molecule and ",strong("pathway score")," across cancers",
              style = "font-size: 18px;"),
            actionLink("link_to_q8", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Survival", style = "font-size: 20px;"),
            solidHeader = TRUE, status="info",
            width = 12, height = 250,
            p("Calculate the ",strong("log-rank test")," analysis of one multi-omics molecule in one cancer.",
              style = "font-size: 18px;"),
            actionLink("link_to_q10", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Survival", style = "font-size: 20px;"),
            solidHeader = TRUE, status="info",
            width = 12, height = 250,
            p("Performe the ",strong("univariate cox regreesion")," analysis of one multi-omics molecule across cancers.",
              style = "font-size: 18px;"),
            actionLink("link_to_q4", p("Go >>>",style = "font-size: 20px;")),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("TCGA-Dimension", style = "font-size: 20px;"),
            solidHeader = TRUE, status="warning",
            width = 12, height = 250,
            p("Perform ",strong("dimension reduction analysis")," for multiple molecules of samples in one phenotype.",
              style = "font-size: 18px;"),
            actionLink("link_to_q11", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = strong("PCAWG/CCLE", style = "font-size: 20px;"),
            solidHeader = TRUE, status="danger",
            width = 12, height = 250,
            p("Enter ",em(strong("Custom T·P·C Modules"))," page to explore similar modules designed for PCAWG and CCLE databases.",
              style = "font-size: 18px;")
          )
        ),
      ),
      br(),
      h2(strong("※ General TCGA pipelines with personalized operations")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(
          3,
          shinydashboard::box(
            title = strong("TCGA-Comparison", style = "font-size: 20px;"),
            solidHeader = TRUE, status="primary",
            width = 12, height = 250,
            p("Perform versatile comparison analysis for one identifier from the comprehensive tumor ",
              "omics and non-omics data or user-defined metadata based on ",strong("customizable grouping"),".",
              style = "font-size: 18px;"),
            actionLink("link_to_p1", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = strong("TCGA-Correlation", style = "font-size: 20px;"),
            solidHeader = TRUE, status="success",
            width = 12, height = 250,
            p("Perform versatile correlation analysis between ",strong("two random identifiers")," from the comprehensive tumor ",
              "omics and non-omics data and user-defined metadata.",
              style = "font-size: 18px;"),
            actionLink("link_to_p2", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = strong("TCGA-Survival", style = "font-size: 20px;"),
            solidHeader = TRUE, status="info",
            width = 12, height = 250,
            p("Perform ",strong("log-rank test or univariate cox regreesion")," survival analysis for one identifier from the comprehensive tumor ",
              "omics and non-omics data and user-defined metadata based on ",strong("customizable grouping"),".",
              style = "font-size: 18px;"),
            actionLink("link_to_p3", p("Go >>>",style = "font-size: 20px;")),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = strong("PCAWG/CCLE", style = "font-size: 20px;"),
            solidHeader = TRUE, status="danger",
            width = 12, height = 250,
            p("Enter ",em(strong("Personalized T·P·C Pipelines"))," page to explore ",
              "similar versatile pipeline analyses (including comparison, correlation, survival) with  personalized operations for PCAWG and CCLE databases.",
              style = "font-size: 18px;")
          )

        )
      ),
      br(),
      h2(strong("※ Latest significant release notes")),
      tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
      fluidRow(
        column(12, #offset = 1,
          tags$ul(
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


