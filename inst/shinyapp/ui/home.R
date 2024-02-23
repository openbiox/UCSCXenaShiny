ui.page_home <- function() {
  tabPanel(
    title = "Home",
    icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    fluidPage(
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
            title = "TCGA-Comparison", 
            solidHeader = TRUE, status="primary",
            width = 12, height = 200,
            p("Compare one multi-omics molecular expression between tumor and normal (including GTEx) samples."),
            actionLink("link_to_q1", "Go >>>"),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Comparison", 
            solidHeader = TRUE, status="primary",
            width = 12, height = 200,
            p("Observe molecular expression between tumor and normal (including GTEx) samples via anatomy plot."),
            actionLink("link_to_q2", "Go >>>"),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Comparison", 
            solidHeader = TRUE, status="primary",
            width = 12, height = 200,
            p("Compare one multi-omics molecular expression between gene mutation and wild of tumor samples."),
            actionLink("link_to_q9", "Go >>>"),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p("Calculate the correlation of two multi-omics molecules acccording to their expression values."),
            actionLink("link_to_q3", "Go >>>"),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p("Calculate the correlation between one multi-omics molecule and immune signature scores across cancers"),
            actionLink("link_to_q5", "Go >>>"),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p("Calculate the correlation between one multi-omics molecule and tumor immune infiltration across cancers"),
            actionLink("link_to_q6", "Go >>>"),
          ),
        ),
      ),
      fluidRow(
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p("Calculate the correlation between one multi-omics molecule and TMB/Stemness/MSI across cancers"),
            actionLink("link_to_q7", "Go >>>"),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p("Calculate the correlation between one multi-omics molecule and pathway score across cancers"),
            actionLink("link_to_q8", "Go >>>"),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Survival", 
            solidHeader = TRUE, status="info",
            width = 12, height = 200,
            p("Calculate the log-rank test analysis of one multi-omics molecule in one cancer."),
            actionLink("link_to_q10", "Go >>>"),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Survival", 
            solidHeader = TRUE, status="info",
            width = 12, height = 200,
            p("Performe the univariate cox regreesion analysis of one multi-omics molecule across cancers."),
            actionLink("link_to_q4", "Go >>>"),
          ),
        ),
        column(
          2,
          shinydashboard::box(
            title = "TCGA-Dimension", 
            solidHeader = TRUE, status="warning",
            width = 12, height = 200,
            p("Perform dimension reduction analysis for multiple molecules of samples in one phenotype."),
            actionLink("link_to_q11", "Go >>>"),
          )
        ),
        column(
          2,
          shinydashboard::box(
            title = "PCAWG/CCLE", 
            solidHeader = TRUE, status="danger",
            width = 12, height = 200,
            p("Enter 'Quick T·P·C Analysis' page to explore similar modules designed for PCAWG and CCLE databases.")
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
            title = "TCGA-Comparison", 
            solidHeader = TRUE, status="primary",
            width = 12, height = 200,
            p(paste0("Perform versatile comparison analysis (3 modes) for one identifier from the comprehensive tumor ",
              "omics and non-omics data together with user-defined metadata based on custom grouping after optional sample filtering step.")),
            actionLink("link_to_p1", "Go >>>"),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = "TCGA-Correlation", 
            solidHeader = TRUE, status="success",
            width = 12, height = 200,
            p(paste0("Perform versatile correlation analysis (3 modes) between two random identifiers from the comprehensive tumor ",
              "omics and non-omics data together with user-defined metadata after optional sample filtering step.")),
            actionLink("link_to_p2", "Go >>>"),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = "TCGA-Survival", 
            solidHeader = TRUE, status="info",
            width = 12, height = 200,
            p(paste0("Perform versatile log-rank test or univariate cox regreesion survival analysis (3 modes) for one identifier from the comprehensive tumor ",
              "omics and non-omics data together with user-defined metadata after optional sample filtering step.")),
            actionLink("link_to_p3", "Go >>>"),
          )
        ),
        column(
          3,
          shinydashboard::box(
            title = "PCAWG/CCLE", 
            solidHeader = TRUE, status="danger",
            width = 12, height = 200,
            p(paste0("Enter 'personalized T·P·C Analysis' page to explore ",
              "similar versatile pipeline analyses (including comparison, correlation, survival) with  personalized operations for PCAWG and CCLE databases."))
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
              "If you have any questions, please report the ",
              a("issue", href = "https://github.com/openbiox/UCSCXenaShiny/issues"),
              " or email at lishensuo@163.com. We will get back to you ASAP.",
              style = "font-size: 20px;")
          )  
        )
      )
    )
  )
}


