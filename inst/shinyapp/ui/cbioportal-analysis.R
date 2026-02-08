ui.page_cbioportal <- function() {
  tabPanel(
    title = "cBioPortal Analysis",
    icon = icon("dna"),
    h1("cBioPortal Cancer Genomics Data Analysis", align = "center"),
    
    tabsetPanel(
      # Reorder: Put About first to guide users
      tabPanel(
        "Getting Started",
        fluidRow(
          column(
            12,
            wellPanel(
              style = "background-color: #e7f4ff; border-left: 4px solid #0066cc; padding: 15px;",
              h3(icon("info-circle"), " About cBioPortal Integration"),
              br(),
              
              tags$p(
                "The cBioPortal for Cancer Genomics is an open-access, open-source resource for ",
                "interactive exploration of multidimensional cancer genomics data sets. This integration ",
                "allows you to access and analyze data from hundreds of cancer studies directly within UCSCXenaShiny.",
                style = "font-size: 16px;"
              )
            ),
            
            h4(icon("star"), " Key Features:"),
            tags$ul(
              tags$li("Access to 400+ cancer studies from cBioPortal"),
              tags$li("Multiple data types: gene expression, mutations, copy number alterations"),
              tags$li("Interactive correlation and comparison analysis"),
              tags$li("Clinical data integration"),
              tags$li("Survival analysis capabilities")
            ),
            
            h4(icon("database"), " Data Sources:"),
            tags$ul(
              tags$li("TCGA (The Cancer Genome Atlas)"),
              tags$li("ICGC (International Cancer Genome Consortium)"),
              tags$li("Published cancer studies"),
              tags$li("Institutional studies")
            ),
            
            wellPanel(
              style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px;",
              h4(icon("lightbulb"), " Quick Start Guide:"),
              tags$ol(
                tags$li(HTML("<strong>Step 1:</strong> Go to 'Study Explorer' tab and click 'Refresh Studies' to load available studies")),
                tags$li(HTML("<strong>Step 2:</strong> Select a study from the dropdown menu")),
                tags$li(HTML("<strong>Step 3:</strong> Choose the type of molecular data you want to analyze")),
                tags$li(HTML("<strong>Step 4:</strong> Use the 'Gene-Gene Correlation' tab to explore relationships between genes"))
              )
            ),
            
            wellPanel(
              style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px;",
              h4(icon("exclamation-triangle"), " Troubleshooting:"),
              tags$p(HTML("<strong>If you see connection errors:</strong>")),
              tags$ul(
                tags$li("Ensure you have a stable internet connection"),
                tags$li("Check that your firewall allows access to www.cbioportal.org"),
                tags$li("The cBioPortal server may be temporarily down - try again later"),
                tags$li(HTML("Install required package: <code>install.packages('cbioportalR')</code>"))
              )
            ),
            
            hr(),
            
            h4(icon("book"), " References:"),
            tags$p(
              tags$a("cBioPortal for Cancer Genomics", 
                     href = "https://www.cbioportal.org/", target = "_blank"),
              br(),
              tags$a("cbioportalR R Package", 
                     href = "https://www.karissawhiting.com/cbioportalR/", target = "_blank"),
              br(),
              tags$a("API Documentation", 
                     href = "https://docs.cbioportal.org/web-api-and-clients/", target = "_blank")
            )
          )
        )
      ),
      
      tabPanel(
        "Study Explorer",
        fluidRow(
          column(
            12,
            wellPanel(
              style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 15px;",
              icon("info-circle"),
              strong(" Instructions: "),
              "Click 'Refresh Studies' to load available cancer studies from cBioPortal. ",
              "This requires an active internet connection."
            ),
            ui.modules_cbioportal_study_selector("study_explorer")
          )
        )
      ),
      
      tabPanel(
        "Gene-Gene Correlation",
        wellPanel(
          style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 15px;",
          icon("info-circle"),
          strong(" Instructions: "),
          "Use this module to analyze correlation between two genes. ",
          "First load a study in the Study Explorer tab, then enter gene symbols and run the analysis."
        ),
        ui.modules_cbioportal_correlation("modules_cbioportal_correlation")
      )
    )
  )
}
              tags$li("Survival analysis capabilities")
            ),
            
            h4("Data Sources:"),
            tags$ul(
              tags$li("TCGA (The Cancer Genome Atlas)"),
              tags$li("ICGC (International Cancer Genome Consortium)"),
              tags$li("Published cancer studies"),
              tags$li("Institutional studies")
            ),
            
            h4("Getting Started:"),
            tags$ol(
              tags$li("Select a study from the dropdown menu"),
              tags$li("Choose the type of molecular data you want to analyze"),
              tags$li("Load the data and proceed with your analysis"),
              tags$li("Use the correlation module to explore gene-gene relationships")
            ),
            
            hr(),
            
            h4("References:"),
            tags$p(
              tags$a("cBioPortal for Cancer Genomics", 
                     href = "https://www.cbioportal.org/", target = "_blank"),
              br(),
              tags$a("cbioportalR R Package", 
                     href = "https://www.karissawhiting.com/cbioportalR/", target = "_blank"),
              br(),
              tags$a("API Documentation", 
                     href = "https://docs.cbioportal.org/web-api-and-clients/", target = "_blank")
            )
          )
        )
      )
    )
  )
}