ui.page_cbioportal <- function() {
  tabPanel(
    title = "cBioPortal Analysis",
    icon = icon("dna"),
    h1("cBioPortal Cancer Genomics Data Analysis", align = "center"),
    
    tabsetPanel(
      tabPanel(
        "Molecular Correlation",
        tabsetPanel(
          tabPanel(
            "Gene-Gene Correlation",
            ui.modules_cbioportal_correlation("modules_cbioportal_correlation")
          )
        )
      ),
      
      tabPanel(
        "Study Explorer",
        fluidRow(
          column(
            12,
            h3("Explore cBioPortal Studies"),
            p("Use this module to browse available studies and data types from cBioPortal."),
            ui.modules_cbioportal_study_selector("study_explorer")
          )
        )
      ),
      
      tabPanel(
        "About cBioPortal",
        fluidRow(
          column(
            12,
            h3("About cBioPortal Integration"),
            br(),
            
            tags$p(
              "The cBioPortal for Cancer Genomics is an open-access, open-source resource for ",
              "interactive exploration of multidimensional cancer genomics data sets. This integration ",
              "allows you to access and analyze data from hundreds of cancer studies directly within UCSCXenaShiny.",
              style = "font-size: 16px;"
            ),
            
            h4("Key Features:"),
            tags$ul(
              tags$li("Access to 400+ cancer studies from cBioPortal"),
              tags$li("Multiple data types: gene expression, mutations, copy number alterations"),
              tags$li("Interactive correlation and comparison analysis"),
              tags$li("Clinical data integration"),
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
              tags$a("cBioPortalData R Package", 
                     href = "https://waldronlab.io/cBioPortalData/", target = "_blank"),
              br(),
              tags$a("API Documentation", 
                     href = "https://docs.cbioportal.org/6.-web-api-and-clients/api-and-api-clients", target = "_blank")
            )
          )
        )
      )
    )
  )
}