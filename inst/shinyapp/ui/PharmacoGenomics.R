ui.page_PharmacoGenomics <- function() {
  navbarMenu(
    title = "PharmacoGenomics",
    icon = icon("buromobelexperte"),
    ## Drug-Omics Correlation Analysis ----
    tabPanel(
      "Drug-Omics Correlation Analysis",
      uiDrugOmicPair("DrugOmicPair")
    ),
    ## Profiles Display ----
    tabPanel(
      "Feature Abundance Profile in Databases",
      uiFeatureAcrossType("FeatureAcrossType")
    ),
    tabPanel(
      "Dimension Reduction Profile of Cell Drug Sensitivity",
      uiProfileDrugSens("ProfileDrugSens")  
    ),
    ## Features database significant analysis ----
    tabPanel(
      "Feature Scaling Association Analysis",
      uiFeatureDatabaseSig("FeatureDatabaseSig")
    ),
    ## Statistics and Annotations ----
    tabPanel(
      "Statistics and Annotations",
      uiStatAnno("StatAnno")
    ),
    ## Contact ----
    tabPanel("Contact",
             fluidPage(
               strong("Feel free to talk with me if you find any bugs or have any suggestions. :)"),
               p(""),
               p("Email: mugpeng@foxmail.com"),
               p("github: https://github.com/mugpeng"),
               p("You can visit https://github.com/mugpeng/OmicsPharDB to reach the toturial.")
             ))
  )
}
