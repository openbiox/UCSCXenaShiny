ui.page_PharmacoGenomics <- function() {
  navbarMenu(
    title = "PharmacoGenomics Analysis",
    icon = icon("buromobelexperte"),
    ## Drugs-omics pairs analysis ----
    tabPanel(
      "Drugs-omics pairs Analysis",
      uiDrugOmicPair("DrugOmicPair")
    ),
    ## Profiles Display ----
    tabPanel(
      "Profiles Display: Features across different types",
      uiFeatureAcrossType("FeatureAcrossType")
    ),
    tabPanel(
      "Profiles Display: Profile of drug sensitivity",
      uiProfileDrugSens("ProfileDrugSens")  
    ),
    ## Features database significant analysis ----
    tabPanel(
      "Features database significant analysis",
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
