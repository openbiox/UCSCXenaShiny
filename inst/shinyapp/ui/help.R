ui.page_help <- function() {
  md_prefix <- system.file("shinyapp/shiny-doc", package = "UCSCXenaShiny")
  set_md_path <- function(x) {
    file.path(md_prefix, x)
  }

  navbarMenu(
    title = "Help",
    icon = icon("question-circle"),
    tabPanel(
      "Usage",
      fluidPage(
        includeMarkdown(set_md_path("usage.md"))
      )
    ),
    tabPanel(
      "Term List",
      fluidPage(
        includeMarkdown(set_md_path("terms.md"))
      )
    ),
    tabPanel(a("Package doc", 
               href="https://openbiox.github.io/UCSCXenaShiny/index.html",
               target="_blank")),
    tabPanel(
      "Citation",
      fluidPage(
        includeMarkdown(set_md_path("citation.md"))
      )
    ),
    tabPanel(
      "Extra Datasets",
      fluidPage(
        includeMarkdown(set_md_path("datasets.md"))
      )
    ),
    tabPanel(a("Feature overview", 
               href="https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/bioinformatics/38/2/10.1093_bioinformatics_btab561/1/btab561_supplementary_data.pdf?Expires=1656169623&Signature=rJ6nWlQTfnLBq4O4r5CshbOoVclqyRN7jiibdgO7cGER8Fm-PCArYkzWp~oyhZxu7~VYdPtygAYjssguu9OsYi~OiSf5QCFA9S4Ws7BaNaxK7-oxqduZX9uK6I00IMHC4ED1jovLs3X44rx7Pk--RwFrYoAYV1u8M9UiiCTfTyX3JzySHqA0FDpi5MvKSPOwg5tqquCHUsg9pMB2uahyuhmyJ1R0n7YGhagmAT~nh0irTEtiEEgTrXtNRgcVTTUHsDvEl-QCU9jEssma0oxQxwoipeLlRWygOEPM1KffojCEvNetQDQnQqpePrDiGLkhqM3PkyUtnc62wwjkvhH1KA__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA",
               target="_blank"))
  )
}
