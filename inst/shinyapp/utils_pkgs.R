if (!requireNamespace("pacman")) {
  install.packages("pacman", repos = "http://cran.r-project.org")
}
library(pacman)

if (!requireNamespace("gganatogram")) {
  pacman::p_load(remotes)
  tryCatch(
    remotes::install_github("jespermaag/gganatogram"),
    error = function(e) {
      remotes::install_git("https://gitee.com/XenaShiny/gganatogram")
    }
  )
}


pacman::p_load(
  purrr,
  tidyr,
  stringr,
  magrittr,
  R.utils,
  data.table,
  dplyr,
  ggplot2,
  cowplot,
  patchwork,
  ggpubr,
  plotly,
  UpSetR,
  UCSCXenaTools,
  UCSCXenaShiny,
  shiny,
  shinyBS,
  shinyjs,
  shinyWidgets,
  shinyalert,
  shinyFiles,
  shinyFeedback,
  shinythemes,
  shinyhelper,
  shinycssloaders,
  shinydashboard,
  shinydashboardPlus,
  # bs4Dash,
  # bslib,
  survival,
  survminer,
  ezcox,
  waiter,
  colourpicker,
  DT,
  fs,
  RColorBrewer,
  gganatogram,
  ggcorrplot,
  ggstatsplot,
  ggradar,
  zip,
  msigdbr,
  slickR
)

if (!requireNamespace("ggradar")) {
  pacman::p_load(remotes)
  tryCatch(
    remotes::install_github("ricardo-bion/ggradar"),
    error = function(e) {
      remotes::install_git("https://gitee.com/XenaShiny/ggradar")
    }
  )
}

if (packageVersion("UCSCXenaTools") < "1.4.4") {
  tryCatch(
    install.packages("UCSCXenaTools", repos = "http://cran.r-project.org"),
    error = function(e) {
      warning("UCSCXenaTools <1.4.4, this shiny has a known issue (the download button cannot be used) to work with it. Please upate this package!",
        immediate. = TRUE
      )
    }
  )
}