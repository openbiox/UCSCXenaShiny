##!/usr/bin/env Rscript
# Copyright (C) 2021-2024 Xena Shiny Team
# An indepedent version for starting UCSCXenaShinyV1 v1 on zhoulab

# Check system info
print(Sys.info())

# Set options and run app

if(dir.exists("/xena")){ 
  stop("Not working for this")
  # hiplot
  #xena.cacheDir = "/xena"
  #xena.zenodoDir = "/xena/datasets"
} else {
  # zhoulab
  xena.cacheDir = "/home/shiny/apps/xena_v1/cache"
  xena.zenodoDir = "/home/shiny/apps/xena_v1/datasets"
}
options(xena.cacheDir = xena.cacheDir, xena.zenodoDir = xena.zenodoDir)
options(xena.runMode = "server")

library(UCSCXenaShinyV1)

tryCatch({
  # Preload datasets
  load_data("transcript_identifier")
  load_data("tcga_TIL")
  invisible(NULL)
}, error = function(e) {
  warning("Preload data failed due to the network, it will try again when starting Shiny!")
})

shiny::shinyAppFile(
  system.file("shinyapp", "App.R", package = "UCSCXenaShinyV1")
)
