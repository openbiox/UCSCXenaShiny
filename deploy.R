#!/usr/bin/env Rscript
# Copyright (C) 2021 Xena Shiny Team

# The cache directory and port all should be consistent with
# configs in Dockerfile.

# Check system info
print(Sys.info())

# Set options and run app

options(xena.cacheDir = "/xena", xena.zenodoDir = "/xena/datasets")
options(xena.runMode = "server")

library(UCSCXenaShiny)

tryCatch({
  # Preload datasets
  load_data("transcript_identifier")
  load_data("tcga_TIL")
  invisible(NULL)
}, error = function(e) {
  warning("Preload data failed due to the network, it will try again when starting Shiny!")
})

shiny::shinyAppFile(
  system.file("shinyapp", "App.R", package = "UCSCXenaShiny")
)
