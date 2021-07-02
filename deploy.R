#!/usr/bin/env Rscript
# Copyright (C) 2021 Xena Shiny Team

# The cache directory and port all should be consistent with
# configs in Dockerfile.

options(xena.cacheDir = "/opt/xena")
options(xena.runMode = "server")

library(UCSCXenaShiny)

shiny::shinyAppFile(
  system.file("shinyapp", "App.R", package = "UCSCXenaShiny")
)
