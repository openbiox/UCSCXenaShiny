#!/usr/bin/env Rscript
# Copyright (C) 2021-2024 Xena Shiny Team

# The cache directory and port all should be consistent with
# configs in Dockerfile.

# Check system info
print(Sys.info())

# Set options and run app

if(dir.exists("/app/xena")){ 
  # hiplot
  xena.cacheDir = "/app/xena"
  xena.zenodoDir = "/app/xena/zdatasets"
} else {
  # zhoulab
  xena.cacheDir = "/home/shiny/apps/xena/cache"
  xena.zenodoDir = "/home/shiny/apps/xena/datasets/"
}
options(xena.cacheDir = xena.cacheDir, xena.zenodoDir = xena.zenodoDir)
options(xena.runMode = "server")

library(UCSCXenaShiny)

shiny::shinyAppFile(
  system.file("shinyapp", "App.R", package = "UCSCXenaShiny")
)

