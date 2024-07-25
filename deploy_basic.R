#!/usr/bin/env Rscript
# Copyright (C) 2021-2024 Xena Shiny Team

# The cache directory and port all should be consistent with
# configs in Dockerfile.

# Check system info
print(Sys.info())

# Set options and run app

if(dir.exists("/xena")){ 
  # hiplot
  #xena.cacheDir = "/xena"
  #xena.zenodoDir = "/xena/datasets"
} else {
  # zhoulab
  xena.cacheDir = "/home/shiny/apps/xena/cache"
  xena.zenodoDir = "/home/shiny/apps/xena/datasets/"
}
options(xena.cacheDir = xena.cacheDir, xena.zenodoDir = xena.zenodoDir)
options(xena.runMode = "server")

library(UCSCXenaShiny)
app_run2("server", content = "sq")

