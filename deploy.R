#!/usr/bin/env r
# Copyright (C) 2021 Xena Shiny Team

# The cache directory and port all should be consistent with
# configs in Dockerfile.

options(xena.cacheDir = "/opt/xena") 
library(UCSCXenaShiny)
app_run(runMode = "server", port = 3838)
