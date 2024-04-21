# options(shiny.autoreload = TRUE)
# Step1: Global setting ---------------------------------------------------------
options(shiny.fullstacktrace=TRUE)
xena.runMode <- getOption("xena.runMode", default = "client")
message("Run mode: ", xena.runMode)
# 'client' for personal user, 'server' for running on server.

# Path for storing dataset files
if (is.null(getOption("xena.cacheDir"))) {
  options(xena.cacheDir = switch(xena.runMode,
                                 client = file.path(tempdir(), "UCSCXenaShiny"), 
                                 server = "~/.xenashiny"
  ))
}
XENA_DEST <- path.expand(file.path(getOption("xena.cacheDir"), "datasets"))
if (!dir.exists(XENA_DEST)) {
  dir.create(XENA_DEST, recursive = TRUE)
}

options(shiny.maxRequestSize=1024*1024^2) #maximum file size upload

# Step2: Load necessary packages & data & functions ----------------------------------
message("Checking dependencies...")
source(system.file("shinyapp/utils_pkgs.R", package = "UCSCXenaShiny"))
source(system.file("shinyapp/utils_appdata.R", package = "UCSCXenaShiny"))
source(system.file("shinyapp/utils_func.R", package = "UCSCXenaShiny"))
source(system.file("shinyapp/utils_plot.R", package = "UCSCXenaShiny")) #TPC pipelines plot

# Put modules here --------------------------------------------------------
message("Loading modules and UIs...")
modules_path <- system.file("shinyapp", "modules", package = "UCSCXenaShiny", mustWork = TRUE)
modules_file <- dir(modules_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
sapply(modules_file, function(x, y) source(x, local = y), y = environment())

# Put page UIs here -----------------------------------------------------
pages_path <- system.file("shinyapp", "ui", package = "UCSCXenaShiny", mustWork = TRUE)
pages_file <- dir(pages_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
sapply(pages_file, function(x, y) source(x, local = y), y = environment())
