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



# Step4: Run APP
# UI part ----------------------------------------------------------------------
message("Starting...")
ui <- tagList(
  tags$head(
    tags$title("XenaShiny"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/global.css")
  ),

  useWaiter(), 
  waiterPreloader(html = tagList(
    spin_fading_circles(), 
    br(), br(),
    h1(strong("Welcome to use UCSCXenaShiny v2 application!")),
    br(),
    p("An interactive web tool with general and personalized modules to explore UCSC Xena datasets"
      ,style = "font-size: 25px;"),
    br(),br(),
    p("Notes:", "(1) The initiation could take about 10 seconds. (2) Please zoom in or up screen for better representation.",
      style = "font-size: 16px;")
  ), color = "#2C3E50"),

  shinyjs::useShinyjs(),
  autoWaiter(html = spin_loader(), color = transparent(0.5)), # change style https://shiny.john-coene.com/waiter/

  navbarPage(
    id = "navbar",
    title = "UCSCXenaShiny v2",
    windowTitle = "UCSCXenaShiny",
    # inst/shinyapp/ui
    ui.page_home(),
    ui.page_repository(),
    ui.page_general_analysis(),
    ui.page_pancan_quick(),
    # ui.page_pancan_tcga(),
    # ui.page_PharmacoGenomics(),
    ui.page_download(),
    ui.page_help(),
    ui.page_developers(),
    footer = ui.footer(),
    collapsible = TRUE,
    theme = tryCatch(shinythemes::shinytheme("flatly"),
                     error = function(e) {
                       "Theme 'flatly' is not available, use default."
                       NULL
                     })
  )
)

# Server Part ---------------------------------------------------------------
server <- function(input, output, session) {
  message("Shiny app run successfully! Enjoy it!\n")
  message("               --  Xena shiny team\n")
  # Stop warn
  storeWarn <- getOption("warn")
  options(warn = -1) 
  # observe(print(input$navbar))
  # inst/shinyapp/server
  source(server_file("home.R"), local = TRUE)
  source(server_file("repository.R"), local = TRUE)
  source(server_file("modules.R"), local = TRUE)
  source(server_file("general-analysis.R"), local = TRUE)
  observe_helpers(help_dir = system.file("shinyapp", "helper", package = "UCSCXenaShiny"))
}

# Run web app -------------------------------------------------------------
shiny::shinyApp(
  ui = ui,
  server = server
)