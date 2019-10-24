# Load necessary packages ----------------------------------
require(dplyr)
require(ggplot2)
require(ggpubr)
require(plotly)
require(UCSCXenaTools)
require(UCSCXenaShiny)
require(shiny)
require(shinyBS)
require(shinyjs)
require(shinyWidgets)
require(survival)
require(survminer)

# Put data here -----------------------------------------------------------
data("XenaData", package = "UCSCXenaTools", envir = environment())
xena_table <- XenaData[, c(
  "XenaDatasets", "XenaHostNames", "XenaCohorts",
  "SampleCount", "DataSubtype", "Label"
)]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")
data("dat_datasets", package = "UCSCXenaShiny", envir = environment())
data("dat_samples", package = "UCSCXenaShiny", envir = environment())
data("XenaInfo", package = "UCSCXenaShiny", envir = environment())
Data_hubs_number <- XenaInfo[["n_hubs"]]
Cohorts_number <- XenaInfo[["n_cohorts"]]
Datasets_number <- XenaInfo[["n_datasets"]]
Samples_number <- XenaInfo[["n_samples"]]
Primary_sites_number <- XenaInfo[["n_origin"]]
Data_subtypes_number <- XenaInfo[["n_subtypes"]]

# global color
mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))
# need at least 140 colors for summary plot
mycolor <- rep(mycolor, 15)


# Put modules here --------------------------------------------------------
modules_path <- system.file("shinyapp", "modules", package = "UCSCXenaShiny", mustWork = TRUE)
modules_file <- dir(modules_path, pattern = "\\.R$", full.names = TRUE)
sapply(modules_file, function(x, y) source(x, local = y), y = environment())


# Put page UIs here -----------------------------------------------------
pages_path <- system.file("shinyapp", "ui", package = "UCSCXenaShiny", mustWork = TRUE)
pages_file <- dir(pages_path, pattern = "\\.R$", full.names = TRUE)
sapply(pages_file, function(x, y) source(x, local = y), y = environment())


# Obtain path to individual server code parts ----------------------------
server_file = function(x) {
  server_path = system.file("shinyapp", "server", 
                            package = "UCSCXenaShiny", mustWork = TRUE)
  file.path(server_path, x)
}



# UI part ----------------------------------------------------------------------
ui <- tagList(
  tags$head(tags$title("XenaShiny")),
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(
      img(src = "xena_shiny-logo_white.png", height = 49.6, style = "margin:-20px -15px -15px -15px")
    ),
    # inst/shinyapp/ui
    ui.page_home(),
    ui.page_repository(),
    ui.page_modules(),
    ui.page_pipelines(),
    ui.page_help(),
    ui.page_developers(),
    footer = ui.footer(),
    theme = shinythemes::shinytheme("cosmo")
  )
)

# Server Part ---------------------------------------------------------------
server <- function(input, output, session) {
  message("Shiny app run successfully! Enjoy it!\n")
  message("               --  Xena shiny team\n")
  
  # inst/shinyapp/server
  source(server_file("home.R"), local = TRUE)
  source(server_file("repository.R"), local = TRUE)
  source(server_file("modules.R"), local = TRUE)

}

# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
