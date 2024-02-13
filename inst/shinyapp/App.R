# Global setting ---------------------------------------------------------
options(shiny.fullstacktrace=TRUE)
xena.runMode <- getOption("xena.runMode", default = "client")
message("Run mode: ", xena.runMode)

if (is.null(getOption("xena.cacheDir"))) {
  options(xena.cacheDir = switch(xena.runMode,
                                 client = file.path(tempdir(), "UCSCXenaShiny"), 
                                 server = "~/.xenashiny"
  ))
}

# Path for storing dataset files
XENA_DEST <- path.expand(file.path(getOption("xena.cacheDir"), "datasets"))

if (!dir.exists(XENA_DEST)) {
  dir.create(XENA_DEST, recursive = TRUE)
}

# Set default path for saving extra-data downloaded from https://zenodo.org
if (xena.runMode == "server") {
  if (is.null(getOption("xena.zenodoDir"))) options(xena.zenodoDir = XENA_DEST)
}

# Load necessary packages ----------------------------------
message("Checking dependencies...")

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

options(shiny.maxRequestSize=1024*1024^2)
message("Starting...")

# Put data here -----------------------------------------------------------
#source(system.file("shinyapp/appdata.R", package = "UCSCXenaShiny"))

appdata_path = path.expand(file.path(getOption("xena.cacheDir"), "appdata.RData"))
message("Checking shiny app data from ", appdata_path, "...")
if (!file.exists(appdata_path)) {
  message("Processing and caching shiny app data...")
  source(system.file("shinyapp/appdata.R", package = "UCSCXenaShiny"))
  message("Done")
} else {
  load(appdata_path)
  message("Cached processed data used.")
}

# Put modules here --------------------------------------------------------
modules_path <- system.file("shinyapp", "modules", package = "UCSCXenaShiny", mustWork = TRUE)
modules_file <- dir(modules_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
sapply(modules_file, function(x, y) source(x, local = y), y = environment())


# Put page UIs here -----------------------------------------------------
pages_path <- system.file("shinyapp", "ui", package = "UCSCXenaShiny", mustWork = TRUE)
pages_file <- dir(pages_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
sapply(pages_file, function(x, y) source(x, local = y), y = environment())


# Obtain path to individual server code parts ----------------------------
server_file <- function(x) {
  server_path <- system.file("shinyapp", "server",
    package = "UCSCXenaShiny", mustWork = TRUE
  )
  file.path(server_path, x)
}


# Set utility functions ---------------------------------------------------
QUERY_CACHE <- dplyr::tibble()
xe_query_url <- function(data, use_cache = TRUE) {
  if (use_cache) {
    if (nrow(QUERY_CACHE) == 0) {
      non_exist_idx <- !data$XenaDatasets %in% NULL
    } else {
      non_exist_idx <- !data$XenaDatasets %in% QUERY_CACHE$datasets
    }
    if (any(non_exist_idx)) {
      non_exist_query <- xe_query_url(data[non_exist_idx, , drop = FALSE], use_cache = FALSE)
      QUERY_CACHE <<- dplyr::bind_rows(
        QUERY_CACHE,
        non_exist_query
      )
    }

    xe_query <- dplyr::filter(QUERY_CACHE, QUERY_CACHE$datasets %in% data$XenaDatasets)
  } else {
    xe <-
      UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)

    xe_query <- UCSCXenaTools::XenaQuery(xe)
    xe_query$browse <- purrr::map2(
      xe_query$datasets, xe_query$hosts,
      ~ utils::URLencode(
        paste0(
          "https://xenabrowser.net/datapages/?",
          "dataset=", .x, "&host=", .y
        )
      )
    ) %>% unlist()
  }

  return(xe_query)
}

get_data_df <- function(dataset, id) {
  if (dataset == "custom_phenotype_dataset") {
    message("Loading custom phenotype data.")
    df <- readRDS(file.path(tempdir(), "custom_phenotype_data.rds"))
  } else {
    message("Querying data of identifier ", id, " from dataset ", dataset)
    id_value <- if (dataset == "custom_feature_dataset") {
      UCSCXenaShiny:::query_custom_feature_value(id)
    } else {
      UCSCXenaShiny::query_molecule_value(dataset, id)
    }
    df <- dplyr::tibble(
      sample = names(id_value),
      X = as.numeric(id_value)
    )
    colnames(df)[2] <- id 
  }
  df
}

# UI part ----------------------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$title("XenaShiny"),
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }")
    ),
    tags$style(
      '[data-value = "Sole Analysis for Single Cancer"] {
        width: 400px;
       background-color: #bdbdbd;
      }
       [data-value = "Sole Analysis for Multiple Cancers"] {
        width: 400px;
       background-color: #525252;
      }
       [data-value = "Batch Analysis for Single Cancer"] {
        width: 400px;
       background-color: #525252;
      }
       [data-value = "Sole Analysis for Cell Lines"] {
        width: 400px;
       background-color: #bdbdbd;
      }
       [data-value = "Batch Analysis for Cell Lines"] {
        width: 400px;
       background-color: #525252;
      }
      .tab-pane {
        background-color: transparent;
        width: 100%;
        }
      .nav-tabs {font-size: 20px}   
      '
    )
  ),
  shinyjs::useShinyjs(),
  autoWaiter(html = spin_loader(), color = transparent(0.5)), # change style https://shiny.john-coene.com/waiter/
  navbarPage(
    id = "navbar",
    title = "UCSCXenaShiny v2",
    # div(
    #   img(src = "xena_shiny-logo_white.png", height = 49.6, style = "margin:-20px -15px -15px -15px")
    # ),
    windowTitle = "UCSCXenaShiny",
    # inst/shinyapp/ui
    ui.page_home(),
    ui.page_repository(),
    ui.page_general_analysis(),
    ui.page_pancan_quick(),
    ui.page_pancan_tcga(),
    ui.page_PharmacoGenomics(),
    # ui.page_pancan_pcawg(),
    # ui.page_pancan_ccle(),
    ui.page_download(),
    # ui.page_global(),
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

  # observeEvent({input$navbar=="TCGA+GTEx: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)"},{
  #     callModule(server.modules_pancan_dist, "module_gene_pancan_dist")
  # }, once = TRUE, ignoreInit = TRUE)  

  # source(server_file("global.R"), local = TRUE)

  source(server_file("general-analysis.R"), local = TRUE)
  # observe_helpers(help_dir ="helper")
  observe_helpers(help_dir = system.file("shinyapp", "helper", package = "UCSCXenaShiny"))

}

# Run web app -------------------------------------------------------------
shiny::shinyApp(
  ui = ui,
  server = server
)
