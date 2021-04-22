# Detect run mode ---------------------------------------------------------
xena.runMode <- getOption("xena.runMode", default = "client")
message("Run mode: ", xena.runMode)

# Load necessary packages ----------------------------------
message("Checking depedencies...")

if (!requireNamespace("pacman")) {
  install.packages("pacman")
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

pacman::p_load(
  purrr,
  tidyr,
  stringr,
  magrittr,
  dplyr,
  ggplot2,
  cowplot,
  ggpubr,
  plotly,
  UCSCXenaTools,
  UCSCXenaShiny,
  shiny,
  shinyBS,
  shinyjs,
  shinyWidgets,
  shinyalert,
  shinyFiles,
  survival,
  survminer,
  ezcox,
  waiter,
  colourpicker,
  DT,
  fs,
  RColorBrewer,
  gganatogram,
  ggstatsplot,
  ggradar,
  zip
)

if (packageVersion("UCSCXenaTools") < "1.4.4") {
  warning("UCSCXenaTools <1.4.4, this shiny has a known issue (the download button cannot be used) to work with it. Please upate this package!",
    immediate. = TRUE
  )
}

message("Starting...")

# Put data here -----------------------------------------------------------
data("XenaData", package = "UCSCXenaTools", envir = environment())
xena_table <- XenaData[, c(
  "XenaDatasets", "XenaHostNames", "XenaCohorts",
  "SampleCount", "DataSubtype", "Label", "Unit"
)]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")

TCGA_datasets <- xena_table %>%
  dplyr::filter(Hub == "tcgaHub") %>%
  dplyr::select("Cohort") %>%
  unique() %>%
  dplyr::mutate(
    id = stringr::str_match(Cohort, "\\((\\w+?)\\)")[, 2],
    des = stringr::str_match(Cohort, "(.*)\\s+\\(")[, 2]
  ) %>%
  dplyr::arrange(id)

TCGA_cli_merged <- dplyr::full_join(
  load_data("tcga_clinical"),
  load_data("tcga_surv"),
  by = "sample"
)

pancan_identifiers <- readRDS(
  system.file(
    "extdata", "pancan_identifier_list.rds",
    package = "UCSCXenaShiny"
  )
)
all_preload_identifiers <- c("NONE", as.character(unlist(pancan_identifiers)))

phenotype_datasets <- UCSCXenaTools::XenaData %>%
  dplyr::filter(Type == "clinicalMatrix") %>%
  dplyr::pull(XenaDatasets)

themes_list <- list(
  "cowplot" = cowplot::theme_cowplot(),
  "Light" = theme_light(),
  "Minimal" = theme_minimal(),
  "Classic" = theme_classic(),
  "Gray" = theme_gray(),
  "half_open" = cowplot::theme_half_open(),
  "minimal_grid" = cowplot::theme_minimal_grid()
)

TIL_signatures <- colnames(load_data("tcga_TIL"))[-1]

## data summary
Data_hubs_number <- length(unique(xena_table$Hub))
Cohorts_number <- length(unique(xena_table$Cohort))
Datasets_number <- length(unique(xena_table$`Dataset ID`))
Samples_number <- "~2,000,000"
Primary_sites_number <- "~37"
Data_subtypes_number <- "~45"
Xena_summary <- dplyr::group_by(xena_table, Hub) %>%
  dplyr::summarise(
    n_cohort = length(unique(.data$Cohort)),
    n_dataset = length(unique(.data$`Dataset ID`)), .groups = "drop"
  )

# global color
mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))

# Cache file dest directory
XENA_DEST <- if (xena.runMode == "client") {
  file.path(tempdir(), "UCSCXenaShiny")
} else {
  path.expand("~/.xenashiny/datasets")
}

if (!dir.exists(XENA_DEST)) {
  dir.create(XENA_DEST, recursive = TRUE)
}

# Put modules here --------------------------------------------------------
modules_path <- system.file("shinyapp", "modules", package = "UCSCXenaShiny", mustWork = TRUE)
modules_file <- dir(modules_path, pattern = "\\.R$", full.names = TRUE)
sapply(modules_file, function(x, y) source(x, local = y), y = environment())


# Put page UIs here -----------------------------------------------------
pages_path <- system.file("shinyapp", "ui", package = "UCSCXenaShiny", mustWork = TRUE)
pages_file <- dir(pages_path, pattern = "\\.R$", full.names = TRUE)
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
  message("Querying data of identifier ", id, " from dataset ", dataset)
  id_value <- get_data(dataset, id)
  df <- dplyr::tibble(
    sample = names(id_value),
    X = as.numeric(id_value)
  )
  colnames(df)[2] <- id
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
    )
  ),
  shinyjs::useShinyjs(),
  use_waiter(),
  waiter_on_busy(html= spin_3k(),color= transparent(0.7)),
  navbarPage(
    id = "navbar",
    title = div(
      img(src = "xena_shiny-logo_white.png", height = 49.6, style = "margin:-20px -15px -15px -15px")
    ),
    # inst/shinyapp/ui
    ui.page_home(),
    ui.page_repository(),
    ui.page_general_analysis(),
    ui.page_pancan(),
    ui.page_global(),
    ui.page_help(),
    ui.page_developers(),
    footer = ui.footer(),
    collapsible = TRUE,
    theme = shinythemes::shinytheme("flatly")
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
  source(server_file("global.R"), local = TRUE)
  source(server_file("general-analysis.R"), local = TRUE)
}

# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
