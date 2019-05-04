#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: MIT @Openbiox   ##########
#########################################


# Here data goes
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


#' Run Xena Shiny App
#' 
#' @importFrom shiny shinyAppFile
#' @return NULL
#' @export
#'
#' @examples 
#' \donttest{app_run()}
app_run <- function() {
  shiny::shinyAppFile(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))
}

# Global variables --------------------------------------------------------
# This is used for passing package checks
#   Fix problem caused by 'no visible binding for global variable'

# utils::globalVariables(
#   c("Cohort",
#     "DataSubtype",
#     "DatasetCount",
#     "Hub",
#     "Percent",
#     "SampleCount",
#     "SampleCount_percent",
#     "SampleCount_sum",
#     "Sample_percent",
#     "Type",
#     "XenaCohorts",
#     "XenaDatasets",
#     "XenaHostNames",
#     "N"
#   )
# )