#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: MIT @Openbiox   ##########
#########################################


#' Run Xena Shiny App
#' 
#' @importFrom shiny shinyAppFile
#' @importFrom utils data
#' @return NULL
#' @export
#'
#' @examples 
#' \donttest{app_run()}
app_run <- function() {
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
  shiny::shinyAppFile(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))
}

# Global variables --------------------------------------------------------
# This is used for passing package checks
#   Fix problem caused by 'no visible binding for global variable'

utils::globalVariables(
  c("XenaData"
  )
)