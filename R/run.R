#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: MIT @Openbiox   ##########
#########################################


#' Run Xena Shiny App
#'
#' @importFrom shiny shinyAppFile
#' @return NULL
#' @export
#'
#' @examples
#' \donttest{
#' app_run()
#' }
app_run <- function() {
  shiny::shinyAppFile(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))
}

# Global variables --------------------------------------------------------
# This is used for passing package checks
#   Fix problem caused by 'no visible binding for global variable'

# utils::globalVariables(
#   c("XenaData")
# )
