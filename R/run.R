#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: MIT @Openbiox   ##########
#########################################


#' Run UCSC Xena Shiny App
#'
#' @importFrom shiny shinyAppFile
#' @param runMode default is 'client' for personal user, set it to 'server' for running on server.
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' app_run()
#' }
app_run <- function(runMode = "client") {
  runMode <- match.arg(runMode, choices = c("client", "server"))
  options(xena.runMode = runMode)
  shiny::shinyAppFile(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))
}
