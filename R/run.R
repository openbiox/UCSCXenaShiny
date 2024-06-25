#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: GPLv3 @Openbiox   ##########
#########################################


#' Run UCSC Xena Shiny App
#'
#' @importFrom shiny shinyAppFile
#' @inheritParams shiny::runApp
#' @param runMode default is 'client' for personal user, set it to 'server' for running on server.
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' app_run()
#' }
app_run <- function(runMode = "client", port = getOption("shiny.port")) {
  runMode <- match.arg(runMode, choices = c("client", "server"))
  options(xena.runMode = runMode)
  shiny::shinyAppFile(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"),
                      options = list(port = port))
}




#' Run UCSC Xena Shiny App with specifc content
#'
#' @importFrom shiny shinyAppFile
#' @inheritParams shiny::runApp
#' @param runMode default is 'client' for personal user, set it to 'server' for running on server.
#' @param content a: all module; s: simplfied module; q: quick tpc modules; p: personalized tpc pipelines; d: pharmcogenomics
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' app_run2(content = 's')
#' }
app_run2 = function(runMode = "client", port = getOption("shiny.port"),
                    content = c("a",'s','q','p','d')){
  runMode <- match.arg(runMode, choices = c("client", "server"))
  options(xena.runMode = runMode)
  content = match.arg(content)
  if(content=="a"){ 
    cm1 = ""; cm2 = ""; cm3 = ""; cm0 = ""; cm_link = "#"
  } else if (content == "s"){
    cm1 = "#"; cm2 = "#"; cm3 = "#"; cm0 = ""; cm_link = "#"
    if(runMode == "server") {cm_link = ""} # server application with link2advanced tabpanel
  } else if (content == "q"){
    cm1 = ""; cm2 = "#"; cm3 = "#"; cm0 = "#"; cm_link = "#"
  } else if (content == "p"){
    cm1 = "#"; cm2 = ""; cm3 = "#"; cm0 = "#"; cm_link = "#"
  } else if (content == "d"){
    cm1 = "#"; cm2 = ""; cm3 = ""; cm0 = "#"; cm_link = "#"
  } 
  codes = glue::glue(
    'source(system.file("shinyapp/app_preset.R", package = "UCSCXenaShiny")) \n',
    '\n',
    'ui <- tagList( \n',
    '  tags$head( \n',
    '    tags$title("XenaShiny"), \n',
    '    tags$link(rel = "stylesheet", type = "text/css", href = "./css/global.css") \n',
    '  ), \n',
    '  useWaiter(), \n',
    '  waiterPreloader(html = tagList( \n',
    '    spin_fading_circles(), \n', 
    '    br(), br(), \n',
    '    h1(strong("Welcome to use UCSCXenaShiny v2 application!")), \n',
    '    br(), \n',
    '    p("An interactive web tool with general and personalized modules to explore UCSC Xena datasets" \n',
    '      ,style = "font-size: 25px;"), \n',
    '    br(),br(), \n',
    '    p("Notes:", "(1) The initiation could take about 10 seconds. (2) Please zoom in or up screen for better representation.", \n',
    '      style = "font-size: 16px;") \n',
    '  ), color = "#2C3E50"), \n',
    '  shinyjs::useShinyjs(), \n',
    '  autoWaiter(html = spin_loader(), color = transparent(0.5)), # change style https://shiny.john-coene.com/waiter/ \n',
    '  navbarPage( \n',
    '    id = "navbar", \n',
    '    title = "UCSCXenaShiny v2", \n',
    '    windowTitle = "UCSCXenaShiny", \n',
    '    # inst/shinyapp/ui \n',
    '    ui.page_home(), \n',
    '    {cm0}ui.page_repository(), \n',
    '    {cm0}ui.page_general_analysis(), \n',
    '    {cm_link}ui.link2advanced(), \n',
    '    {cm1}ui.page_pancan_quick(), \n',
    '    {cm2}ui.page_pancan_tcga(), \n',
    '    {cm3}ui.page_PharmacoGenomics(), \n',
    '    {cm0}ui.page_download(), \n',
    '    ui.page_help(), \n',
    '    ui.page_developers(), \n',
    '    footer = ui.footer(), \n',
    '    collapsible = TRUE, \n',
    '    theme = tryCatch(shinythemes::shinytheme("flatly"), \n',
    '                     error = function(e) {{ \n',
    '                       "Theme \'flatly\' is not available, use default." \n',
    '                       NULL \n',
    '                     }}) \n',
    '  ) \n',
    ') \n',
    '\n',
    'server <- function(input, output, session) {{ \n',
    '  message("Shiny app run successfully! Enjoy it!\n") \n',
    '  message("               --  Xena shiny team\n") \n',
    '  # Stop warn \n',
    '  storeWarn <- getOption("warn") \n',
    '  options(warn = -1) \n',
    '  # observe(print(input$navbar)) \n',
    '  # inst/shinyapp/server \n',
    '  source(server_file("home.R"), local = TRUE) \n',
    '  source(server_file("repository.R"), local = TRUE) \n',
    '  source(server_file("modules.R"), local = TRUE) \n',
    '  source(server_file("general-analysis.R"), local = TRUE) \n',
    '  observe_helpers(help_dir = system.file("shinyapp", "helper", package = "UCSCXenaShiny")) \n',
    '}} \n',
    '\n',
    'shiny::shinyApp( \n',
    '  ui = ui, \n',
    '  server = server \n',
    ') \n'
  )
  
  
  app_file = paste0(get_cache_dir(),"/App_",content,".R")
  dirname(app_file)
  
  if(!dir.exists(dirname(app_file))){
    dir.create(dirname(app_file),recursive  = TRUE)
  }
  
  readr::write_file(codes, file = app_file)
  
  shiny::shinyAppFile(app_file,
                      options = list(port = port))
}
