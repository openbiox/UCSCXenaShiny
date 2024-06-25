source(system.file("shinyapp/app_preset.R", package = "UCSCXenaShiny"))

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
    ui.page_pancan_tcga(),
    ui.page_PharmacoGenomics(),
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
