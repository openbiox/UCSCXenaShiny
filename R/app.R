#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox


# Dependencies check ------------------------------------------------------
pkgs <- c("shiny", "shinythemes", "magrittr", "UCSCXenaTools",
          "echarts4r")
for (pkg in pkgs){
  if (!require(pkg, character.only = TRUE)) {
    message("Installing dependencies ", "\'", pkg, "\'...")
    install.packages(pkg, dependencies = TRUE)
  }
}
# Clean variable
rm(pkgs)

# Global definition -------------------------------------------------------

# Here data goes
data("XenaData", package = "UCSCXenaTools")

# UI ----------------------------------------------------------------------

ui = fluidPage(
  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  sidebarPanel(
    textInput("txt", "Text input:", "text here"),
    sliderInput("slider", "Slider input:", 1, 100, 30),
    actionButton("action", "Button"),
    actionButton("action2", "Button2", class = "btn-primary")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Tab 1"),
      tabPanel("Tab 2")
    )
  )
)


# Server ------------------------------------------------------------------

server = function(input, output) {}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
