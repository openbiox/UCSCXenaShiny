#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox


# Dependencies check ------------------------------------------------------
pkgs <- c("shiny", "argonR", "argonDash", "magrittr", "UCSCXenaTools",
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

ui = argonDashPage(
  title = "Xena Shiny",
  author = "Openbiox Xena Shiny Team",
  description = "A shiny app for UCSC Xena",
  sidebar = argonDashSidebar(
    vertical = TRUE,
    skin = "light",
    background = "white",
    size = "md",
    side = "left",
    id = "xena_sidebar",
    brand_url = "https://github.com/openbiox",
    brand_logo = "https://github.com/openbiox/XenaShiny/blob/master/images/xena_shiny-logo.png?raw=true",
    argonSidebarMenu(
      argonSidebarItem(
        tabName = "home",
        icon = "tv-2",
        icon_color = "primary",
        "Home"
      ),
      argonSidebarItem(
        tabName = "repo",
        icon = "planet",
        icon_color = "warning",
        "Repository"
      ),
      argonSidebarItem(
        tabName = "developers",
        icon = "spaceship",
        icon_color = "info",
        "Developers"
      )
    )
  ),
  body = argonDashBody(
    argonTabItems(
      argonTabItem(
        # Home Page
        tabName = "home",
        argonRow(
          argonColumn(
            width = 6,
            sliderInput(
              "obs", 
              "Number of observations:",
              min = 0, 
              max = 1000, 
              value = 500
            )
          ),
          argonColumn(width = 6, plotOutput("distPlot"))
        )
      ),
      argonTabItem(
        tabName = "repo"
      ),
      argonTabItem(
        tabName = "developers",
        argonRow(
          argonColumn(
            width = 3,
            argonUser(
              title = "Shixiang Wang",
              subtitle = "Ph.D. student at ShianghaiTech. University",
              src = "https://avatars1.githubusercontent.com/u/25057508?s=460&v=4"
            )
          ),
          argonColumn(
            width = 3,
            argonUser(
              title = "Fei Zhao",
              subtitle = "Focus on Bioinformatics and Epigenetics",
              src = "https://avatars2.githubusercontent.com/u/17489298?s=460&v=4"
            )
          ),
          argonColumn(
            width = 3,
            argonUser(
              title = "Yi Xiong",
              subtitle = "Clinical Medicine major Xiangya hospital, Central South University",
              src = "https://avatars1.githubusercontent.com/u/28949856?s=460&v=4"
            )
          ),
          argonColumn(
            width = 3,
            argonUser(
              title = "Longfei Zhao",
              subtitle = "Bioinformatian",
              src = "https://avatars2.githubusercontent.com/u/37660840?s=460&v=4"
            )
          )
        )
      )
    )
  ),
  footer = argonDashFooter(
    copyrights = "MIT @ Openbiox Xena Shiny Team, 2019",
    src = "https://github.com/openbiox/XenaShiny",
    argonFooterMenu(
      argonFooterItem("Openbiox", src = "https://github.com/openbiox"),
      argonFooterItem("UCSC Xena", src = "http://xena.ucsc.edu/"), 
      argonFooterItem("UCSCXenaTools", src = "https://github.com/ShixiangWang/UCSCXenaTools")
    )
  )
)


# Server ------------------------------------------------------------------

server = function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
