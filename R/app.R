#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox


# Dependencies check ------------------------------------------------------
pkgs <- c("shiny", "argonR", "argonDash", "magrittr", "UCSCXenaTools",
          "ECharts2Shiny")
for (pkg in pkgs){
  if (!require(pkg, character.only = TRUE)) {
    message("Installing dependencies ", "\'", pkg, "\'...")
    install.packages(pkg, dependencies = TRUE)
  }
}
# Clean variable
rm(pkgs)

# Here data goes
data("XenaData", package = "UCSCXenaTools")

library(dplyr)
library(tidyr)
library(tibble)
dat = XenaData %>% 
  group_by(XenaHostNames, XenaCohorts) %>% 
  summarise(N = n()) %>% 
  spread(key = XenaCohorts, value = N) %>% 
  column_to_rownames("XenaHostNames")

# Global definition -------------------------------------------------------

tabText1 <- "Raw denim you probably haven't heard of them jean shorts Austin. 
            Nesciunt tofu stumptown aliqua, retro synth master cleanse. Mustache 
            cliche tempor, williamsburg carles vegan helvetica. Reprehenderit 
            butcher retro keffiyeh dreamcatcher synth. Raw denim you probably 
            haven't heard of them jean shorts Austin. Nesciunt tofu stumptown 
            aliqua, retro synth master cleanse"

tabText2 <- "Cosby sweater eu banh mi, qui irure terry richardson ex squid. 
            Aliquip placeat salvia cillum iphone. Seitan aliquip quis cardigan 
            american apparel, butcher voluptate nisi qui."

tabText3 <- "Raw denim you probably haven't heard of them jean shorts Austin. 
            Nesciunt tofu stumptown aliqua, retro synth master cleanse. 
            Mustache cliche tempor, williamsburg carles vegan helvetica. 
            Reprehenderit butcher retro keffiyeh dreamcatcher synth"


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
    brand_logo = "https://github.com/openbiox/XenaShiny/blob/master/images/openbiox-logo.png?raw=true",
    argonSidebarMenu(
      argonSidebarItem(
        tabName = "home",
        icon = "tv-2",
        icon_color = "primary",
        "Home"
      ),
      argonSidebarItem(
        tabName = "tabs",
        icon = "planet",
        icon_color = "warning",
        "Tabs"
      ),
      argonSidebarItem(
        tabName = "alerts",
        icon = "bullet-list-67",
        icon_color = "danger",
        "Alerts"
      ),
      argonSidebarItem(
        tabName = "images",
        icon = "circle-08",
        icon_color = "success",
        "Images"
      ),

      argonSidebarItem(
        tabName = "badges",
        icon = "ui-04",
        icon_color = "pink",
        "Badges"
      ),
      argonSidebarItem(
        tabName = "progress",
        icon = "pin-3",
        icon_color = "yellow",
        "Progress"
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
        tabName = "home",
        # render plots
        loadEChartsLibrary(),
        tags$div(id="test", style="width:80%;height:300px;"),
        deliverChart(div_id = "test")
      ),
      argonTabItem(
        tabName = "tabs"
      ),
      argonTabItem(
        tabName = "alerts"
      ),
      argonTabItem(
        tabName = "images"
      ),
      argonTabItem(
        tabName = "badges"
      ),
      argonTabItem(
        tabName = "progress",
        argonProgress(value = 10, status = "danger", text = "Custom Text"),
        argonProgress(value = 40, status = "info", text = NULL),
        argonProgress(value = 90, status = "warning", text = argonIcon("atom"))
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
  
  # Call functions from ECharts2Shiny to render charts
  renderBarChart(div_id = "test", 
                 grid_left = '3%',
                 stack_plot = TRUE,
                 show.legend = FALSE,
                 data = dat)
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
