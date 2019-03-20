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
        tabName = "effects",
        icon = "atom",
        icon_color = "black",
        "CSS effects"
      ),
      argonSidebarItem(
        tabName = "sections",
        icon = "credit-card",
        icon_color = "grey",
        "Sections"
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
        argonRow(
          argonCard(
            width = 12,
            src = NULL,
            status = "success",
            shadow = TRUE,
            border_level = 2,
            hover_shadow = TRUE,
            title = "Summary Info",
            argonRow(
              loadEChartsLibrary(),
              tags$div(id="test", style="width:80%;height:300px;"),
              deliverChart(div_id = "test")
            )
          )
        )
      ),
      argonTabItem(
        tabName = "tabs",
        argonRow(
          argonTabSet(
            id = "tab-1",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 6,
            iconList = list("atom", "atom", "atom"),
            argonTab(
              tabName = "Tab 1",
              active = FALSE,
              tabText1
            ),
            argonTab(
              tabName = "Tab 2",
              active = TRUE,
              tabText2
            ),
            argonTab(
              tabName = "Tab 3",
              active = FALSE,
              tabText3
            )
          ),
          argonTabSet(
            id = "tab-2",
            card_wrapper = TRUE,
            horizontal = FALSE,
            circle = TRUE,
            size = "sm",
            argonTab(
              tabName = "Tab 4",
              active = FALSE,
              tabText1
            ),
            argonTab(
              tabName = "Tab 5",
              active = TRUE,
              tabText2
            ),
            argonTab(
              tabName = "Tab 6",
              active = FALSE,
              tabText3
            )
          )
        )
      ),
      argonTabItem(
        tabName = "alerts",
        argonRow(
          argonColumn(
            width = 4,
            argonAlert(
              icon = "basket",
              status = "danger",
              "This is an alert",
              closable = TRUE
            )
          ),
          argonColumn(
            width = 4,
            argonAlert(
              icon = "ui-02",
              status = "success",
              "This is an alert",
              closable = TRUE
            )
          ),
          argonColumn(
            width = 4,
            argonAlert(
              icon = "ui-03",
              status = "info",
              "This is an alert",
              closable = TRUE
            )
          )
        )
      ),
      argonTabItem(
        tabName = "images",
        argonRow(
          argonColumn(
            width = 6,
            argonImage(
              url = "https://www.google.com",
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/promo-1.png",
              floating = FALSE,
              card_mode = TRUE
            )
          ),
          argonColumn(
            width = 6,
            argonImage(
              url = "https://www.google.com",
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/promo-1.png",
              floating = TRUE,
              card_mode = FALSE
            )
          )
        ),
        br(),
        argonRow(
          argonCarousel(
            id = "carousel1",
            argonCarouselItem(
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/img-1-1200x1000.jpg",
              active = TRUE
            ),
            argonCarouselItem(
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/img-2-1200x1000.jpg",
              active = FALSE
            )
          ) %>% argonPersp(side = "left")
        )
      ),
      argonTabItem(
        tabName = "badges",
        argonRow(
          argonColumn(
            width = 3,
            argonBadge(
              text = "My badge",
              src = "https://www.google.com",
              pill = TRUE,
              status = "danger"
            )
          ),
          argonColumn(
            width = 3,
            argonBadge(
              text = "My badge",
              src = "https://www.google.com",
              pill = TRUE,
              status = "primary"
            )
          ),
          argonColumn(
            width = 3,
            argonBadge(
              text = "My badge",
              pill = TRUE,
              status = "warning"
            )
          ),
          argonColumn(
            width = 3,
            argonBadge(
              text = "My badge",
              src = "https://www.google.com",
              pill = FALSE,
              status = "success"
            )
          )
        )
      ),
      argonTabItem(
        tabName = "progress",
        argonProgress(value = 10, status = "danger", text = "Custom Text"),
        argonProgress(value = 40, status = "info", text = NULL),
        argonProgress(value = 90, status = "warning", text = argonIcon("atom"))
      ),
      argonTabItem(
        tabName = "effects",
        argonRow(
          argonColumn(
            width = 6, 
            argonImage(
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/ill/ill-2.svg",
              floating = TRUE,
              card_mode = TRUE
            ) %>% argonPersp(side = "left")
          )
        ) %>% argonPadding(orientation = "y", value = 5),
        argonRow(
          argonColumn(
            width = 6, 
            h1("Perspective effect"),
            h6("Disabled on small screens")
          ),
          argonColumn(
            width = 6, 
            argonImage(
              url = "https://www.google.com",
              src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/promo-1.png",
              floating = TRUE,
              card_mode = TRUE,
              hover_lift = FALSE
            ) %>% argonPersp(side = "right")
          )
        )
      ),
      argonTabItem(
        tabName = "sections",
        argonDashHeader(
          gradient = TRUE,
          color = "warning",
          separator = TRUE,
          separator_color = "info",
          top_padding = 8,
          bottom_padding = 8,
          argonCard(
            src = "https://www.google.com",
            status = "success",
            border_level = 0,
            hover_shadow = TRUE,
            title = "Card with Margins"
          ) %>% argonMargin(orientation = "t", value = -150)
        ),
        argonDashHeader(
          gradient = FALSE,
          color = "info",
          top_padding = 8,
          bottom_padding = 8,
          argonRow(
            argonColumn(
              width = 6,
              h1("Section Text"),
              h3("Some text here"),
              argonCard()
            ),
            argonColumn(
              width = 6, 
              argonCard() %>% argonMargin(orientation = "t", value = -200)
            )
          )
        ),
        argonDashHeader(
          gradient = FALSE,
          color = "secondary",
          top_padding = 8,
          bottom_padding = 8,
          mask = TRUE,
          background_img = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/img-1-1200x1000.jpg",
          opacity = 6,
          argonH1("Header with mask", display = 1) %>% argonTextColor(color = "white"),
          argonLead("This is the content.") %>% argonTextColor(color = "white")
        )
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
