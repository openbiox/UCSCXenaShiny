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

ui = navbarPage(
  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  
  tabPanel(title="Home",
           icon = icon("home") #create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
           ),
  tabPanel(title="Repository",
           icon = icon("database"),
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
  ),
  tabPanel(title="Developers",
           icon = icon("user-friends"),
           fluidPage(
             #titlePanel("Developers"),
             fluidRow(
               class ="text-center center-block",
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Shixiang Wang"),
                                   tags$p(class="card-text","Some information displayed"),
                                   tags$a(href="#",class="card-link","See Profile")
                          )
                 )
               ),
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Fei Zhao"),
                                   tags$p(class="card-text","Some information displayed"),
                                   tags$a(href="#",class="card-link","See Profile")
                          )
                 )
               ),
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Yi Xiong"),
                                   tags$p(class="card-text","Some information displayed"),
                                   tags$a(href="#",class="card-link","See Profile")
                          )
                 )
               )),
             tags$br(),
             fluidRow(
               class ="text-center center-block",
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Longfei Zhao"),
                                   tags$p(class="card-text","Some information displayed")
                          ),
                          tags$div(class="card-footer",
                                   tags$a(href="#",class="card-link","See Profile"))
                 )
               ),
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Yin Li"),
                                   tags$p(class="card-text","Some information displayed"),
                                   tags$a(href="#",class="card-link","See Profile")
                          )
                 )
               ),
               column(
                 4,
                 tags$div(class = "card bg-info text-dark center-block",
                          style="width:400px",
                          tags$img(class="card-img-top  img-circle img-responsive center-block",
                                   src="people.png",alt="Card image"),
                          tags$div(class="card-body",
                                   tags$h4(class="card-title","Kai Gu"),
                                   tags$p(class="card-text","Some information displayed"),
                                   tags$a(href="#",class="card-link","See Profile")
                                   )
                          )
               )
             )
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