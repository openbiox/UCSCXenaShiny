library(shinythemes)
library(shiny)
library(r2d3)

ui <- fluidPage(theme = shinytheme('cosmo'),
                navbarPage('XenaShiny',
                           tabPanel(title = 'Home',
includeMarkdown(https://raw.githubusercontent.com/openbiox/XenaShiny/master/README.md)),
                           tabPanel(title = 'Analysis',
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput('side',
                                                    'Select',
                                                    choices = c('ab'='ab',
                                                               'bc'='bc','cd'='cd'),selected = 'ab')
                                      ),
                                      mainPanel(textOutput(outputId = 'w'))
                                    )),
                           navbarMenu(title = 'more function',
                                      tabPanel('a'),
                                      tabPanel('b'),
                                      tabPanel('b')
                           )
                )
)


server <- function(input, output, session) {
  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste('www',r)
  })
}

shinyApp(ui, server)


