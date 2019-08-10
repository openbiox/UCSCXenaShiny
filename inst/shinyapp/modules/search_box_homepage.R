ui.search_box_homepage <- function(id) {
  shinyWidgets::searchInput(
    inputId = id,
    label = NULL,
    btnSearch = icon("search"),
    btnReset = icon("remove"),
    placeholder = "Enter a gene symbol to show its pan-can distribution, e.g. TP53",
    width = "80%"
  )
}

# xx_mod <- function(input, output, session){
#   output$plot1 <- renderPlot({
#     plot(iris)
#   })
# }

server.search_box_homepage <- function(input, output, session) {

}
