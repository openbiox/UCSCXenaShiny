
observeEvent(input$bt03, {
  updateNavbarPage(inputId = "navbar", 
    selected = "Citation")
  shinyjs::runjs("window.scrollTo(0, 0)")
})




output$slick_output <- slickR::renderSlickR({
  # imgs = paste0("slick_img/N",1:6,".png")
  # system.file("shinyapp/www", package = "UCSCXenaShiny")
  imgs = paste0(system.file("shinyapp/www", package = "UCSCXenaShiny"),"/slick_img/N",1:6,".png")
  x = slickR::slickR(imgs, height = 600, width = "80%")  + #, slideType = 'img-lazy'
        slickR::settings(arrows = TRUE, dots = TRUE, autoplay = TRUE, autoplaySpeed = 2500)
  x
})


observeEvent(input$link_to_q1_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA (GTEx): Molecular comparison")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q2_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Molecular correlation")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q3_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Survival analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})


observeEvent(input$link_to_p1_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Comparison Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_p2_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Correlation Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_p3_n, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Survival Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})