
observeEvent(input$bt03, {
  updateNavbarPage(inputId = "navbar", 
    selected = "Citation")
  shinyjs::runjs("window.scrollTo(0, 0)")
})




output$slick_output <- slickR::renderSlickR({
  imgs = paste0("slick_img/N",1:6,".png")
  x = slickR::slickR(imgs, height = 600, width = "80%", slideType = 'img-lazy')  +
        slickR::settings(arrows = TRUE, dots = TRUE, autoplay = TRUE, autoplaySpeed = 2500)
  x
})

observeEvent(input$link_to_q1, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)")
  shinyjs::runjs("window.scrollTo(0, 0)")
})


observeEvent(input$link_to_q2, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA+GTEx: Molecular Profile Anatomy")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q3, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Molecule-Molecule Correlation")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q4, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Molecular Profile Cox Regression Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q5, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Between Molecular Profile and Immune Signature")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q6, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Between Molecular Profile and Tumor Immune Infiltration")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q7, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Between Molecular Profile and TMB/Stemness/MSI (Radar Show)")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q8, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Between Molecular Profile and Pathway Score")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q9, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Between Molecular Profile and Gene Mutation")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q10, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Molecular Profile Kaplan-Meier Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_q11, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Dimension Reduction Distribution")
  shinyjs::runjs("window.scrollTo(0, 0)")
})



observeEvent(input$link_to_p1, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Comparison Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_p2, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Association Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$link_to_p3, {
  updateNavbarPage(inputId = "navbar", 
    selected = "TCGA: Survival Analysis")
  shinyjs::runjs("window.scrollTo(0, 0)")
})