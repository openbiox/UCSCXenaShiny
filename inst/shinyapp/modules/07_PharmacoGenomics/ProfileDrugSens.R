uiProfileDrugSens <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      # Select drug sensitivity database ----
      sidebarPanel(
        selectInput(inputId = ns("Select_database"), 
                    h4("Please select a drug sensitivity dataset:"), 
                    choices = c("GDSC1", "GDSC2", 
                                "CTRP1", "CTRP2",
                                "Prism", "gCSI")
        )
      ),
      mainPanel(
        column(6,
               radioButtons(inputId = ns("Select_profile_type"), 
                            strong("Visualization Types"),
                            choices = list("TSNE" = "TSNE",
                                           "MAD&MEDIAN" = "MAD"), selected = "TSNE")),
        column(6,
               selectizeInput(
                 ns("select_specific_drug"), "Drugs Selection", choices = NULL,
                 options = list(
                   placeholder = 'You can highlight targeted drug',
                   onInitialize = I('function() { this.setValue(""); }'), selected = ""
                 ))
        ),
        column(12,
               plotly::plotlyOutput(ns("p_drug_sens")),
               # p("You can interact with the above ggplotly-based plot", align = "center"))
        )
      )
    ))
}

serverProfileDrugSens <- function(input, output, session){
  ns <- session$ns
  # Drugs ----
  drugs_search3 <- reactiveValues()
  observeEvent(input$Select_database, {
    drugs_search3$drugs <- switch(input$Select_database,
                                  "CTRP1" = drugs_search2[drugs_search2$type %in% "CTRP1",]$drugs,
                                  "CTRP2" = drugs_search2[drugs_search2$type %in% "CTRP2",]$drugs,
                                  "GDSC1" = drugs_search2[drugs_search2$type %in% "GDSC1",]$drugs,
                                  "GDSC2" = drugs_search2[drugs_search2$type %in% "GDSC2",]$drugs,
                                  "gCSI" = drugs_search2[drugs_search2$type %in% "gCSI",]$drugs,
                                  "Prism" = drugs_search2[drugs_search2$type %in% "Prism",]$drugs
    )
    updateSelectizeInput(session = session, inputId = 'select_specific_drug',
                         label = 'Drugs Selection', choices = drugs_search3$drugs, server = TRUE,
                         options = list(placeholder = 'You can highlight targeted drug', onInitialize = I('function() { this.setValue(""); }')),
                         selected = ""
    )
  })
  # Plot ----
  p_drug1 <- reactive({switch(input$Select_database,
                              "GDSC1" = list(plotTSNE(gdsc1_tsne, "GDSC1"), 
                                             plotMADandMedian(gdsc1_ms, "GDSC1")
                              ),
                              "GDSC2" = list(plotTSNE(gdsc2_tsne, "GDSC2"), 
                                             plotMADandMedian(gdsc2_ms, "GDSC2")
                              ), 
                              "CTRP1" = list(plotTSNE(ctrp1_tsne, "CTRP1"), 
                                             plotMADandMedian(ctrp1_ms, "CTRP1")
                              ), 
                              "CTRP2" = list(plotTSNE(ctrp2_tsne, "CTRP2"), 
                                             plotMADandMedian(ctrp2_ms, "CTRP2")
                              ),
                              "Prism" = list(plotTSNE(prism_tsne, "PRISM"), 
                                             plotMADandMedian(prism_ms, "PRISM")
                              ), 
                              "gCSI" = list(plotTSNE(gCSI_tsne, "gCSI"), 
                                            plotMADandMedian(gCSI_ms, "gCSI")
                              )
  )})
  p_drug2 <- reactive({switch(input$Select_profile_type, 
                              "TSNE" = p_drug1()[[1]],
                              "MAD" = p_drug1()[[2]])
  })
  # highlight selected drug
  p_drug3 <- reactive({
    if(input$select_specific_drug == ""){
      p_drug2()
    } else{
      p_data1 <- p_drug2()$data
      index <- grepl(input$select_specific_drug, p_data1$Name, ignore.case = T)
      for_label <- p_data1[index,]
      p_drug2() + geom_point(
        data = for_label,
        shape = 1, stroke = 1,
        size = 4, fill = "black") 
      # geom_label_repel(
      #   aes(label = Name),
      #   data = for_label,
      #   color="black")
    }
  })
  output$p_drug_sens <- plotly::renderPlotly({
    p_drug3()
  })
}
