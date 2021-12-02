ui.modules_pcawg_sur_plot <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(3, wellPanel(
        shinyWidgets::prettyRadioButtons( 
          inputId = ns("profile"), label = "Select a genomic profile:", 
          choiceValues = c("mRNA", "miRNA_TMM", "miRNA_UQ",  
                           "promoter_raw",
                           "promoter_outlier", 
                           "fusion",
                           "APOBEC"
          ), 
          choiceNames = c("mRNA Expression", 
                          "miRNA Expression (TMM)",  
                          "miRNA Expression (UQ)", 
                          "Raw Promoter Activity",
                          "Promoter Outlier", 
                          "Gene Fusion",
                          "APOBEC mutagenesis"
          ), 
          animation = "jelly" 
        ),
        
        selectizeInput(
          inputId = ns("item_input"),
          label = "Item:",
          choices = NULL,
          width = "100%"
        ),
        
        shinyWidgets::actionBttn(
          inputId = ns("submit_bt"), label = "Submit",
          style = "gradient",
          icon = icon("check"),
          color = "primary",
          block = TRUE,
          size = "sm"
        ),
        br(),
        htmlOutput(ns("pre_re")),
        hr(),
        h4("NOTEs:"),
        h5("The default option <Auto> will return the best p value, if you do not want to do so please choose <Custom>."),
      )),
      shinyjs::hidden(
        column(3, id = ns("parameter"), wellPanel(
          sliderInput(
            inputId = ns("age"), label = "Age",
            min = 0, max = 100, value = c(0, 100)
          ),
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("sex"), label = "Sex",
            choices = c("Female" = "female", "Male" = "male" ),
            selected = c("female", "male"),
            status = "primary",
            animation = "jelly",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.profile == 'mRNA' | input.profile == 'miRNA_TMM' | input.profile == 'miRNA_UQ'| input.profile =='APOBEC'| input.profile =='promoter_raw'",
            ns = ns,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("cutoff_mode"),
              label = "Cutoff mode",
              choices = c("Auto", "Custom"),
              inline = TRUE,
              icon = icon("check"),
              animation = "jelly"
            ),
            conditionalPanel(
              condition = "input.cutoff_mode == 'Custom'", ns = ns,
              sliderInput(
                inputId = ns("cutpoint"), label = "Cutoff (%)",
                min = 25, max = 75, value = c(50, 50)
              ),
              textOutput(ns("cutoff1")),
              textOutput(ns("cutoff2")),
              hr()
            )
          ),
          selectInput(ns("color_palette"), "Color palette:",
                      choices = c("npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons", "rickandmorty", "custom"),
                      selected = "aaas"
          ),
          conditionalPanel(
            condition = "input.color_palette == 'custom'", ns = ns,
            colourpicker::colourInput(inputId = ns("custom_col_1"), "Color for 1st group", "#0000FF"),
            colourpicker::colourInput(inputId = ns("custom_col_2"), "Color for 2nd group", "#FF0000"),
            colourpicker::colourInput(inputId = ns("custom_col_3"), "Color for 3rd group", "#BEBEBE"),
            hr()
          ),
          shinyWidgets::actionBttn(
            inputId = ns("go"), label = " GO!",
            style = "gradient",
            icon = icon("check"),
            color = "primary",
            block = TRUE,
            size = "sm"
          ),
          tags$br(),
          numericInput(inputId = ns("height"), label = "Height", value = 25),
          numericInput(inputId = ns("width"), label = "Width", value = 20),
          column(
            width = 12, align = "center",
            prettyRadioButtons(
              inputId = ns("device"),
              label = "Choose plot format",
              choices = c("png", "pdf"),
              selected = "png",
              inline = TRUE,
              icon = icon("check"),
              animation = "jelly",
              fill = TRUE
            )
          ),
          downloadBttn(
            outputId = ns("download"),
            style = "gradient",
            color = "default",
            block = TRUE,
            size = "sm"
          )
        ))
      ),
      column(
        6,
        verbatimTextOutput(ns("plot_text")),
        plotOutput(ns("surplot"), height = "600px"),
        tags$hr(),
        DT::DTOutput(outputId = ns("tbl")),
        shinyjs::hidden(
          wellPanel(
            id = ns("save_csv"),
            downloadButton(ns("downloadTable"), "Save as csv")
          )
        )
      )
    )
  )
}

server.modules_pcawg_sur_plot <- function(input, output, session) {
  ns <- session$ns
  # Global monitoring
  profile_choices <- reactive({ 
    switch(input$profile, 
           mRNA = list(all = pancan_identifiers$gene, default = "TP53"), 
           miRNA_TMM = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"), 
           miRNA_UQ = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"), 
           promoter_raw = list(all = names(load_data("pcawg_promoter_id")), default = "1:169863093:SCYL3"),
           promoter_outlier = list(all = names(load_data("pcawg_promoter_id")), default = "1:169863093:SCYL3"), 
           fusion = list(all = pancan_identifiers$gene, default = "DPM1"), 
           APOBEC = list(all = c( 
             "tCa_MutLoad_MinEstimate", "APOBECtCa_enrich", 
             "A3A_or_A3B", "APOBEC_tCa_enrich_quartile", "APOBECrtCa_enrich", 
             "APOBECytCa_enrich", "APOBECytCa_enrich-APOBECrtCa_enrich", 
             "BH_Fisher_p-value_tCa", "ntca+tgan", "rtCa_to_G+rtCa_to_T", 
             "rtca+tgay", "tCa_to_G+tCa_to_T", 
             "ytCa_rtCa_BH_Fisher_p-value", "ytCa_rtCa_Fisher_p-value", "ytCa_to_G+ytCa_to_T", 
             "ytca+tgar" 
           ), default = "APOBECtCa_enrich"), 
           list(all = "NONE", default = "NONE") 
    ) 
  }) 
  
  observe({
    updateSelectizeInput(
      session,
      "item_input",
      choices = profile_choices()$all,
      selected = profile_choices()$default,
      server = TRUE
    )
  })
  observe({
    if (is.null(input$sex)) {
      sendSweetAlert(
        session = session,
        title = "Warning !!!",
        text = "Check at least one option.",
        type = "warning"
      )
    }
  })
  
  observe({
    if (!is.null(filter_dat())) {
      if (nrow(filter_dat()) < 10) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Data is too little to analysis (<10).",
          type = "error"
        )
      }
    }
  })
  
  # Action monitoring
  
  observeEvent(input$submit_bt, {
    if (!is.null(sur_dat_pre())) {
      shinyjs::show("parameter")
    }
  })
  
  # block
  sur_dat_pre <- eventReactive(input$submit_bt, {
    val <- switch(input$profile,
                  mRNA = get_pcawg_gene_value(input$item_input),
                  miRNA_TMM = get_pcawg_miRNA_value(input$item_input, norm_method = "TMM") ,
                  miRNA_UQ = get_pcawg_miRNA_value(input$item_input, norm_method = "UQ") ,
                  fusion = get_pcawg_fusion_value(input$item_input) ,
                  promoter_raw = get_pcawg_promoter_value(input$item_input, type = "raw"),
                  promoter_outlier = get_pcawg_promoter_value(input$item_input, type = "outlier"),
                  APOBEC = get_pcawg_APOBEC_mutagenesis_value(input$item_input)
    )
    val <- val$data
    val <- na.omit(val)
    
    if(length(val)<10){
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "There is too little available data for this entry.",
        type = "error"
      )
      return(NULL)
    }
    
    val_dat <- data.frame("icgc_specimen_id" = names(val),"val" = as.numeric(val))
    
    dat <- dplyr::inner_join(pcawg_info,val_dat,by="icgc_specimen_id") %>% 
      dplyr::filter(!is.na(.data$OS.time)) %>% 
      dplyr::select( sampleID = icgc_specimen_id,
                     status = OS ,
                     time = OS.time,
                     value = val,
                     gender = donor_sex,
                     age = donor_age_at_diagnosis)
    
    if(input$profile == "fusion"){
      dat <- dplyr::mutate(dat,group = case_when(
        .data$value == 1 ~ "fusion (1)",
        .data$value == 0 ~ "non-fusion (0)"
      ))
    }
    if(input$profile == "promoter_outlier"){
      dat <- dplyr::mutate(dat,group = case_when(
        .data$value == -1 ~ "low expression (-1)",
        .data$value ==0 ~ "normal (0)",
        .data$value == 1 ~ "high expression (1)"
      ))
    }
    dat
  })
  
  filter_dat <- eventReactive(input$go, {
    dplyr::filter(sur_dat_pre(),
                  .data$age > input$age[1],
                  .data$age < input$age[2],
                  .data$gender %in% input$sex
    )
  })
  
  plot_text <- eventReactive(input$go, {
    paste(
      paste("Profile :", input$profile),
      paste("Item :", input$item_input),
      paste("Number of cases :", nrow(filter_dat())),
      sep = "\n"
    )
  })
  
  plot_func <- eventReactive(input$go, {
    if (!is.null(filter_dat())) {
      if (nrow(filter_dat()) >= 10) {
        color_palette = if (input$color_palette == "custom") {
          c(input$custom_col_1, input$custom_col_2, input$custom_col_3)
        } else {
          input$color_palette
        }
        
        if (input$profile %in% c("mRNA", "miRNA_TMM", "miRNA_UQ","promoter_raw" ,"APOBEC")) {
          p <- UCSCXenaShiny:::sur_plot(filter_dat(), input$cutoff_mode, input$cutpoint, palette = color_palette)
        } else {
          p <- UCSCXenaShiny:::p_survplot(filter_dat(), palette = color_palette)
        }
        return(p)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  return_data <- eventReactive(input$go, {
    if (!is.null(filter_dat()) & nrow(filter_dat()) >= 10) {
      shinyjs::show(id = "save_csv")
      select_data <- dplyr::select(filter_dat(), sampleID, value, status, time)
      return(select_data)
    } else {
      return(NULL)
    }
  })
  
  # output
  w <- waiter::Waiter$new(
    id = ns("surplot"), # Show waiter for surplot
    html = waiter::spin_hexdots(),
    color = "white"
  )
  
  output$cutoff1 <- renderText({
    paste("Cutoff-Low(%) :", "0 -", input$cutpoint[1])
  })
  
  output$cutoff2 <- renderText({
    paste("Cutoff-High(%): ", input$cutpoint[2], "- 100")
  })
  
  output$plot_text <- renderText(plot_text())
  
  output$surplot <- renderPlot({
    w$show() # Waiter add-ins
    plot_func()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_pcawg_surplot.", input$device)
    },
    content = function(file) {
      if(input$device == "png"){
        png(filename = file,units = "cm", width = input$width, height = input$height, res = 600)
        print(plot_func())
        dev.off()
      }
      if(input$device == "pdf"){
        pdf(file = file, onefile = FALSE)
        print(plot_func())
        dev.off()
      }
    }
  )
  
  output$tbl <- renderDT(
    return_data(),
    options = list(lengthChange = FALSE)
  )
  
  ## downloadTable
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$item_input, "_", input$profile, "_sur.csv")
    },
    content = function(file) {
      write.csv(return_data(), file, row.names = FALSE)
    }
  )
}




