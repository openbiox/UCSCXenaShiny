dataset <- dplyr::filter(XenaData, XenaHostNames == "tcgaHub") %>%
  dplyr::select("XenaCohorts") %>%
  unique() %>%
  dplyr::mutate(
    id = stringr::str_match(XenaCohorts, "\\((\\w+?)\\)")[, 2],
    des = stringr::str_match(XenaCohorts, "(.*)\\s+\\(")[, 2]
  ) %>%
  dplyr::arrange(id)

ui.modules_sur_plot <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Module: Surviva Analysis"),
    fluidRow(
      column(3, wellPanel(
        selectInput(
          inputId = ns("dataset"), label = "Choose a dataset:",
          choices = dataset$id
        ),
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("profiles"), label = "Select a genomic profiles:",
          choiceValues = c("mRNA", "mutation", "cnv", "protein"),
          choiceNames = c("mRNA Expression", "Mutations", "Copy-number alterations from GISTIC", "Protein Expression"),
          animation = "jelly"
        ),
        shinyjs::hidden(
          # shinyWidgets::searchInput(
          shinyWidgets::textInputAddon(
            inputId = ns("gene_input"),
            label = "Gene",
            value = NULL,
            placeholder = "KDM1A",
            addon = icon("dna"),
            # btnSearch = icon("search"),
            # btnReset = icon("remove"),
            width = "100%"
          )
        ),
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            inputId = ns("protein_input"),
            label = "Protein",
            choices = UCSCXenaShiny:::.all_pancan_proteins,
            selected = NULL,
            options = list(
              liveSearch = TRUE,
              style = "btn-default btn-lg;"
            )
          )
        ),
        shinyWidgets::actionBttn(
          inputId = ns("submit_bt"), label = "Submit",
          style = "gradient",
          icon = icon("check"),
          color = "default",
          block = TRUE,
          size = "sm"
        ),
        br(),
        shinyjs::hidden(
          tags$div(
            id = ns("progress"),
            shinyWidgets::progressBar(
              id = ns("progressbar"), value = 70
            )
          )
        ),
        htmlOutput(ns("pre_re")),
        hr(),
        h4("NOTEs:"),
        h5("1. Not all dataset have clinical/pathological stages, so, in this case, the stage option is disabled."),
        h5("2. The default option <Auto> will return the best p value, if you do not want to do so please choose <Custom>."),
        tags$a(href = "https://tcga.xenahubs.net", "Data source from TCGA hub")
      )),
      
      shinyjs::hidden(
        column(3, id = ns("parameter"), wellPanel(
          sliderInput(
            inputId = ns("age"), label = "Age",
            min = 0, max = 100, value = c(0, 100)
          ),
          
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("sex"), label = "Sex",
            choices = c("Female" = "FEMALE", "Male" = "MALE", "Unknown" = "Unknown"),
            selected = c("FEMALE", "MALE", "Unknown"),
            # icon = icon("check-square-o"),
            status = "primary",
            animation = "jelly"
          ),
          
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("stage"), label = "Clinical/Pathological stage",
            choices = c("I", "II", "III", "IV", "Unknown"),
            selected = c("I", "II", "III", "IV", "Unknown"),
            # icon = icon("check"),
            status = "primary",
            animation = "jelly"
          ),
          
          conditionalPanel(
            condition = "input.profiles == 'mRNA' | input.profiles == 'protein'", ns = ns,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("cut_off_mode"),
              label = "Cut off mode",
              choices = c("Auto", "Custom"),
              inline = TRUE,
              icon = icon("check"),
              animation = "jelly"
            ),
            conditionalPanel(
              condition = "input.cut_off_mode == 'Custom'", ns = ns,
              sliderInput(
                inputId = ns("cutpoint"), label = "Cut off (%)",
                min = 25, max = 75, value = c(50, 50)
              ),
              textOutput(ns("cutoff1")),
              textOutput(ns("cutoff2")),
              hr()
            )
          ),
          
          conditionalPanel(
            condition = "input.profiles == 'mutation'", ns = ns,
            tags$p("Note: In TCGA somatic mutation (SNP and INDEL) dataset, mutation type is represented by 1 and wild type is 0.")
          ),
          
          shinyWidgets::actionBttn(
            inputId = ns("go"), label = " GO!",
            style = "gradient",
            icon = icon("check"),
            color = "default",
            block = TRUE,
            size = "sm"
          ),
          
          tags$hr(),
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
            # label = "Download Plot",
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
        plotOutput(ns("surplot"), height = "500px")
      )
    )
  )
}

server.modules_sur_plot <- function(input, output, session) {
  ns <- session$ns
  # Global monitoring
  observe({
    if (input$profiles == "protein") {
      shinyjs::hide(id = "gene_input")
      shinyjs::show(id = "protein_input")
    } else {
      shinyjs::show(id = "gene_input")
      shinyjs::hide(id = "protein_input")
    }
  })
  
  observe({
    if (is.null(input$sex) | is.null(input$stage)) {
      sendSweetAlert(
        session = session,
        title = "Warning !!!",
        text = "Check at least one option.",
        type = "warning"
      )
    }
  })
  observe({
    if(!is.null(filter_dat())){
      if(nrow(filter_dat())<10){
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Data is too little to analysis (<10).",
          type = "error")
      }
    }
  })
  
  # Action monitoring
  observeEvent(input$submit_bt, {
    if (input$profiles == "gene" & input$gene_input == "") {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Please add a gene.",
        type = "error"
      )
    }
  })
  
  observeEvent(input$submit_bt, {
    shinyjs::show("progress")
    if (!is.null(sur_dat_pre())) {
      # updateProgressBar(session = session, id = ns("progressbar"), value = 70)
      shinyjs::show("parameter")
    }
    shinyjs::hide("progress")
  })
  
  # block
  sur_dat_pre <- eventReactive(input$submit_bt, {
    if (input$profiles == "protein") {
      sur_get(
        TCGA_cohort = input$dataset, item = input$protein_input,
        profile = input$profiles
      )
    } else {
      sur_get(
        TCGA_cohort = input$dataset, item = input$gene_input,
        profile = input$profiles
      )
    }
  }, )
  
  filter_dat <- eventReactive(input$go, {
    # req(input$age,input$sex,input$stage)
    if (is.null(sur_dat_pre())) {
      return(NULL)
    }
    dat_filter(
      data = sur_dat_pre(), age = input$age,
      gender = input$sex, stage = input$stage
    )
  })
  plot_text <- eventReactive(input$go,{
    if (input$profiles == "protein") {
      item_show <- input$protein_input
    } else {
      item_show <- input$gene_input
    }
    paste(
      paste("Dataset :", input$dataset),
      paste("Profiles :", input$profiles),
      paste("Item :", item_show),
      paste("Number of cases :", nrow(sur_dat_pre())),
      sep = "\n"
    )
  }
  )
  
  plot_func <- eventReactive(input$go,{
    if (!is.null(filter_dat())) {
      if(nrow(filter_dat()) >= 10){
        if (input$profiles == "mRNA" | input$profiles == "protein") {
          p <- sur_plot_mRNA_protein(filter_dat(), input$cut_off_mode, input$cutpoint)
        } else if (input$profiles == "mutation") {
          p <- sur_plot_mut(filter_dat())
        }
        return(p)
      }else{
        return(NULL)}
    }else{
      return(NULL)
    }
  })
  
  # output
  w <- waiter::Waiter$new(id = ns("surplot"), # Show waiter for surplot
                          html = waiter::spin_hexdots(), 
                          color = "white")
  
  output$pre_re <- renderText({
    if (is.null(sur_dat_pre())) {
      return(paste(p("Failure. The possible reason is that the gene cannot be found.", style = "color:red")))
    } else {
      return(paste(p("Next setp.", style = "color:green")))
    }
  })
  
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
      paste0("surplot.", input$device)
    },
    content = function(file) {
      p <- plot_func()
      ggplot2::ggsave(
        filename = file, plot = print(p, newpage = F), device = input$device,
        units = "cm", width = 20, height = 15, dpi = 600
      )
    }
  )
}

# function ---------------------------------------------------------------------------

## Retrieve and pre-download file
sur_get <- function(TCGA_cohort, item, profile) {
  luad_cohort <- XenaData %>%
    filter(XenaHostNames == "tcgaHub") %>%
    .[grep(TCGA_cohort, .$XenaCohorts), ]
  
  data("tcga_clinicalMatrix", package = "UCSCXenaShiny", envir = environment())
  cliMat <- dplyr::filter(cliMat, type == TCGA_cohort)
  
  if (profile == "mRNA") {
    gd <- get_pancan_gene_value(item)$expression
  } else if (profile == "mutation") {
    gd <- get_pancan_mutation_status(item)
  } else if (profile == "protein") {
    gd <- get_pancan_protein_value(item)$expression
  }
  if (all(is.na(gd))) {
    return(NULL)
  }
  gd <- gd[nchar(names(gd)) == 15]
  merged_data <- tibble(
    sampleID = names(gd),
    value = as.numeric(gd),
    patid = substr(sampleID, 1, 12)
  ) %>%
    dplyr::filter(as.numeric(substr(sampleID, 14, 15)) < 10) %>%
    dplyr::left_join(cliMat, by = c("patid" = "bcr_patient_barcode")) %>%
    dplyr::filter(
      !is.na(OS),
      !is.na(OS.time)
    ) %>%
    dplyr::select(sampleID, value,
                  time = OS.time, status = OS,
                  gender, age = age_at_initial_pathologic_diagnosis,
                  stage = ajcc_pathologic_tumor_stage
    ) %>%
    # dplyr::mutate(stage = gsub("[(Stage)ABC ]*", "", stage)) %>%
    dplyr::mutate(stage = stringr::str_match(stage, "Stage\\s+(.*?)[ABC]?$")[, 2]) %>%
    dplyr::mutate(
      stage = ifelse(is.na(stage), "Unknown", stage),
      gender = ifelse(is.na(gender), "Unknown", gender)
    )
  return(merged_data)
}

## Data filter
dat_filter <- function(data, age, gender, stage) {
  dat <- data %>% dplyr::filter(
    age > !!age[1],
    age < !!age[2],
    gender %in% !!gender,
    stage %in% !!stage
  )
  return(dat)
}

## Survaival analysis for mRNA and protein expression
sur_plot_mRNA_protein <- function(data, cut_off_mode, cutpoint) {
  data %<>% dplyr::arrange(value) %>%
    dplyr::mutate(per_rank = 100 / nrow(.) * (1:nrow(.)))
  if (cut_off_mode == "Auto") {
    nd <- nrow(data)
    nr <- which(data$per_rank > 25 & data$per_rank < 75)
    p <- c()
    for (i in nr) {
      dat <- data %>% mutate(group = c(rep("Low", i), rep("High", nd - i)))
      sdf <- survdiff(Surv(time, status) ~ group, data = dat)
      p.val <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
      p <- c(p, p.val)
    }
    nr <- nr[which.min(p)]
    data %<>% mutate(group = c(rep("Low", nr), rep("High", nd - nr)))
  } else {
    data %<>% mutate(group = case_when(
      per_rank > !!cutpoint[2] ~ "High",
      per_rank <= !!cutpoint[1] ~ "Low",
      TRUE ~ NA_character_
    ))
  }
  p_survplot(data)
}

## Survaival analysis for mutation DNA
sur_plot_mut <- function(data) {
  data %<>% dplyr::rename(mut = value) %>%
    mutate(group = ifelse(mut == 1, "MT", "WT"))
  p_survplot(data)
}

## ggsurvplot
p_survplot <- function(data) {
  fit <- survfit(Surv(time, status) ~ group, data = data)
  ggsurvplot(fit,
             data = data, pval = TRUE, pval.method = TRUE,
             size = 1.2, # change line size
             risk.table = TRUE,
             risk.table.col = "strata", # Risk table color by groups
             risk.table.y.text = FALSE, # show bars instead of names in text annotations
             xlab = "Duration overall survival (days)",
             ggtheme = theme_classic(), # Change ggplot2 theme
             ncensor.plot = TRUE, # plot the number of censored subjects at time t
             ncensor.plot.height = 0.20
  )
}
