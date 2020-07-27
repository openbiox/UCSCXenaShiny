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

        # tags$style(HTML(
        #   ".btn-default, .btn-default:hover, .btn-default:active, .btn-default:visited {
        #                background-color: lightgrey !important;
        #              }"
        # )),
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            inputId = ns("protein_input"),
            label = "Protein",
            choices = UCSCXenaShiny:::.all_pancan_proteins,
            selected = NULL,
            options = list(
              `live-search` = TRUE,
              style = "btn-default btn-lg;"
            )
          )
        ),

        shinyjs::hidden(
          shinyWidgets::searchInput(
            inputId = ns("gene_input"),
            label = "Gene",
            value = NULL,
            placeholder = "KDM1A",
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "100%"
          )
        ),

        shinyjs::hidden(
          tags$div(
            id = ns("progress"),
            shinyWidgets::progressBar(
              id = ns("progressbar"), value = 50
            )
          )
        ),
        htmlOutput(ns("pre_re")),

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
        plotOutput(ns("surplot"), height = "500px")
      )
    )
  )
}

server.modules_sur_plot <- function(input, output, session) {
  ns <- session$ns
  # options(shiny.sanitize.errors = TRUE)
  observeEvent(input$gene_input_search, {
    if (input$gene_input == "") {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Please add a gene.",
        type = "error"
      )
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

  sur_dat_pre <- eventReactive(input$gene_input_search, {
    req(input$gene_input)
    tryCatch(sur_get(
      TCGA_cohort = input$dataset, gene = input$gene_input,
      profile = input$profiles
    ), error = function(e) {
      NULL
    })
  })
  # sur_dat_pre <- reactive({
  #   if (!is.null(input$gene_input_search)) {
  #     req(input$gene_input)
  #     tryCatch(sur_get(input$dataset, input$gene_input, input$profiles), error = function(e){NULL})
  #   }else if (!is.null(input$protein_input)) {
  #     tryCatch(sur_get(input$dataset, input$protein_input, input$profiles), error = function(e){NULL})
  #   }
  # })
  sur_dat_pre_protein <- eventReactive(input$protein_input, {
    tryCatch(sur_get(
      TCGA_cohort = input$dataset, protein = input$protein_input,
      profile = input$profiles
    ), error = function(e) {
      NULL
    })
  })

  observe({
    if (input$profiles == "protein") {
      shinyjs::hide(id = "gene_input")
      shinyjs::show(id = "protein_input")
    } else {
      shinyjs::show(id = "gene_input")
      shinyjs::hide(id = "protein_input")
    }
  })

  observeEvent(input$gene_input_search, {
    shinyjs::show("progress")
    if (!is.null(sur_dat_pre())) {
      updateProgressBar(session = session, id = ns("progressbar"), value = 100)
      shinyjs::hide("progress")
      shinyjs::show("parameter")
    }
  })

  observeEvent(
    {
      if (input$profiles == "protein") {
        TRUE
      } else {
        return()
      }
    },
    {
      shinyjs::show("progress")
      if (!is.null(sur_dat_pre_protein())) {
        updateProgressBar(session = session, id = ns("progressbar"), value = 100)
        shinyjs::hide("progress")
        shinyjs::show("parameter")
      }
    }
  )

  filter_dat <- eventReactive(input$go, {
    # req(input$age,input$sex,input$stage)
    if (input$profiles == "protein") {
      dat_filter(
        data = sur_dat_pre_protein(), age = input$age,
        gender = input$sex, stage = input$stage
      )
    } else {
      dat_filter(
        data = sur_dat_pre(), age = input$age,
        gender = input$sex, stage = input$stage
      )
    }
  })

  output$cutoff1 <- renderText({
    paste("Cutoff-Low(%) :", "0 -", input$cutpoint[1])
  })
  output$cutoff2 <- renderText({
    paste("Cutoff-High(%): ", input$cutpoint[2], "- 100")
  })

  output$pre_re <- renderText({
    if (is.null(sur_dat_pre())) {
      return(paste(p("Can't find this gene in dataset.", style = "color:red")))
    } else if (nrow(sur_dat_pre()) > 0) {
      return(paste(p("Ok.", style = "color:green")))
    } else {
      return(paste(p("Failure.", style = "color:red")))
    }
  })

  plot_func <- reactive({
    if (nrow(filter_dat()) >= 10) {
      if (input$profiles == "mRNA") {
        p <- sur_plot_mRNA(filter_dat(), input$cut_off_mode, input$cutpoint)
      } else if (input$profiles == "mutation") {
        p <- sur_plot_mut(filter_dat())
      } else if (input$profiles == "protein") {
        p <- sur_plot_protein(filter_dat(), input$cut_off_mode, input$cutpoint)
      }
      return(p)
    }else{
      return(NULL)
    }
  })

  # Show waiter for surplot
  w <- waiter::Waiter$new(id = ns("surplot"), html = waiter::spin_hexdots(), color = "white")

  output$surplot <- renderPlot({
    if (nrow(filter_dat()) < 10) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Data is too little to analysis (<10).",
        type = "error"
      )
      NULL
    } else {
      w$show() # Waiter add-ins
      plot_func()
    }
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
## Retrieve and pre-download file
sur_get <- function(TCGA_cohort, gene = NULL, protein = NULL, profile) {
  luad_cohort <- XenaData %>%
    filter(XenaHostNames == "tcgaHub") %>%
    .[grep(TCGA_cohort, .$XenaCohorts), ]

  data("tcga_clinicalMatrix", package = "UCSCXenaShiny", envir = environment())
  cliMat <- dplyr::filter(cliMat, type == TCGA_cohort) %>%
    dplyr::rename(pathologic_stage = ajcc_pathologic_tumor_stage)

  if (!("pathologic_stage") %in% names(cliMat)) {
    cli$pathologic_stage <- NA
  }

  # gx <- luad_cohort %>%
  #   dplyr::filter(DataSubtype == "gene expression RNAseq") %>%
  #   {
  #     if ("IlluminaHiSeq" %in% .$Label) {
  #       dplyr::filter(., Label == "IlluminaHiSeq")
  #     } else {
  #       dplyr::filter(., Label == "IlluminaHiSeq pancan normalized")
  #     }
  #   }

  if (profile == "mRNA") {
    gd <- get_pancan_gene_value(gene)$expression
    gd <- gd[nchar(names(gd)) == 15]
  } else if (profile == "mutation") {
    gd <- get_pancan_mutation_status(gene)
    gd <- gd[nchar(names(gd)) == 15]
  } else if (profile == "protein") {
    gd <- get_pancan_protein_value(protein)$expression
    gd <- gd[nchar(names(gd)) == 15]
  }

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
      stage = pathologic_stage
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

## Survaival analysis for mRNA expression
sur_plot_mRNA <- function(data, cut_off_mode, cutpoint) {
  data %<>% dplyr::rename(gene_expression = value) %>%
    dplyr::arrange(gene_expression) %>%
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
      per_rank < !!cutpoint[1] ~ "Low",
      TRUE ~ NA_character_
    ))
  }
  p_survplot(data)
  # fit <- survfit(Surv(time, status) ~ group, data = data)
  # ggsurvplot(fit,
  #   data = data, pval = TRUE, pval.method = TRUE,
  #   risk.table = TRUE,
  #   xlab = "Duration overall survival (days)",
  # )
}

## Survaival analysis for mutation DNA
sur_plot_mut <- function(data) {
  data %<>% dplyr::rename(mut = value) %>%
    mutate(group = ifelse(mut == 1, "MT", "WT"))

  p_survplot(data)
}

## Survaival analysis for protein expression
sur_plot_protein <- function(data, cut_off_mode, cutpoint) {
  data %<>% dplyr::rename(protein_expression = value) %>%
    dplyr::arrange(protein_expression) %>%
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
      per_rank < !!cutpoint[1] ~ "Low",
      TRUE ~ NA_character_
    ))
  }
  p_survplot(data)
}


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
