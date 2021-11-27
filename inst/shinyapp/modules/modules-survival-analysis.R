ui.modules_sur_plot <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(3, wellPanel(
        selectInput(
          inputId = ns("dataset"), label = "Choose a dataset:",
          choices = setdiff(TCGA_datasets$id, "FPPP")
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("profile"), label = "Select a genomic profile:",
          choiceValues = c("mRNA", "transcript", "miRNA", "mutation", "cnv", "methylation", "protein"),
          choiceNames = c("mRNA Expression", "Transcript Expression", "miRNA Expression", "Mutations", "Copy Number Variation", "DNA Methylation", "Protein Expression"),
          animation = "jelly"
        ),
        shinyBS::bsPopover(ns("item_input"),
          title = "Tips",
          content = "e.g., Gene symbol: TP53; transcript: ENST00000000233; miRNA ID: hsa-miR-128-3p;",
          placement = "right", options = list(container = "body")
        ),
        shinyjs::hidden(
          shinyWidgets::textInputIcon(
            inputId = ns("item_input"),
            label = "Item:",
            value = NULL,
            placeholder = "",
            icon = icon("dna"),
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
              `live-search` = TRUE,
              style = "btn-light"
            )
          )
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
        tags$a(href = "https://pancanatlas.xenahubs.net", "Data source from Pan-Cancer Atlas Hub")
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
            status = "primary",
            animation = "jelly"
          ),
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("stage"), label = "Clinical/Pathological stage",
            choices = c("I", "II", "III", "IV", "Unknown"),
            selected = c("I", "II", "III", "IV", "Unknown"),
            status = "primary",
            animation = "jelly"
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("endpoint"),
            label = "Primary endpoint",
            choices = c("OS", "DSS", "DFI", "PFI"),
            inline = TRUE,
            icon = icon("check"),
            animation = "jelly"
          ),
          conditionalPanel(
            condition = "input.profile == 'mRNA' | input.profile == 'protein' | input.profile == 'miRNA' | input.profile == 'methylation' | input.profile == 'transcript'",
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
          conditionalPanel(
            condition = "input.profile == 'mutation'", ns = ns,
            tags$p("Note: In TCGA somatic mutation (SNP and INDEL) dataset, mutation type is represented by 1 and wild type is 0.")
          ),
          conditionalPanel(
            condition = "input.profile == 'cnv'", ns = ns,
            awesomeCheckboxGroup(
              inputId = ns("cs_cnv"),
              label = "Select CNV type.",
              choices = c("Normal", "Duplicated", "Deleted"),
              selected = c("Normal", "Duplicated", "Deleted"),
              width = "120%",
              inline = TRUE
            )
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

server.modules_sur_plot <- function(input, output, session) {
  ns <- session$ns
  # Global monitoring
  observe({
    if (input$profile == "protein") {
      shinyjs::hide(id = "item_input")
      shinyjs::show(id = "protein_input")
    } else {
      shinyjs::show(id = "item_input")
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
    if (input$profile == "gene" & input$item_input == "") {
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
      shinyjs::show("parameter")
    }
    shinyjs::hide("progress")
  })

  # block
  sur_dat_pre <- eventReactive(input$submit_bt, {
    if (input$profile == "protein") {
      tcga_surv_get(
        TCGA_cohort = input$dataset, item = input$protein_input,
        profile = input$profile, TCGA_cli_data = TCGA_cli_merged
      )
    } else {
      tcga_surv_get(
        TCGA_cohort = input$dataset, item = input$item_input,
        profile = input$profile, TCGA_cli_data = TCGA_cli_merged
      )
    }
  }, )

  filter_dat <- eventReactive(input$go, {
    if (is.null(sur_dat_pre())) {
      return(NULL)
    }
    message("cases before filtering: ", nrow(sur_dat_pre()))
    dat_filter(
      data = sur_dat_pre(), age = input$age,
      gender = input$sex, stage = input$stage,
      endpoint = input$endpoint
    )
  })
  plot_text <- eventReactive(input$go, {
    if (input$profile == "protein") {
      item_show <- input$protein_input
    } else {
      item_show <- input$item_input
    }
    paste(
      paste("Dataset :", input$dataset),
      paste("Profile :", input$profile),
      paste("Item :", item_show),
      paste("Number of cases :", nrow(filter_dat())),
      sep = "\n"
    )
  })

  plot_func <- eventReactive(input$go, {
    if (!is.null(filter_dat())) {
      if (nrow(filter_dat()) >= 10) {
        p <- tcga_surv_plot(filter_dat(),
          cutoff_mode = input$cutoff_mode,
          cutpoint = input$cutpoint,
          cnv_type = input$cs_cnv,
          profile = input$profile
        )
        if (is.null(p)) {
          sendSweetAlert(
            session = session,
            title = "Error...",
            text = "Something wrong, maybe only one genotype for this gene or bad input item.",
            type = "error"
          )
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

  output$pre_re <- renderText({
    if (is.null(sur_dat_pre())) {
      return(paste(p("Failure. The possible reason is that the gene cannot be found.", style = "color:red")))
    } else {
      return(paste(p("Next step.", style = "color:green")))
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
      paste0(Sys.Date(), "_tcga_surplot.", input$device)
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
      if (input$profile == "protein") {
        item_show <- input$protein_input
      } else {
        item_show <- input$item_input
      }
      paste0(item_show, "_", input$profile, "_sur.csv")
    },
    content = function(file) {
      write.csv(return_data(), file, row.names = FALSE)
    }
  )
}


# Functions ---------------------------------------------------------------

## Data filter
dat_filter <- function(data, age, gender, stage, endpoint) {
  endpoint.time <- paste0(endpoint, ".time")
  dat <- data %>%
    dplyr::rename(time = !!endpoint.time, status = !!endpoint) %>%
    dplyr::filter(
      age >= !!age[1],
      age <= !!age[2],
      gender %in% !!gender,
      stage %in% !!stage,
      !is.na(time),
      !is.na(status)
    )
  message("cases after filtering: ", nrow(dat))
  return(dat)
}
