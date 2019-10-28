ui.modules_sur_plot<- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Module: surviva analysis"),
    fluidRow(
      column(3, wellPanel(
        selectInput(ns("dataset"), "Choose a dataset:",
          choices = c(
            "LAML", "ACC", "CHOL", "BLCA", "BRCA", "CESC", "COADREAD",
            "COAD", "UCEC", "ESCA", "FPPP", "GBM", "HNSC", "KICH", "KIRC",
            "KIRP", "DLBC", "LIHC", "LGG", "GBMLGG", "LUAD", "LUNG", "LUSC",
            "SKCM", "MESO", "UVM", "OV", "PANCAN", "PAAD", "PCPG", "PRAD",
            "READ", "SARC", "STAD", "TGCT", "THYM", "THCA", "UCS"
          )
        ),
        searchInput(
          inputId = ns("gene_input"),
          label = "Gene",
          value = NULL,
          placeholder = "KDM1A",
          btnSearch = icon("search"),
          btnReset = icon("remove"),
          width = "100%"
        ),
        textOutput(ns("pre_re")),
        h4("NOTEs:"),
        h5("1. Not all dataset have clinical/pathological stages, so, in this case, the stage option is disabled."),
        h5("2. The default option <Auto> will return the best p value, if you do not want to do so please choose <Custom>.")
      )
      ),
      column(3, wellPanel(
        conditionalPanel(
          condition = "input.gene_input_search >= '1'",ns=ns,
          sliderInput(ns("age"), "Age",
            min = 0, max = 100,
            value = c(0, 100)
          ),
          checkboxGroupInput(ns("sex"), "Sex",
                             choices = c("Female"="FEMALE", "Male"="MALE", "Unknown"="Unknown"),
                             selected = c("FEMALE", "MALE","Unknown" )
                             # inline = T
          ),
          checkboxGroupInput(ns("stage"), "Clinical/Pathological stage",
                             choices = c("I", "II", "III", "IV","Unknown"),
                             selected = c("I", "II", "III", "IV", "Unknown")
            # inline = T
          ),
          radioButtons(ns("cut_off_mode"), "Cut off mode", c("Auto", "Custom"), ),
          conditionalPanel(
            condition = "input.cut_off_mode == 'Custom'",ns=ns,
            sliderInput(ns("cutpoint"), "Cut off (%)",
              min = 25, max = 75,
              value = c(50, 50)
            ),
            textOutput(ns("cutoff1")),
            textOutput(ns("cutoff2")),
            hr()
          ),
          actionButton(ns("go"), "GO!",style = "simple", 
                       color = "blue",
                       width = "100%", icon("check"))
        )
      )),
      column(6,
          plotOutput(ns("surplot"))
      )
    )
  )
}

server.modules_sur_plot <- function(input, output, session) {
  #options(shiny.sanitize.errors = TRUE)
  observeEvent(input$gene_input_search, {
    if (input$gene_input=="") {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Please add a gene.",
        type = "error"
      )
    }})
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
  sur_dat_pre <- eventReactive(input$gene_input_search,{
    progress <- shiny::Progress$new()
    progress$set(message = "Preparing data", value = 50)
    on.exit(progress$close())
    req(input$gene_input)
    sur_get(input$dataset,input$gene_input)
  })
  filter_dat <- eventReactive(input$go,{
    #req(input$age,input$sex,input$stage)
    dat_filter(data=sur_dat_pre(),age=input$age,
               gender=input$sex,stage=input$stage)
  })
  output$cutoff1 <- renderText({
    paste("Cutoff-Low(%) :","0 -",input$cutpoint[1])
  })
  output$cutoff2 <- renderText({
    paste("Cutoff-High(%): ",input$cutpoint[2],"- 100")
  })
  output$pre_re <- renderText({
    if(nrow(sur_dat_pre())>0){
      return("Ok.")
    }else{
      return("Failure.")
    }
  })
  output$surplot <- renderPlot({
    if(nrow(filter_dat())<10){
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Data is too little to analysis (<10).",
        type = "error"
      )
      NULL
    }else{
      progress <- shiny::Progress$new()
      progress$set(message = "Preparing data", value = 30)
      on.exit(progress$close())
      sur_plot(filter_dat(),input$cut_off_mode,input$cutpoint)
    }
  })
}
## Retrieve and pre-download file
sur_get <- function(TCGA_cohort, gene) {
  luad_cohort <- XenaData %>%
    filter(XenaHostNames == "tcgaHub") %>%
    .[grep(TCGA_cohort, .$XenaCohorts), ]

  cli <- luad_cohort %>%
    filter(DataSubtype == "phenotype") %>%
    XenaGenerate() %>%
    XenaQuery() %>%
    XenaDownload() %>%
    XenaPrepare()
  
  if(!("pathologic_stage") %in% names(cli) ){
    cli$pathologic_stage=NA
    }
    
  gx <- luad_cohort %>%
    dplyr::filter(DataSubtype == "gene expression RNAseq") %>%
    {
      if ("IlluminaHiSeq" %in% .$Label) {
        dplyr::filter(., Label == "IlluminaHiSeq")
      } else {
        dplyr::filter(., Label == "IlluminaHiSeq pancan normalized")
      }
    }

  gd <- fetch_dense_values(
    host = gx$XenaHosts,
    dataset = gx$XenaDatasets,
    identifiers = gene,
    use_probeMap = TRUE
  ) %>%
    .[1, ]

  merged_data <- tibble(
    sampleID = names(gd),
    gene_expression = as.numeric(gd)
  ) %>%
    left_join(cli, by = "sampleID") %>%
    dplyr::filter(sample_type == names(which.max(table(sample_type))), 
                  !is.na(OS)) %>%
    dplyr::select(sampleID, gene_expression,
      time = OS.time, status = OS,
      gender, age = age_at_initial_pathologic_diagnosis,
      stage = pathologic_stage
    ) %>%
    dplyr::mutate(stage = gsub("[(Stage)ABC ]*", "", stage)) %>% 
    dplyr::mutate(stage=ifelse(is.na(stage),"Unknown",stage),
                  gender=ifelse(is.na(gender),"Unknown", gender)) 
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

## Survaival analysis
sur_plot <- function(data, cut_off_mode, cutpoint) {
  data %<>% dplyr::arrange(gene_expression) %>%
    dplyr::mutate(per_rank = 100 / nrow(.) * (1:nrow(.)))
  if (cut_off_mode == "Auto") {
    nd <- nrow(data)
    nr <- which(data$per_rank >25 & data$per_rank <75)
    p <- c()
    for (i in nr) {
      dat <- data %>% mutate(group = c(rep("Low level", i), rep("High level",nd-i)))
      sdf <- survdiff(Surv(time, status) ~ group, data = dat)
      p.val <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
      p <- c(p, p.val)
    }
    nr <- nr[which.min(p)]
    data %<>% mutate(group = c(rep("Low level", nr), rep("High level",nd-nr)))
  } else {
    data %<>%  mutate(group = case_when(
      per_rank > !!cutpoint[2] ~ "High level",
      per_rank < !!cutpoint[1] ~ "Low level",
      TRUE ~ NA_character_
    ))
  }
  fit <- survfit(Surv(time, status) ~ group, data=data)
  ggsurvplot(fit,data=data,pval = TRUE,pval.method = TRUE,
             risk.table=TRUE,
             xlab = "Duration overall survival (days)",
             )
}
