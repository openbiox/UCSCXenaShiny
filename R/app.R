#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox
# setwd("~/Repo/XenaShiny/R")  # Shiny use .R as work directory
# so set this when not run app

# Dependencies check ------------------------------------------------------

# 暂时使用开发版本的UCSCXenaTools，第一次使用先取消下面注释进行安装
# remotes::install_github("ShixiangWang/UCSCXenaTools", build_vignettes = TRUE)

pkgs <- c(
  "shiny", "shinythemes", "UCSCXenaTools",
  "echarts4r", "DT", "shinyjs"
)
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    message("Installing dependencies ", "\'", pkg, "\'...")
    install.packages(pkg, dependencies = TRUE)
  }
}
# Clean variable
rm(pkgs)

# Global definition -------------------------------------------------------

# Here data goes
data("XenaData", package = "UCSCXenaTools")
xena_table <- XenaData[, c(
  "XenaDatasets", "XenaHostNames", "XenaCohorts",
  "SampleCount", "DataSubtype", "Label"
)]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")
# View(XenaData)

# xena_all = XenaData %>% XenaGenerate()
# XenaInfo = list()
# samps = list()
# for (i in 1:nrow(XenaData)) {
#   s = XenaData[i, ] %>% XenaGenerate() %>% samples(by = "datasets", how = "any")
#   message("Processing #", i)
#   samps[[i]] = s[[1]]
# }
# rm(s)
# names(samps) = XenaData$XenaDatasets
# saveRDS(samps, file = "data/XenaSamples.rds")
#
# all_chr = purrr::map_lgl(samps, is.character)
# XenaInfo$all_samples = purrr::reduce(samps[as.integer(which(all_chr))], union)
# #XenaInfo$all_samples = Reduce(union, samps)
# XenaInfo$n_samples = length(XenaInfo$all_samples)
# XenaInfo$n_hubs = length(hosts(xena_all))
# XenaInfo$n_cohorts = length(cohorts(xena_all))
# XenaInfo$n_datasets = length(datasets(xena_all))
# XenaInfo$all_samples = NULL
# save(XenaInfo, file = "data/XenaInfo.RData")

load(file = "data/XenaInfo.RData")



# Functions ---------------------------------------------------------------
fun_download <- function(datasets, destdir = tempdir(),
                         keep_structure = TRUE, force = FALSE, ...) {
  # keep_structure 设置为TRUE时，因为数据集ID带'/'，下载的多个数据集会放到不同的文件夹中
  # 设置为FALSE将会把'/'替换为'__'
  dplyr::filter(UCSCXenaTools::XenaData, XenaDatasets %in% datasets) %>%
    UCSCXenaTools::XenaGenerate() %>%
    UCSCXenaTools::XenaQuery() %>%
    UCSCXenaTools::XenaDownload(destdir = destdir, trans_slash = !keep_structure, force = force, ...)
}

# test
# fun_download("TCGA.LUAD.sampleMap/LUAD_clinicalMatrix")

# UI ----------------------------------------------------------------------

library(shiny)
library(shinyjs)

ui <- tagList(
  shinyjs::useShinyjs(),
  
  navbarPage(
    title = "",
    # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
    tabPanel(
      title = "Home",
      icon = icon("home") # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    ),
    tabPanel(
      title = "Repository",
      icon = icon("database"),
      
      sidebarLayout(
        sidebarPanel(
          hr(),
          actionLink("hubs", "Active Data Hubs :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
          tags$div(
            id = "hubs_info",
            style = "padding: 0px 5px 1px",
            checkboxGroupInput("hubs_text", NULL, choiceNames = c("UCSC Public Hub", "TCGA Hub",
                                                              "GDC Xena Hub", "ICGC Xena Hub",
                                                              "Pan-Cancer Atlas Hub", "GA4GH (TOIL) Hub",
                                                              "Treehouse Hub", "PCAWG Hub",
                                                              "ATAC-seq Hub", "Singel Cell Xena hub"),
                               choiceValues = c("publicHub", "tcgaHub", "gdcHub", "icgcHub", "pancanAtlasHub",
                                                "toilHub", "treehouseHub", "pcawgHub", "atacseqHub", "singlecellHub"))
          ),
          
          hr(),
          actionLink("cohorts", "Cohorts Name :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
          tags$div(
            id = "cohorts_info",
            style = "padding: 8px 1px 0px",
            textInput("cohorts_text", NULL, width = "80%", placeholder = "Breast", value = NULL)
          ),
          
          hr(),
          actionLink("subtype", "Data subtype :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
          tags$div(
            id = "subtype_info",
            style = "padding: 8px 1px 0px",
            textInput("subtype_text", NULL, width = "80%", placeholder = "gene expression", value = NULL)
          ),
          
          hr(),
          actionLink("type", "Type :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
          tags$div(
            id = "type_info",
            style = "padding: 0px 5px 1px",
            checkboxGroupInput("type_text", NULL, choices  = c("genomicMatrix", "clinicalMatrix", "genomicSegment", "mutationVector"))
          )
        ),
        
        mainPanel(
          DT::dataTableOutput("xena_table")
        )
      )
    ),
    
    navbarMenu(
      title = "Modules",
      icon = icon("buromobelexperte"),
      tabPanel("module 1"),
      tabPanel("module 1"),
      tabPanel("module 3")
    ),
    
    navbarMenu(
      title = "Pipelines",
      icon = icon("angle-double-down"),
      tabPanel("pipeline 1"),
      tabPanel("pipeline 2"),
      tabPanel("pipeline 3")
    ),
    tabPanel(
      title = "Developers",
      icon = icon("user-friends"),
      fluidPage(
        # titlePanel("Developers"),
        fluidRow(
          class = "text-center center-block",
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars2.githubusercontent.com/u/25057508?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Shixiang Wang"),
                tags$p(class = "card-text", "Some information displayed"),
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          ),
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars3.githubusercontent.com/u/17489298?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Fei Zhao"),
                tags$p(class = "card-text", "Some information displayed"),
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          ),
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars1.githubusercontent.com/u/28949856?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Yi Xiong"),
                tags$p(class = "card-text", "Some information displayed"),
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          )
        ),
        tags$br(),
        fluidRow(
          class = "text-center center-block",
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars3.githubusercontent.com/u/37660840?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Longfei Zhao"),
                tags$p(class = "card-text", "Some information displayed")
              ),
              tags$div(
                class = "card-footer",
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          ),
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars1.githubusercontent.com/u/38618580?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Yin Li"),
                tags$p(class = "card-text", "Some information displayed"),
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          ),
          column(
            4,
            tags$div(
              class = "card bg-info text-dark center-block",
              style = "width:400px",
              tags$img(
                class = "card-img-top  img-circle img-responsive center-block",
                src = "https://avatars3.githubusercontent.com/u/22772592?s=60&v=4", alt = "Card image"
              ),
              tags$div(
                class = "card-body",
                tags$h4(class = "card-title", "Kai Gu"),
                tags$p(class = "card-text", "Some information displayed"),
                tags$a(href = "#", class = "card-link", "See Profile")
              )
            )
          )
        )
      )
    ),
    footer = tags$footer(tags$a(href = "https://github.com/openbiox", "Openbiox"),
                         HTML(" &copy; "), tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "MIT"),
                         align = "center", style = "
                            position:relative;
                            bottom:0;
                            width:100%;
                            height:50px;   /* Height of the footer */
                            padding: 10px;
                            z-index: 1000;"
    ),
    theme = shinythemes::shinytheme("cosmo")
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$hubs, {
    toggle("hubs_info")
  })
  observeEvent(input$cohorts, {
    toggle("cohorts_info")
  })
  observeEvent(input$subtype, {
    toggle("subtype_info")
  })
  observeEvent(input$type, {
    toggle("type_info")
  })
  
  
  observe({
    s = input$xena_table_rows_selected
    if (length(s)) {
      showModal(
        modalDialog(
          title = "Detail information...",
          size = "l",
          DTOutput("detail_info"),
          hr(),
          tags$p(tags$span("Please click link to download: ", style = "font-size:110%; padding:0px 5px"), tags$a(href = url(), "Target dataset"))
        )
      )
    }
  })
  
  dataset <- reactive({
    res <- XenaData
    if (!is.null(input$hubs_text)){
      # print(input$hubs_text)
      res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
    }
    if (!is.null(input$cohorts_text)){
      res <- dplyr::filter(res, grepl(input$cohorts_text, XenaCohorts, ignore.case = TRUE))
    }
    if (!is.null(input$subtype_text)){
      res <- dplyr::filter(res, grepl(input$subtype_text, DataSubtype, ignore.case = TRUE))
    }
    if (!is.null(input$type_text)){
      res <- dplyr::filter(res, Type %in% input$type_text)
    }
    return(res)
  })
  
  output$xena_table <- DT::renderDataTable(dataset()[,c("XenaDatasets", "XenaHostNames", "XenaCohorts","SampleCount", "DataSubtype", "Label")], selection = 'single')
  
  selected_database <- reactive({
    s <- input$xena_table_rows_selected
    if (length(s)) {
      return(dataset()[s,])
    }
  })
  
  url <- reactive({
    data <- selected_database()
    xe <- XenaGenerate(subset = XenaDatasets == data$XenaDatasets)
    xe_query = XenaQuery(xe)
    return(xe_query$url)
  })
  
  output$detail_info <- renderDT(selected_database(), options = list(dom = 't', scrollX = TRUE))
  
  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste("www", r)
  })
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
