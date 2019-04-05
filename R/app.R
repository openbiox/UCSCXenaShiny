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
  "echarts4r", "DT"
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

ui <- navbarPage(
  title = "",
  # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  tabPanel(
    title = "Home",
    icon = icon("home") # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
  ),
  tabPanel(
    title = "Repository",
    icon = icon("database"),
    sidebarPanel(
      textInput("txt", "Text input:", "text here"),
      sliderInput("slider", "Slider input:", 1, 100, 30),
      actionButton("action", "Button"),
      actionButton("action2", "Button2", class = "btn-primary")
    ),
    mainPanel(DT::dataTableOutput("xena_table"))
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

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste("www", r)
  })
  output$xena_table <- DT::renderDataTable(DT::datatable(xena_table))
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
