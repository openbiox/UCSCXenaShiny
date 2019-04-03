#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox
#setwd("~/Repo/XenaShiny/R")  # Shiny use .R as work directory
                             # so set this when not run app

# Dependencies check ------------------------------------------------------

# 暂时使用开发版本的UCSCXenaTools，第一次使用先取消下面注释进行安装
# remotes::install_github("ShixiangWang/UCSCXenaTools", build_vignettes = TRUE)

pkgs <- c("shiny", "shinythemes", "UCSCXenaTools",
          "echarts4r", "DT")
for (pkg in pkgs){
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
xena_table <- XenaData[, c("XenaDatasets","XenaHostNames", "XenaCohorts",
                           "SampleCount", "DataSubtype", "Label")]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")
#View(XenaData)

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
fun_download <- function(datasets, destdir=tempdir(), 
                         keep_structure = FALSE, force = FALSE, ...) {
  # keep_structure 设置为TRUE时，因为数据集ID带'/'，下载的多个数据集会放到不同的文件夹中
  # 设置为FALSE将会把'/'替换为'__'
  dplyr::filter(UCSCXenaTools::XenaData, XenaDatasets %in% datasets) %>% 
    UCSCXenaTools::XenaGenerate() %>% 
    UCSCXenaTools::XenaQuery() %>% 
    UCSCXenaTools::XenaDownload(destdir = destdir, trans_slash = keep_structure, force = force, ...)
}

# test
#fun_download("TCGA.LUAD.sampleMap/LUAD_clinicalMatrix")

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("spacelab"),
                navbarPage('XenaShiny',
                           tabPanel(title = "Home"), 
                           tabPanel(title = 'Repository',
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput('side',
                                                    'Select',
                                                    choices = c('ab'='ab',
                                                                'bc'='bc','cd'='cd'),selected = 'ab')
                                      ),
                                      mainPanel(DT::dataTableOutput("xena_table"))
                                    )),
                           navbarMenu(title = 'Analyses',
                                      tabPanel('a'),
                                      tabPanel('b'),
                                      tabPanel('b')
                           ),
                           tabPanel(title = 'About',
                                    includeMarkdown("md/about.md")),
                           tags$footer(HTML("Openbiox &copy; MIT"), align = "center", style = "
                            position:absolute;
                            bottom:0;
                            width:100%;
                            height:50px;   /* Height of the footer */
                            padding: 10px;
                            z-index: 1000;"))
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste('www',r)
  })
  output$xena_table <- DT::renderDataTable(DT::datatable(xena_table))
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
