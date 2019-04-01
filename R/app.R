#########################################
#### A Shiny App for UCSC Xena #########
########################################
##### LICENSE: MIT @Openbiox


# Dependencies check ------------------------------------------------------

# 暂时使用开发版本的UCSCXenaTools，第一次使用先取消下面注释进行安装
# remotes::install_github("ShixiangWang/UCSCXenaTools", build_vignettes = TRUE)

pkgs <- c("shiny", "shinythemes", "UCSCXenaTools",
          "echarts4r")
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

load(file = "~/Repo/XenaShiny/data/XenaInfo.RData")

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
                                      mainPanel(textOutput(outputId = 'w'))
                                    )),
                           navbarMenu(title = 'Analyses',
                                      tabPanel('a'),
                                      tabPanel('b'),
                                      tabPanel('b')
                           ),
                           tabPanel(title = 'About',
                                    includeMarkdown("https://raw.githubusercontent.com/openbiox/XenaShiny/master/README.md"))
                )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$w <- renderText({
    req(input$side)
    r <- input$side
    paste('www',r)
  })
}


# Run web app -------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
