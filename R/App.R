#########################################
#### A Shiny App for UCSC Xena ##########
#########################################
##### LICENSE: MIT @Openbiox   ##########
#########################################



#' Run Xena Shiny App
#' 
#' @import shiny
#' @import shinythemes
#' @return NULL
#' @export
#'
#' @examples app_run()
app_run <- function() {
  # Global definition -------------------------------------------------------

  # Here data goes
  data("XenaData", package = "UCSCXenaTools", envir = environment())
  xena_table <- XenaData[, c(
    "XenaDatasets", "XenaHostNames", "XenaCohorts",
    "SampleCount", "DataSubtype", "Label"
  )]
  xena_table$SampleCount <- as.integer(xena_table$SampleCount)
  colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")

  data("dat_datasets", package = "XenaShiny", envir = environment())
  data("dat_samples", package = "XenaShiny", envir = environment())
  data("XenaInfo", package = "XenaShiny", envir = environment())

  Data_hubs_number <- XenaInfo[["n_hubs"]]
  Cohorts_number <- XenaInfo[["n_cohorts"]]
  Datasets_number <- XenaInfo[["n_datasets"]]
  Samples_number <- XenaInfo[["n_samples"]]
  Primary_sites_number <- XenaInfo[["n_origin"]]
  Data_subtypes_number <- XenaInfo[["n_subtypes"]]

  # global color
  mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))
  # need at least 140 colors for summary plot
  mycolor <- rep(mycolor, 15)

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


  # UI ----------------------------------------------------------------------

  ui <- tagList(
    shinyjs::useShinyjs(),

    navbarPage(
      title = "",
      # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
      tabPanel(
        title = "Home",
        icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
        fluidPage(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
          ),
          fluidRow(
            column(
              12,
              tags$div(
                class = "center-block text-center bg-primary",
                tags$img(src = "xena_shiny-logo.png", alt = "logo", style = "width: 20%")
              )
            ),
            column(
              6,
              tags$div(
                column(
                  12,
                  tags$h2("Data Portal Summary"),
                  tags$a(href = "#", "XenaShiny version 0.0.0.9000 - 2019.04.04 "),
                  tags$br(),
                  tags$a(href = "https://xenabrowser.net/datapages/", "Data are controled by UCSC Xena"),
                  tags$hr(),
                  tags$div(
                    class = "card-deck text-center block-center",
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-body",
                        tags$img(src = "host.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        tags$h5("DATA HUBS")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Data_hubs_number)
                      )
                    ),
                    tags$div(
                      class = "card",
                      tags$div(
                        tags$img(src = "cohort.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        class = "card-body",
                        tags$h5("COHORTS")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Cohorts_number)
                      )
                    ),
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-body",
                        tags$img(src = "dataset.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        tags$h5("DATASETS")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Datasets_number)
                      )
                    )
                  ),
                  tags$br(),
                  tags$div(
                    class = "card-deck text-center block-center",
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-body",
                        tags$img(src = "sample.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        tags$h5("SAMPLES")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Samples_number)
                      )
                    ),
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-body",
                        tags$img(src = "site.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        tags$h5("PRIMARY SITES")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Primary_sites_number)
                      )
                    ),
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-body",
                        tags$img(src = "file.png", alt = "logo", style = "width: 30% ; margin: 0 auto "),
                        tags$h5("DATA SUBTYPES")
                      ),
                      tags$div(
                        class = "card-footer",
                        tags$h4(Data_subtypes_number)
                      )
                    )
                  )
                )
              )
            ),
            column(
              6,
              tags$br(),
              tags$br(),
              tabsetPanel(
                tabPanel(
                  "Sample Distribution",
                  tags$br(),
                  plotlyOutput("Xenasummary1")
                ),
                tabPanel(
                  "Dataset Distribution",
                  tags$br(),
                  plotlyOutput("Xenasummary")
                )
              )
            )
          ),
          tags$br(),
          tags$div(
            class = "text-center ",
            tags$div(
              class = "bg-primary",
              tags$p("The goal of XenaShiny is to provide a web app for downloading, analyzing and visulizing datasets from UCSC Xena, which is a collection of UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and others. Databases are normalized so they can be combined, linked, filtered, explored and downloaded.")
            )
          )
        )
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
              checkboxGroupInput("hubs_text", NULL,
                choiceNames = c(
                  "UCSC Public", "TCGA",
                  "GDC", "ICGC",
                  "Pan-Cancer Atlas", "TOIL",
                  "Treehouse", "PCAWG",
                  "ATAC-seq", "Single Cell"
                ),
                choiceValues = c(
                  "publicHub", "tcgaHub", "gdcHub", "icgcHub", "pancanAtlasHub",
                  "toilHub", "treehouseHub", "pcawgHub", "atacseqHub", "singlecellHub"
                )
              )
            ),

            hr(),
            actionLink("cohorts", "Cohorts Name :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
            tags$div(
              id = "cohorts_info",
              style = "padding: 8px 1px 0px",
              textInput("cohorts_text", NULL, width = "80%", placeholder = "e.g. Breast", value = NULL)
            ),


            hr(),
            actionLink("type", "Data Type :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
            tags$div(
              id = "type_info",
              style = "padding: 0px 5px 1px",
              checkboxGroupInput("type_text", NULL,
                choiceNames = c(
                  "Clinical/phenotype", "Feature (e.g. gene, proble) by sample matrix",
                  "Genomic segments", "Mutations"
                ),
                choiceValues = c("clinicalMatrix", "genomicMatrix", "genomicSegment", "mutationVector")
              )
            ),

            hr(),
            actionLink("subtype", "Data Subtype :", icon = icon("arrow-circle-right"), style = "font-size:125%"),
            tags$div(
              id = "subtype_info",
              style = "padding: 8px 1px 0px",
              textInput("subtype_text", NULL, width = "80%", placeholder = "e.g. gene expression", value = NULL)
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

      navbarMenu(
        title = "Help",
        icon = icon("question-circle"),
        tabPanel("Term List"),
        tabPanel("Repository Selections"),
        tabPanel("Help 2"),
        tabPanel("Help 3")
      ),

      tabPanel(
        title = "Developers",
        icon = icon("user-friends"),
        fluidPage(
          # titlePanel("Developers"),
          fluidRow(
            column(
              12,
              tags$div(
                class = "card-deck text-center block-center",
                tags$div(
                  class = "card bg-info",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars3.githubusercontent.com/u/25057508?s=400&v=4.png",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Shixiang Wang"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/ShixiangWang", class = "card-link", "See Profile")
                  )
                ),
                tags$div(
                  class = "card bg-warning",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars3.githubusercontent.com/u/17489298?s=400&v=4",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Fei Zhao"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/fei0810", class = "card-link", "See Profile")
                  )
                ),
                tags$div(
                  class = "card bg-info",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars0.githubusercontent.com/u/28949856?s=400&v=4",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Yi Xiong"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/Byronxy", class = "card-link", "See Profile")
                  )
                )
              ),
              tags$br(),
              tags$div(
                class = "card-deck text-center block-center",
                tags$div(
                  class = "card bg-warning",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars2.githubusercontent.com/u/37660840?s=400&v=4",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Longfei Zhao"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/longfei8533", class = "card-link", "See Profile")
                  )
                ),
                tags$div(
                  class = "card bg-info",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars0.githubusercontent.com/u/38618580?s=400&v=4",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Yin Li"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/yinlisssss", class = "card-link", "See Profile")
                  )
                ),
                tags$div(
                  class = "card bg-warning",
                  tags$div(
                    class = "card-body",
                    tags$img(
                      src = "https://avatars2.githubusercontent.com/u/22772592?s=400&v=4",
                      class = "img-circle",
                      alt = "logo", style = "width: 30% ; margin: 0 auto "
                    ),
                    tags$h4(class = "card-title", "Kai Gu"),
                    tags$p(class = "card-text", "Some information displayed")
                  ),
                  tags$div(
                    class = "card-footer",
                    tags$a(href = "https://github.com/kaigu1990", class = "card-link", "See Profile")
                  )
                )
              )
            )
          )
        )
      ),
      footer = tags$footer(HTML("Copyright &copy; 2019&nbsp;&nbsp;&nbsp;&nbsp;"), tags$a(href = "https://github.com/openbiox", "Openbiox"),
        HTML("- A community-driven bioinformatics innovation collaboration group in China |"),
        tags$a(href = "https://github.com/openbiox/XenaShiny/blob/master/LICENSE", "MIT License"),
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

    # Server - Home -----------------------------------------------------------
    output$Xenasummary <- renderPlotly({
      p <- dat_datasets %>%
        #  filter(XenaHostNames == "gdcHub") %>%
        rename(
          Hub = XenaHostNames, Percent = Sample_percent,
          Cohort = XenaCohorts, DatasetCount = N
        ) %>%
        ggplot(aes(x = Hub, y = Percent, fill = Cohort, label = DatasetCount)) +
        geom_bar(stat = "identity", width = 0.8, color = "black") +
        coord_flip() +
        labs(y = "", x = "") +
        theme_bw(base_size = 15) + # 去除背景色
        theme(panel.grid = element_blank()) + # 去除网格线
        theme(panel.border = element_blank()) + # 去除外层边框
        theme(axis.line = element_line(colour = "black")) + # 沿坐标轴显示直线
        theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + # 去除x轴
        guides(fill = F) +
        guides(color = F) +
        scale_fill_manual(values = mycolor)

      ggplotly(p, tooltip = c("fill", "label")) %>% layout(showlegend = FALSE)
    })
    output$Xenasummary1 <- renderPlotly({
      p <- dat_samples %>%
        #  filter(XenaHostNames == "gdcHub") %>%
        rename(
          Hub = XenaHostNames, Percent = SampleCount_percent,
          Cohort = XenaCohorts, SampleCount = SampleCount_sum
        ) %>%
        ggplot(aes(x = Hub, y = Percent, fill = Cohort, label = SampleCount)) +
        geom_bar(stat = "identity", width = 0.8, color = "black") +
        coord_flip() +
        labs(y = "", x = "") +
        theme_bw(base_size = 15) + # 去除背景色
        theme(panel.grid = element_blank()) + # 去除网格线
        theme(panel.border = element_blank()) + # 去除外层边框
        theme(axis.line = element_line(colour = "black")) + # 沿坐标轴显示直线
        theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + # 去除x轴
        guides(fill = F) +
        guides(color = F) +
        scale_fill_manual(values = mycolor)
      ggplotly(p, tooltip = c("fill", "label")) %>% layout(showlegend = FALSE)
    })


    # Server - Repository -----------------------------------------------------

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
      s <- input$xena_table_rows_selected
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

    # 这里使用正则表达式进行匹配的输入条件增加多个输入的支持，即用户可以选择使用
    # ,或者;对条件分割
    dataset <- reactive({
      res <- XenaData
      if (!is.null(input$hubs_text)) {
        # print(input$hubs_text)
        res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
      }
      if (!is.null(input$cohorts_text)) {
        res <- dplyr::filter(res, grepl(input$cohorts_text, XenaCohorts, ignore.case = TRUE))
      }
      if (!is.null(input$subtype_text)) {
        res <- dplyr::filter(res, grepl(input$subtype_text, DataSubtype, ignore.case = TRUE))
      }
      if (!is.null(input$type_text)) {
        res <- dplyr::filter(res, Type %in% input$type_text)
      }
      return(res)
    })

    output$xena_table <- DT::renderDataTable(dataset()[, c(
      "XenaDatasets",
      "XenaHostNames",
      "XenaCohorts",
      "SampleCount",
      "DataSubtype",
      "Label"
    )],
    selection = "single", colnames = c("Dataset", "Hub", "Cohort", "Samples", "Subtype", "Label")
    )

    selected_database <- reactive({
      s <- input$xena_table_rows_selected
      if (length(s)) {
        return(dataset()[s, ])
      }
    })


    # 我们不在详细信息这里提供下载功能
    # 而是利用Xena API提供我们已知的所有信息
    # 举例：
    # .p_dataset_metadata("https://ucscpublic.xenahubs.net", "chin2006_public/chin2006Exp_genomicMatrix")
    # 上面代码可以获取数据集的元信息，和下面链接看到的一致
    # https://xenabrowser.net/datapages/?dataset=chin2006_public%2Fchin2006Exp_genomicMatrix&host=https%3A%2F%2Fucscpublic.xenahubs.net&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443
    # 返回的是一个数据框，pmtext和text需要使用json解析，使用函数jsonlite::parse_json

    url <- reactive({
      data <- selected_database()
      xe <- XenaGenerate(subset = XenaDatasets == data$XenaDatasets)
      xe_query <- XenaQuery(xe)
      return(xe_query$url)
    })

    output$detail_info <- renderDT(selected_database(), options = list(dom = "t", scrollX = TRUE))

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
}
