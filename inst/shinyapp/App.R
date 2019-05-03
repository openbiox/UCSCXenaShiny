# Prepare -----------------------------------------------------------------
library(shiny)
library(UCSCXenaTools)
library(shinyBS)

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
    title=div(
      img(src="xena_shiny-logo_white.png",height = 49.6,style="margin:-20px -15px -15px -15px")),
    # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
    
    # Home page ---------------------------------------------------------------
    tabPanel(
      title = "Home",
      icon = icon("home"), # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap4.css")
        ),
        fluidRow(
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
                plotly::plotlyOutput("Xenasummary1")
              ),
              tabPanel(
                "Dataset Distribution",
                tags$br(),
                plotly::plotlyOutput("Xenasummary")
              )
            )
          )
        ),
        tags$br(),
        tags$div(
          class = "text-center",
          tags$div(
            class = "bg-dark text-white",
            tags$p("The goal of XenaShiny is to provide a web app for downloading, analyzing and visulizing datasets from UCSC Xena, which is a collection of UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and others. Databases are normalized so they can be combined, linked, filtered, explored and downloaded.")
          )
        )
      )
    ),
    tabPanel(
      title = "Repository",
      icon = icon("database"),
      
      # Set checkbox css
      tags$head(
        tags$style(
          HTML(
            ".checkbox-inline { 
            margin-left: 0px;
            margin-right: 10px;
            }
            .checkbox-inline+.checkbox-inline {
            margin-left: 0px;
            margin-right: 10px;
            }
            .shiny-notification {
            position:fixed;
            top: calc(50% + 120px);;
            left: calc(50% + 250px);;
            }
            "
          )
        ) 
      ),
      
      fluidPage(
        br(),
        
        column(
          width = 3,
          tags$div(
            id = "hubs_info",
            # style = "padding: 0px 5px 1px;border: 1px solid #EEE;border-radius: 3px;margin-bottom: 20px;",
            style = "padding: 0px 5px 1px;margin-bottom: 0px;",
            checkboxGroupInput("hubs_text", h4("Active Data Hubs :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"), 
                               inline = TRUE,
                               width = "80%",
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
            ),
            # add popover on UI
            shinyBS::bsPopover("hubs_text", title = "Tips", content = "待补充................", placement = "right", options = list(container = "body"))
            # bsTooltip("hubs_text", "The wait times will be broken into this many equally spaced bins",
            #           "right", options = list(container = "body"))
          ),
          
          tags$div(
            id = "cohorts_info",
            style = "padding: 8px 1px 0px;margin-bottom: 25px;",
            tags$style(type='text/css','#cohorts_text {height: 35px;}'),
            textInput("cohorts_text", h4("Cohorts Name :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"), 
                      width = "80%", placeholder = "e.g. Breast", value = NULL),
            shinyBS::bsPopover("cohorts_text", title = "Tips", content = "待补充................", placement = "right", options = list(container = "body"))
          ),
          
          tags$div(
            id = "type_info",
            style = "padding: 0px 5px 1px;margin-bottom: 0px;",
            checkboxGroupInput("type_text", h4("Data Type :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
                               choiceNames = c("Clinical/phenotype", "Feature (e.g. gene, proble) by sample matrix",
                                               "Genomic segments", "Mutations"), 
                               choiceValues = c("clinicalMatrix", "genomicMatrix", "genomicSegment", "mutationVector")
            ),
            shinyBS::bsPopover("type_text", title = "Tips", content = "待补充................", placement = "right", options = list(container = "body"))
          ),
          
          tags$div(
            id = "subtype_info",
            style = "padding: 8px 1px 0px;margin-bottom: 25px;",
            tags$style(type='text/css','#subtype_text {height: 35px;}'),
            textInput("subtype_text", h4("Data Subtype :", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"), 
                      width = "80%", placeholder = "e.g. gene expression", value = NULL),
            shinyBS::bsPopover("subtype_text", title = "Tips", content = "待补充................", placement = "right", options = list(container = "body"))
          )
        ),
        
        column(
          width = 9,
          DT::dataTableOutput("xena_table"),
          
          hr(),
          
          actionButton(inputId = "show_met", label = "Show Metadata", icon = icon("database"), style = "margin-bottom: 10px; margin-left: 25px;"),
          actionButton(inputId = "req_data", label = "Request Data", icon = icon("file"), style = "margin-bottom: 10px; margin-left: 75px;"),
          
          br(),
          
          shinyjs::hidden(
            tags$div(
              id = "show_data",
              h4("Data:", style = "font-size: 1.2em;font-weight: bold;margin-bottom: 5px;margin-top: 0;"),
              tableOutput(
                "table"
              )
            )
          )
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
  output$Xenasummary <- plotly::renderPlotly({
    p <- dat_datasets %>%
      #  filter(XenaHostNames == "gdcHub") %>%
      dplyr::rename(
        Hub = XenaHostNames, Percent = Sample_percent,
        Cohort = XenaCohorts, DatasetCount = N
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = DatasetCount)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
      ggplot2::coord_flip() +
      ggplot2::labs(y = "", x = "") +
      ggplot2::theme_bw(base_size = 15) + # 去除背景色
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
      ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
      ggplot2::theme(axis.line.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_blank()) + # 去除x轴
      ggplot2::guides(fill = F) +
      ggplot2::guides(color = F) +
      ggplot2::scale_fill_manual(values = mycolor)
    
    plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
  })
  output$Xenasummary1 <- plotly::renderPlotly({
    p <- dat_samples %>%
      #  filter(XenaHostNames == "gdcHub") %>%
      dplyr::rename(
        Hub = XenaHostNames, Percent = SampleCount_percent,
        Cohort = XenaCohorts, SampleCount = SampleCount_sum
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = SampleCount)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
      ggplot2::coord_flip() +
      ggplot2::labs(y = "", x = "") +
      ggplot2::theme_bw(base_size = 15) + # 去除背景色
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
      ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
      ggplot2::theme(axis.line.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_blank()) + # 去除x轴
      ggplot2::guides(fill = F) +
      ggplot2::guides(color = F) +
      ggplot2::scale_fill_manual(values = mycolor)
    plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
  })
  
  
  # Server - Repository -----------------------------------------------------
  
  # 这里使用正则表达式进行匹配的输入条件增加多个输入的支持，即用户可以选择使用
  # ,或者;对条件分割
  # **已修改**，使用‘;’分隔符进行多条件筛选
  dataset <- reactive({
    res <- XenaData
    
    if (!is.null(input$hubs_text)) {
      res <- dplyr::filter(res, XenaHostNames %in% input$hubs_text)
    }
    if (!is.null(input$cohorts_text) & input$cohorts_text != "") {
      ids <- sub(";", "|", input$cohorts_text)
      res <- dplyr::filter(res, grepl(ids, XenaCohorts, ignore.case = TRUE))
    }
    if (!is.null(input$subtype_text) & input$subtype_text != "") {
      ids <- sub(";", "|", input$subtype_text)
      res <- dplyr::filter(res, grepl(ids, DataSubtype, ignore.case = TRUE))
    }
    if (!is.null(input$type_text)) {
      res <- dplyr::filter(res, Type %in% input$type_text)
    }
    
    return(res)
  })
  
  # Show database information in repository page
  output$xena_table <- DT::renderDataTable(dataset()[, c("XenaDatasets",
                                                         "XenaHostNames",
                                                         "XenaCohorts",
                                                         "SampleCount",
                                                         "DataSubtype",
                                                         "Label")],
                                           colnames=c("Dataset", "Hub", "Cohort", "Samples", "Subtype", "Label"))
  
  # Keep selected database
  selected_database <- reactive({
    s <- input$xena_table_rows_selected
    if (length(s)) {
      return(dataset()[s, ])
    }
  })
  
  query_url <- reactive({
    s <- input$xena_table_rows_selected
    if (!is.null(s)){
      data <- selected_database()
      xe <- UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)
      xe_query <- UCSCXenaTools::XenaQuery(xe)
      return(xe_query)
    }
  })
  
  # Show download url for selected database
  observe({
    s <- input$xena_table_rows_selected
    if (!is.null(s)){
      data <- selected_database()
      
      output$table <- renderTable({
        data.frame(ID = 1:length(s), XenaCohorts = data$XenaCohorts, Label = data$Label, URL = unlist(lapply(query_url()$url, function(x){as.character(tags$a(href = x, x))})))
      }, sanitize.text.function = function(x) x)
      
      shinyjs::show("show_data")
    }else{
      shinyjs::hide("show_data")
    }
  })
  
  # Dialog for showing selected data information
  observeEvent(input$show_met, {
    s <- input$xena_table_rows_selected
    if (length(s)) {
      showModal(
        modalDialog(
          title = "Detail information...",
          size = "l",
          DT::DTOutput("detail_info")
        )
      )
    }
  })
  
  # Dialog for some operations of request data
  observeEvent(input$req_data, {
    s <- input$xena_table_rows_selected
    if (length(s)) {
      showModal(
        modalDialog(
          title = "Submitted database...",
          size = "l",
          DT::DTOutput(
            "table_query"
          ),
          
          hr(),
          
          actionButton(inputId = "load", label = "Load Data", icon = icon("upload"), style = "margin-bottom: 10px; margin-right: 75px;"),
          shinyBS::bsPopover("load", title = "Tips", content = "待补充................", placement = "bottom", options = list(container = "body")),
          downloadButton(outputId  = "download", label = "Download Data", icon = icon("download"), style = "margin-bottom: 10px;"),
          shinyBS::bsPopover("download", title = "Tips", content = "待补充................", placement = "bottom", options = list(container = "body"))
        )
      )
      
      # data <- selected_database()
      # xe <- UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)
      # xe_query <- UCSCXenaTools::XenaQuery(xe)
      output$table_query <- DT::renderDT(query_url(), options = list(dom = "t", scrollX = TRUE))
    }
  })
  
  # 我们不在详细信息这里提供下载功能
  # 而是利用Xena API提供我们已知的所有信息
  # 举例：
  # .p_dataset_metadata("https://ucscpublic.xenahubs.net", "chin2006_public/chin2006Exp_genomicMatrix")
  # 上面代码可以获取数据集的元信息，和下面链接看到的一致
  # https://xenabrowser.net/datapages/?dataset=chin2006_public%2Fchin2006Exp_genomicMatrix&host=https%3A%2F%2Fucscpublic.xenahubs.net&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443
  # 返回的是一个数据框，pmtext和text需要使用json解析，使用函数jsonlite::parse_json
  # **已修改**
  
  observeEvent(input$show_met, {
    s <- input$xena_table_rows_selected
    if (length(s)){
      m <- matrix(NA, nrow = length(s), ncol = 11)
      
      j = 0
      for (i in s) {
        j <- j + 1
        temp <- .p_dataset_metadata(as.character(dataset()[i, "XenaHosts"]), as.character(dataset()[i, "XenaDatasets"]))
        # temp <- .p_dataset_metadata(as.character(XenaData[i, "XenaHosts"]), as.character(XenaData[i, "XenaDatasets"]))
        
        title <- temp$longtitle
        dataset_ID <- temp$name
        samples <- temp$count
        
        if (is.na(temp$pmtext)){
          version <- NA
        }else{
          version <- jsonlite::parse_json(temp$pmtext)$version
        }
        
        cohort <- jsonlite::parse_json(temp$text)$cohort
        type_of_data <- jsonlite::parse_json(temp$text)$dataSubType
        ID_Mapping <- paste0("https://ucscpublic.xenahubs.net/download/", jsonlite::parse_json(temp$text)$probeMap)
        
        publication <- jsonlite::parse_json(temp$text)$articletitle
        if (is.null(publication)) publication <- NA 
        
        citation <- jsonlite::parse_json(temp$text)$citation
        if (is.null(citation)) citation <- NA 
        
        authorChin <- jsonlite::parse_json(temp$text)$dataproducer
        if (is.null(authorChin)) authorChin <- NA
        
        raw_data <- jsonlite::parse_json(temp$text)$url
        if (is.null(raw_data)) raw_data <- NA
        
        m[j,] <- c(cohort, title, dataset_ID, samples, version, type_of_data, ID_Mapping, publication, citation, authorChin, raw_data)
      }
      
      m <- as.data.frame(m)
      names(m) <- c("cohort", "title", "dataset ID", "samples", "version", "type of data", "ID/Gene Mapping", "publication", "citation", "authorChin", "raw data")
    }
    output$detail_info <- DT::renderDT(m, options = list(dom = "t", scrollX = TRUE))
  })
  
  
  # Download request data by XenaDownload function
  observeEvent(input$load, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Begin to download files, Please wait...", value = 0)
    
    UCSCXenaTools::XenaDownload(query_url())
    
    progress$set(message = "Over...", value = 1)
  })
  
  # Prepare request data by XenaPrepare function
  request_data <- eventReactive(input$load, {
    xe_download <- UCSCXenaTools::XenaDownload(query_url())
    return(UCSCXenaTools::XenaPrepare(xe_download))
  })
  
  # observeEvent(input$load, {
  #   print(request_data())
  # })
  
  # Download buttom of request data with zip compress
  output$download <- downloadHandler(
    filename = "database.zip",
    contentType = "application/zip",
    content = function(file){
      xe_download <- UCSCXenaTools::XenaDownload(query_url())
      zip::zip(zipfile = paste0(tempdir(), "/target_database.zip"), files = xe_download$destfiles, recurse = F)
      file.copy(paste0(tempdir(), "/target_database.zip"), file)
      file.remove(paste0(tempdir(), "/target_database.zip"))
    }
  )
  
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