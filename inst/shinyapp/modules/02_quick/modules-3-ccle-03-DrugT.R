ui.modules_3_ccle_03 = function(id){
    ns = NS(id)
    main_ui = tagList(
        h4("1. Select omics type"),
        selectInput(
            inputId = ns("profile"), label = NULL,
            choices  = c("mRNA Expression"="mRNA"),
            selected = "mRNA"
        ),
        h4(id = ns("id_tip"), "2. Select one gene list"),
        bsTooltip(ns("id_tip"), "When two or more genes are selected, their geometric mean is calculated as the signature value.", 
                    placement = "right", trigger = "hover", options = list(container = "body")),
        virtualSelectInput(
            inputId = ns("Pancan_search_1"),
            label = NULL, choices = NULL, multiple = TRUE,
            width = "100%", search = TRUE,
            allowNewOption = FALSE, dropboxWidth = "100%"
        ),
        h4("3. Select X-axis type"),
        selectInput(
            inputId = ns("x_axis_type"),
            label = NULL,
            choices = c("mean.diff", "median.diff"),
            selected = "mean.diff"
        ),
        shinyWidgets::actionBttn(
            inputId = ns("search_bttn"),
            label = "Go!",
            style = "gradient",
            icon = icon("search"),
            color = "primary",
            block = TRUE,
            size = "sm"
        )
    )
    out_ui = tagList(
        fluidRow(
            uiOutput(ns("gene_ccle_drug_target")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                selectInput(
                    inputId = ns("output_form"),
                    label = "(1) Plot output form:",
                    choices = c("plotly", "ggplot2"),
                    selected = "plotly"
                ),
            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 6),
                numericInput(inputId = ns("width"), label = "Width", value = 6),
                awesomeRadio(ns("device"), label = "Format", 
                    choices = c("pdf", "png"), selected = "pdf", inline = TRUE),
                downloadBttn(
                  outputId = ns("download_1"),
                  style = "gradient",
                  color = "primary",
                  block = TRUE,
                  size = "sm"
                ),
                h5("(2) Data table:"),
                downloadBttn(
                  outputId = ns("download_2"),
                  style = "gradient",
                  color = "primary",
                  block = TRUE,
                  size = "sm"
                )
            )
        )
    )
    fluidPage(
        wellPanel(style = "height:725px",
        box(main_ui,
            width = 5,
            solidHeader = TRUE,
            title = "Quick Analysis: Analyze Association between Gene (Signature) and Drug Response", 
            status = "primary",
            background = "gray",
            collapsible = FALSE,
            style = "height:600px",
            footer = "TIPs: Click the bottom button to execute/update the analysis."
        ),
        box(out_ui,
            width = 7,
            solidHeader = TRUE,
            title = "Analytical results:", 
            status = "primary",
            background = "gray",
            collapsible = FALSE,
            style = "height:600px",
            footer = "TIPs: Pull the sidebar to adjust plot parameters or download results through the top-right widget.",
            sidebar = boxSidebar(
                        id = ns("sidebar"),
                        width = 50,
                        side_ui
            )
        )
        )
    )
}



server.modules_3_ccle_03 = function(input, output, session){
    ns = session$ns

    gene_ref = sort(rownames(load_data("ccle_expr_and_drug_response")$expr))

    profile_choices <- reactive({
        list(all = ccle_id.list[["Gene"]], default = "TP53")
    })

    observe({
        updateVirtualSelect(
        "Pancan_search_1",
        # choices = profile_choices()$all,
        choices = gene_ref,
        selected = profile_choices()$default
        )
    })

    plot_func <- eventReactive(input$search_bttn, {
        shiny::validate(
            need(try(length(input$Pancan_search_1)<10), 
                "Error: Less than 10-gene signature is supported.")
        )

        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")

        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令

        p <- vis_gene_drug_response_asso(
            Gene = input$Pancan_search_1,
            output_form = input$output_form,
            x_axis_type = input$x_axis_type
        )
        print(class(p))
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("gene_ccle_drug_target"), html = waiter::spin_hexdots(), color = "black")
 
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        if (input$output_form == "ggplot2") {
            output$gene_ccle_drug_target <- renderUI({
                w$show()
                output$plot = renderPlot(plot_func())
                fluidRow(
                    column(10, offset = 1,
                        plotOutput(ns("plot"), height = "580px"),
                    )
                )
            }) 
        } else if (input$output_form == "plotly"){
            output$gene_ccle_drug_target <- renderUI({
                w$show()
                output$plot = plotly::renderPlotly(plot_func())
                fluidRow(
                    column(10, offset = 1,
                        plotly::plotlyOutput(ns("plot"), height = "580px"),
                    )
                )
            }) 
        }
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0("ccle_drug_target.", input$device)
        },
        content = function(file) {
            p <- plot_func()
            if (input$device == "pdf") {
                pdf(file, width = input$width, height = input$height)
                print(p)
                dev.off()
            } else {
                png(file, width = input$width, height = input$height, res = 600, units = "in")
                print(p)
                dev.off()
            }
        }
    )

    output$download_2 <- downloadHandler(
        filename = function() {
            "ccle_drug_target.csv"
        },
        content = function(file) {
            data = data <- analyze_gene_drug_response_asso(input$Pancan_search_1, combine = TRUE)
            write.csv(data, file, row.names = FALSE)
        }
    )
} 