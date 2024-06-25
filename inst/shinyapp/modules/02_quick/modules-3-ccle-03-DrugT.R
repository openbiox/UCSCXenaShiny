ui.modules_3_ccle_03 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "ccle", c("mRNA")),
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
        ),
        p("NOTE: ")
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
            title = "Quick Analysis: Compare between tumor and normal", 
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
            footer = "TIPs: Pull the sidebar to adjsut plot parameters or download results through the top-right widget.",
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

    mol_info = callModule(mol_quick_select_Server, "id", "ccle")

    plot_func <- eventReactive(input$search_bttn, {
        # # check whether valid out plot
        # chect_plot = inherits(plot_func(), "try-error")
        # if(chect_plot){
        #     sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
        #     req(chect_plot)
        # }
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        p <- vis_gene_drug_response_asso(
            Gene = mol_info$molecule(),
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
            paste0(mol_info$molecule(), "_ccle_drug_target.", input$device)
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
            paste0(mol_info$molecule(), "_ccle_drug_target.csv")
        },
        content = function(file) {
            data = data <- analyze_gene_drug_response_asso(mol_info$molecule(), combine = TRUE)
            write.csv(data, file, row.names = FALSE)
        }
    )

} 