ui.modules_3_ccle_01 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "ccle", c("mRNA","protein","cnv")),

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
            uiOutput(ns("gene_ccle_dist")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 5),
                numericInput(inputId = ns("width"), label = "Width", value = 12),
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
            title = "Quick CCLE Analysis: Compare across primary sites", 
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



server.modules_3_ccle_01 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "ccle")

    plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        print(input$Mode)
        p <- vis_ccle_tpm(
            Gene = mol_info$molecule(),
            data_type = mol_info$profile()
        )
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::enable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$gene_ccle_dist <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
            fluidRow(
                column(12, offset = 0,
                    plotOutput(ns("plot"), height = "580px"),
                )
            )
        })   
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_gene_ccle_dist.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_gene_ccle_dist.csv")
        },
        content = function(file) {
        data = plot_func()$data %>%
            dplyr::rename('Cell_line'='cell','Value'='tpm') %>%
            dplyr::select(Cell_line, Cell_line_aliases, Site_Primary, Value)
        write.csv(data, file, row.names = FALSE)
        }
    )

} 