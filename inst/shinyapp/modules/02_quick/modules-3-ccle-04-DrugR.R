ui.modules_3_ccle_04 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "ccle", c("mRNA")),
        h4("3. Select primary site(s)"),
        virtualSelectInput(
            ns("SitePrimary"), NULL,
            choices = choices_primary_site,
            multiple = TRUE,
            selected = "prostate"
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
            uiOutput(ns("gene_ccle_drug_response_diff")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Show P value:"),
                switchInput(
                    inputId = ns("pdist_show_p_value"),
                    value = FALSE,
                    onLabel = "Yes",
                    offLabel = "No"
                ),
                h5("(2) Colors:"),
                colourpicker::colourInput(inputId = ns("high_col"), "High group color", "#DF2020"),
                colourpicker::colourInput(inputId = ns("low_col"), "Low group color", "#DDDF21"),
                h5("(3) Point transparent:"),
                sliderTextInput(
                    inputId = ns("alpha"), label = NULL,
                    choices = seq(from = 0, to = 1, by = 0.1),
                    selected = "0.5", grid = TRUE
                ),
            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 8),
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



server.modules_3_ccle_04 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "ccle")

    plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        p <- vis_gene_drug_response_diff(
            Gene = mol_info$molecule(),
            values = c(input$high_col, input$low_col),
            tissue = input$SitePrimary,
            Method = "wilcox.test",
            Show.P.label = input$pdist_show_p_value,
            alpha = input$alpha
        )
        print(class(p))
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("gene_ccle_drug_response_diff"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # # check whether valid out plot
        # chect_plot = inherits(plot_func(), "try-error")
        # if(chect_plot){
        #     sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
        #     req(chect_plot)
        # }
        output$gene_ccle_drug_response_diff <- renderUI({
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
            paste0(mol_info$molecule(), "_ccle_target_response_diff.", input$device)
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
            paste0(mol_info$molecule(), "_ccle_target_response_diff.csv")
        },
        content = function(file) {
            data = data <- analyze_gene_drug_response_asso(mol_info$molecule(), combine = TRUE)
            write.csv(data, file, row.names = FALSE)
        }
    )

} 