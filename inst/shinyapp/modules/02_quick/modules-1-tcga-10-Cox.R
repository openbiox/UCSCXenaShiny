ui.modules_1_tcga_10 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA","protein", "cnv")),
        
        h4("3. Select TCGA endpoint type"),
        selectInput(ns("measure"), NULL,c("OS", "DSS", "DFI", "PFI")),
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
            uiOutput(ns("unicox_gene_tree"))
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Color palette:"),
                colourpicker::colourInput(inputId = ns("first_col"), "First color", "#6A6F68"),
                colourpicker::colourInput(inputId = ns("second_col"), "Second color", "#E31A1C"),
                colourpicker::colourInput(inputId = ns("third_col"), "Third color", "#377DB8"),

            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 8),
                numericInput(inputId = ns("width"), label = "Width", value = 8),
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
            title = "Quick TCGA Analysis:  Univariate Cox regression survival analysis", 
            status = "info",
            background = "gray",
            collapsible = FALSE,
            style = "height:600px",
            footer = "TIPs: Click the bottom button to execute/update the analysis."
        ),
        box(out_ui,
            width = 7,
            solidHeader = TRUE,
            title = "Analytical results:", 
            status = "info",
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

server.modules_1_tcga_10 = function(input, output, session){
    ns = session$ns


    mol_info = callModule(mol_quick_select_Server, "id", "tcga")


    plot_func = eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令

        p <- vis_unicox_tree(
            Gene = mol_info$molecule(),
            measure = input$measure,
            data_type = mol_info$profile(),
            values = c(input$first_col, input$second_col, input$third_col)
        )
        if(is.null(p)){return(NULL)}
        pdata <- p$data %>% 
            as.data.frame() %>%
            dplyr::select(cancer, measure, n_contrast, n_ref, beta, HR_log, lower_95_log, upper_95_log, Type, p.value)
        return(list(plot = p, data = pdata))
    })

    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("unicox_gene_tree"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$unicox_gene_tree <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func()$plot)
            fluidRow(
                column(8, offset = 2,
                    plotOutput(ns("plot"), height = "580px"),
                )
            )
        })   
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_unicox.", input$device)
        },
        content = function(file) {
            p <- plot_func()$plot
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_unicox.csv")
        },
        content = function(file) {
            data = plot_func()$data %>%
                dplyr::rename('Cancer'='cancer', 'Event'='measure','Samples'='n_ref',
                'P.value' = 'p.value') %>%
                dplyr::select(Cancer,Event,Samples,HR_log,lower_95_log,upper_95_log,Type,P.value)
            write.csv(data, file, row.names = FALSE)
        }
    )
}