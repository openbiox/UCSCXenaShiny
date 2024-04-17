ui.modules_1_tcga_02 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA", "protein", "cnv")),

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
            uiOutput(ns("pancan_anatomy"))
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Gender:"),
                selectInput(inputId = ns("Gender"), label = NULL, 
                            choices = c("Male", "Female"), selected = "Female"),
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
        style = "height:600px",
        box(main_ui,
            width = 5,
            solidHeader = TRUE,
            title = "Quick TCGA Analysis: Compare between tumor and normal", 
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
}


server.modules_1_tcga_02 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "tcga")

    plot_func <- eventReactive(input$search_bttn, {
        out <- vis_pancan_anatomy(
            Gene = mol_info$molecule(),
            data_type = mol_info$profile(),
            Gender = input$Gender
        )
        return(out)
    })

    w <- waiter::Waiter$new(id = ns("pancan_anatomy"), html = waiter::spin_hexdots(), color = "white")

    observeEvent(input$search_bttn,{
        # check whether valid out plot
        shinyjs::disable("search_bttn")
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$pancan_anatomy <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func()$plot)
            fluidRow(
                column(10, offset = 1,
                    plotOutput(ns("plot"), height = "580px"),
                )
            )
        })    
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_anatomy.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_anatomy.csv")
        },
        content = function(file) {
        data = plot_func()$data %>%
            dplyr::rename('Cancer'='tissue', 'Group'='type.x','Organ'='organ','Value'='value') %>%
            dplyr::select(Cancer, Group, Organ, Value)
        write.csv(data, file, row.names = FALSE)
        }
    )
}