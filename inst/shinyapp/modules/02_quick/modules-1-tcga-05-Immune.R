ui.modules_1_tcga_05 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA","protein", "cnv")),

        h4("3. Select TIL cell types"),
        selectInput(
            ns("immune_sig"), NULL,
            choices = "Cibersort",
            selected = c("Yasin", "Wolf", "Attractors", "ICR", 
                         "c7atoms", "Bindea", "Cibersort")
        ),
        h4("4. Select correlation method"),
        awesomeRadio(
            inputId = ns("cor_method"),
            label = NULL, 
            choices = c("spearman", "pearson"),
            selected = "spearman",
            inline = TRUE, checkbox = TRUE
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
            uiOutput(ns("hm_gene_immune_cor")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Colors:"),
                colourpicker::colourInput(
                    inputId = ns("color_low"), "Low", "#377DB8"),
                colourpicker::colourInput(
                    inputId = ns("color_mid"), "Middle", "white"),
                colourpicker::colourInput(
                    inputId = ns("color_high"), "High", "#E31A1C"),
            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 6),
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
            title = "Quick TCGA Analysis: Correlation in tumor samples", 
            status = "success",
            background = "gray",
            collapsible = FALSE,
            style = "height:600px",
            footer = "TIPs: Click the bottom button to execute/update the analysis."
        ),
        box(out_ui,
            width = 7,
            solidHeader = TRUE,
            title = "Analytical results:", 
            status = "success",
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


server.modules_1_tcga_05 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "tcga")

    plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        p <- vis_gene_immune_cor(
            Gene = mol_info$molecule(),
            data_type = mol_info$profile(),
            Immune_sig_type = input$immune_sig,
            cor_method = input$cor_method 
        )
        p = p + 
            scale_fill_gradient2(low = input$color_low, mid = input$color_mid, high = input$color_high)
        return(p)
    })

    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("hm_gene_immune_cor"), html = waiter::spin_hexdots(), color = "white")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$hm_gene_immune_cor <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
            fluidRow(
                column(12,
                    plotOutput(ns("plot"), height = "580px"),
                )
            )
        }) 
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_immune.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_immune.csv")
        },
        content = function(file) {
            data = plot_func()$data %>%
                dplyr::rename('Cancer'='cancer','Cell type'='immune_cells',
                'Cor'='cor', 'P.value'='p.value') %>%
                dplyr::select(Cancer, `Cell type`, Cor, P.value) %>%
                tibble::remove_rownames()
            write.csv(data, file, row.names = FALSE)
        }
    )
}