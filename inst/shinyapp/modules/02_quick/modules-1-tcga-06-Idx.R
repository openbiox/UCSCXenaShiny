ui.modules_1_tcga_06 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA","protein", "cnv")),

        h4("3. Select fearture type"),
        selectInput(
            ns("Type"), NULL,
            choices = "Stemness",
            selected = c("Stemness", "TMB", "MSI")
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
            uiOutput(ns("gene_pancan_radar")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Line color:"),
                colourpicker::colourInput(
                    inputId = ns("color_line"), NULL, "#00AFBB")
            ),
            column(6,
                h4("2. Download options"),
                h5("(1) Figure:"),
                numericInput(inputId = ns("height"), label = "Height", value = 6),
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
        style = "height:600px",
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



}


server.modules_1_tcga_06 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "tcga")

    plot_func <- eventReactive(input$search_bttn, {
        vis_fun <- switch(input$Type,
            Stemness = vis_gene_stemness_cor,
            TMB = vis_gene_tmb_cor,
            MSI = vis_gene_msi_cor
        )
        p <- vis_fun(Gene = mol_info$molecule(), 
                    data_type = mol_info$profile(),
                    cor_method = input$cor_method)
        
        if (is.null(p)) {
            sendSweetAlert(session, title = "Warning", text = "No data could be queried!")
            return(NULL)
        }

        pdata <- p$data %>%
            dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))
        df <- pdata %>%
            select(cor, cancer) %>%
            pivot_wider(names_from = cancer, values_from = cor) %>%
            tibble::rownames_to_column()
        
        plot <- ggradar::ggradar(
            df[1, ],
            font.radar = "sans",
            values.radar = c("-1", "0", "1"),
            grid.min = -1, grid.mid = 0, grid.max = 1,
            # Background and grid lines
            background.circle.colour = "white",
            gridline.mid.colour = "grey",
            # Polygons
            group.line.width = 1,
            group.point.size = 3,
            group.colours = input$color_line,
            plot.title = paste0(mol_info$molecule(), " ", mol_info$profile(), "——", input$Type)
        ) + theme(plot.title = element_text(hjust = .5))
        return(list(plot = plot, data = pdata))
    })



    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("gene_pancan_radar"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$gene_pancan_radar <- renderUI({
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_index.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_index.csv")
        },
        content = function(file) {
            data = plot_func()$data %>%
                dplyr::rename('Cancer'='cancer',
                'Cor'='cor', 'P.value'='p.value') %>%
                dplyr::select(Cancer, Cor, P.value) %>%
                tibble::remove_rownames()
            write.csv(data, file, row.names = FALSE)
        }
    )
}