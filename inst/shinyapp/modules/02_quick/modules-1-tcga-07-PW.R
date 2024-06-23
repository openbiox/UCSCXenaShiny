ui.modules_1_tcga_07 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA","protein", "cnv")),

        h4("3. Select one pathway"),
        virtualSelectInput(
            inputId = ns("pw_name"), 
            label = NULL,
            choices = list(
                HALLMARK = paste0("HALLMARK_",tcga_id.list[["HM"]]),
                KEGG = paste0("KEGG_",tcga_id.list[["KEGG"]]),
                IOBR = paste0("IOBR_",tcga_id.list[["IOBR"]])
            ),
            selected = "HALLMARK_ADIPOGENESIS", 
            width = "100%",
            search = TRUE,
            dropboxWidth = "200%"
        ),
        h4("3. Select TCGA cancer(s)"),
        virtualSelectInput(
            ns("Cancer"), NULL,
            choices = sort(tcga_names),
            multiple = TRUE,
            selected = "ACC"
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
            uiOutput(ns("pw_plot")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Regression line:"),
                switchInput(
                    inputId = ns("use_regline"),
                    value = TRUE,
                    onLabel = "Yes",
                    offLabel = "No"
                ),
                h5("(2) Point transparent:"),
                sliderTextInput(
                    inputId = ns("alpha"),
                    label = NULL,
                    choices = seq(from = 0, to = 1, by = 0.1),
                    selected = "0.5", grid = TRUE
                ),
                h5("(3) Point color:"),
                colourpicker::colourInput(
                    inputId = ns("color"), NULL, "#000000"),
                h5("(4) ggplot theme:"),
                selectInput(inputId = ns("theme"), label = NULL, 
                            choices = names(themes_list), selected = "Cowplot"),
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


server.modules_1_tcga_07 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "tcga")


	plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        p <- vis_gene_pw_cor(
                Gene = mol_info$molecule(),
                data_type = mol_info$profile(),
                pw_name = input$pw_name,
                cancer_choose = input$Cancer,
                cor_method = input$cor_method,
                use_regline = input$use_regline,
                color = input$color,
                alpha = input$alpha)
        p <- p + themes_list[[input$theme]] +
                theme(text = element_text(size = 20),
                    legend.position = "none")
	    return(p)
	})

    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("pw_plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$pw_plot <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_pathway.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pancan_pathway.csv")
        },
        content = function(file) {
            data = plot_func()$data %>%
                    dplyr::select(Cancer, Sample, identifier, values, pw_name, pw_score) %>%
                    dplyr::rename("Molecule"="identifier", "Expression"="values",
                                "Pathway"="pw_name", "ssGSEA"="pw_score")
            write.csv(data, file, row.names = FALSE)
        }
    )
}