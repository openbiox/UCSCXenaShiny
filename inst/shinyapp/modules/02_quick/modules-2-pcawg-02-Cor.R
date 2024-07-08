ui.modules_2_pcawg_02 = function(id){
    ns = NS(id)
    main_ui = tagList(
        fluidRow(
            column(6,
                style = 'border-right: 1px solid; border-color: white',
                h4(strong("X-axis:")),
                mol_quick_select_UI(ns("id_1"), "pcawg", 
                                    c("mRNA","miRNA","promoter","fusion","APOBEC")),
            ),
            column(6,
                h4(strong("Y-axis:")),
                mol_quick_select_UI(ns("id_2"), "pcawg", 
                                    c("mRNA","miRNA","promoter","fusion","APOBEC")),
            )
        ),
        h4("3. Select PCAWG project(s)"),
        virtualSelectInput(
            ns("dcc_project_code_choose"), NULL,
            choices = sort(pcawg_names),
            multiple = TRUE,
            selected = "BLCA-US"
        ),
        h4("4. Select correlation method"),
        awesomeRadio(
            inputId = ns("cor_method"),
            label = NULL, 
            choices = c("spearman", "pearson"),
            selected = "spearman",
            inline = TRUE, checkbox = TRUE
        ),
        h4("5. Adjust tumor purity"),
        switchInput(
            inputId = ns("purity_adj"),
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"
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
            uiOutput(ns("gene_cor")),
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
            title = "Quick PCAWG Analysis: Correlation in tumor samples", 
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



server.modules_2_pcawg_02 = function(input, output, session){
    ns = session$ns

    mol_info_1 = callModule(mol_quick_select_Server, "id_1", "pcawg")
    mol_info_2 = callModule(mol_quick_select_Server, "id_2", "pcawg")

    plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        p <- vis_pcawg_gene_cor(
            Gene1 = mol_info_1$molecule(),
            Gene2 = mol_info_2$molecule(),
            data_type1 = mol_info_1$profile(),
            data_type2 = mol_info_2$profile(),
            purity_adj = input$purity_adj,
            dcc_project_code_choose = input$dcc_project_code_choose,
            cor_method = input$cor_method,
            use_regline = input$use_regline,
            color = input$color,
            alpha = input$alpha
        )
        if(!is.null(p)){
            p <- p + themes_list[[input$theme]] +
                    theme(text = element_text(size = 20),
                        legend.position = "none")
        }
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("gene_cor"), html = waiter::spin_hexdots(), color = "white")
    
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$gene_cor <- renderUI({
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
            paste0(mol_info_1$molecule(), "_", mol_info_1$profile(), "_",  
                   mol_info_2$molecule(), "_", mol_info_2$profile(), "_cor.", input$device)
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
            paste0(mol_info_1$molecule(), "_", mol_info_1$profile(), "_", 
                   mol_info_2$molecule(), "_", mol_info_2$profile(), "_cor.csv")
        },
        content = function(file) {
        data = plot_func()$data %>%
            dplyr::rename('Project'='dcc_project_code','Sample'='icgc_specimen_id', 'Group'='type2',
            'Purity'='purity','Molecule1'='gene1', 'Molecule2'='gene2') %>%
            dplyr::select(Project, Sample, Group, Molecule1, Molecule2, Purity)
        write.csv(data, file, row.names = FALSE)
        }
    )
}