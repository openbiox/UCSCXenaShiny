ui.modules_2_pcawg_01 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "pcawg", c("mRNA","miRNA","promoter","fusion","APOBEC")),

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
            uiOutput(ns("gene_pancan_dist")),
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Geometry type:"),
                awesomeRadio(ns("pdist_mode"), label = NULL, 
                    choices = c("Boxplot", "Violinplot"), selected = "Boxplot", inline = TRUE),
                h5("(2) Label type:"),
                awesomeRadio(ns("pdist_show_p"), label = NULL, 
                    choices = c("None", "P value", "P label"), selected = "None", inline = TRUE),
                h5("(3) Colors:"),
                colourpicker::colourInput(inputId = ns("tumor_col"), "Tumor sample color", "#DF2020"),
                colourpicker::colourInput(inputId = ns("normal_col"), "Normal sample color", "#DDDF21"),
                h5("(4) ggplot theme:"),
                selectInput(inputId = ns("theme"), label = "Select theme for plot", 
                            choices = names(themes_list), selected = "Cowplot"),
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
            title = "Quick PCAWG Analysis: Compare between tumor and normal", 
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



server.modules_2_pcawg_01 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "pcawg")


    mark_p = reactiveValues(Show.P.value=FALSE, Show.P.label=FALSE)
    observe({
        if(input$pdist_show_p=="P value"){
            mark_p$Show.P.value = TRUE
        } else if (input$pdist_show_p=="P label") {
           mark_p$Show.P.label = TRUE
        }
    })
    observeEvent(input$Mode, {
        if(input$Mode=="Pan-cancer"){
            updateAwesomeRadio(session, "pdist_mode", label = NULL, 
                    choices = c("Boxplot", "Violinplot"), selected = "Boxplot", inline = TRUE)
        } else {
            updateAwesomeRadio(session, "pdist_mode", label = NULL, 
                    choices = c("Dotplot", "Violinplot"), selected = "Violinplot", inline = TRUE)
        }
    })


    plot_func <- eventReactive(input$search_bttn, {
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        print(input$Mode)
        p = vis_pcawg_dist(
                Gene = mol_info$molecule(),
                data_type = mol_info$profile(),
                Mode = input$pdist_mode,
                Show.P.value = mark_p$Show.P.value,
                Show.P.label = mark_p$Show.P.label,
                values = c(input$tumor_col, input$normal_col)
            ) + themes_list[[input$theme]] + 
                ggplot2::theme(
                    axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
                    text = element_text(size = 20))
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$gene_pancan_dist <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pcawg_dist.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pcawg_dist.csv")
        },
        content = function(file) {
        data = plot_func()$data %>%
            dplyr::rename('Project'='dcc_project_code','Sample'='icgc_specimen_id',
            'Group'='type2', 'Value'='tpm') %>%
            dplyr::select(Project, Sample, Group,Value)
        write.csv(data, file, row.names = FALSE)
        }
    )

} 