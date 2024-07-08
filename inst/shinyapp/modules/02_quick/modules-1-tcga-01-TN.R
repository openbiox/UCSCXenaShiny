ui.modules_1_tcga_01 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", c("mRNA","transcript","methylation","miRNA")),

        h4("3. Select analysis mode"),
        shinyWidgets::prettyRadioButtons(
            inputId = ns("Mode"), label = NULL,
            choiceValues = c("Pan-cancer", "Single-cancer"),
            choiceNames = c("Pan-cancer", "Single-cancer"),
            animation = "jelly"
        ),
        tabsetPanel(
            id = ns("Mode_params"),
            type = "hidden",
            tabPanel("Single-cancer",
            selectInput(ns("Cancer"), "Select one cancer",sort(tcga_names))
            ),
            tabPanel("Pan-cancer")
        ),
        h4("4. Include GTEx normal samples"),
        switchInput(
            inputId = ns("pdist_dataset"),
            value = TRUE,
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



server.modules_1_tcga_01 = function(input, output, session){
    ns = session$ns

    observeEvent(input$Mode, {
        updateTabsetPanel(inputId = "Mode_params", selected = input$Mode)
    })

	
    mol_info = callModule(mol_quick_select_Server, "id", "tcga")


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
        p = switch(input$Mode,
        `Pan-cancer` =  vis_toil_TvsN(
                            Gene = mol_info$molecule(),
                            data_type = mol_info$profile(),
                            Mode = input$pdist_mode,
                            Show.P.value = mark_p$Show.P.value,
                            Show.P.label = mark_p$Show.P.label,
                            TCGA.only = !input$pdist_dataset,
                            values = c(input$tumor_col, input$normal_col)
                        ) + themes_list[[input$theme]] + 
                            ggplot2::theme(
                                axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
                                text = element_text(size = 20)),                        
        `Single-cancer` = vis_toil_TvsN_cancer(
                            Gene = mol_info$molecule(),
                            data_type = mol_info$profile(),
                            Cancer = input$Cancer,
                            Mode = input$pdist_mode,
                            Show.P.value = mark_p$Show.P.value,
                            Show.P.label = mark_p$Show.P.label,
                            TCGA.only = !input$pdist_dataset,
                            values = c(input$tumor_col, input$normal_col)
                        ) + themes_list[[input$theme]] +
                            ggplot2::theme(text = element_text(size = 20),
                                           legend.position = "none")
        )
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        # check whether valid out plot
        shinyjs::disable("search_bttn")
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$gene_pancan_dist <- renderUI({
            w$show()
            if(isolate(input$Mode)=="Pan-cancer"){
                output$plot = renderPlot(plot_func())
                fluidRow(
                    column(10, offset = 1,
                        plotOutput(ns("plot"), height = "580px"),
                    )
                )
            } else {
                output$plot = renderPlot(plot_func())
                fluidRow(
                    column(6, offset = 3,
                        plotOutput(ns("plot"), height = "580px"),
                    )
                )
            }
        })    
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_tumorVSnormal.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_tumorVSnormal.csv")
        },
        content = function(file) {
        data = plot_func()$data %>%
            dplyr::rename('Cancer'='tissue', 'Dataset'='dataset', 'Sample'='sample',
            'Group'='type2', 'Value'="value") %>%
            dplyr::select(Cancer,Dataset,Sample,Group,Value) %>%
            dplyr::arrange(Cancer, desc(Dataset), Sample, desc(Group))
        write.csv(data, file, row.names = FALSE)
        }
    )

} 