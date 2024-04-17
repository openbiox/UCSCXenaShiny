ui.modules_1_tcga_08 = function(id){
    ns = NS(id)
    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", c("mRNA","transcript","methylation","miRNA")),

        h4("3. Group by gene mutation"),
        virtualSelectInput(
            inputId = ns("mut_Gene"),
            label = NULL, choices = NULL,
            width = "100%", search = TRUE,
            allowNewOption = TRUE,
            dropboxWidth = "200%"
        ), 
        h4("4. Select analysis mode"),
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

        shinyWidgets::actionBttn(
            inputId = ns("search_bttn"),
            label = "Go!",
            style = "gradient",
            icon = icon("search"),
            color = "primary",
            block = TRUE,
            size = "sm"
        ),
        verbatimTextOutput(ns("mut_tip")),
    )
    out_ui = tagList(
        fluidRow(
            uiOutput(ns("mut_plot")),
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
                # h5("(4) ggplot theme:"),
                # selectInput(inputId = ns("theme"), label = "Select theme for plot", 
                #             choices = names(themes_list), selected = "Cowplot"),
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
            title = "Quick TCGA Analysis: Compare between mutation and wild tumor samples", 
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



server.modules_1_tcga_08 = function(input, output, session){
    ns = session$ns

	observe({
	  updateVirtualSelect(
	    "mut_Gene",
	    choices = tcga_id.list[["Gene"]],
	    selected = "TP53"
	  )
	})

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

	mut_tip = eventReactive(input$search_bttn,{
		mut_dat_raw <- query_pancan_value(input$mut_Gene, data_type = "mutation")
        if(all(is.na(mut_dat_raw))){
            sendSweetAlert(session, title = "Warning", type = "error", text = "No valid mutation data for the gene!")
            return("Warning: No mutation information for the gene.")
        }
		tcga_gtex <- load_data("tcga_gtex")
		mut_dat <- mut_dat_raw %>%
		  as.data.frame() %>%
		  tibble::rownames_to_column("Sample") %>%
		  dplyr::rename("mut" = ".") %>% 
		  dplyr::inner_join(., tcga_gtex, by = c("Sample"="sample")) %>% 
		  dplyr::filter(.data$type2 == "tumor") %>%
		  dplyr::select("Sample", "tissue", "mut")

		exp_dat_raw <- query_pancan_value(mol_info$molecule(), data_type = mol_info$profile())
		exp_dat <- exp_dat_raw$expression %>%
		  as.data.frame() %>%
		  tibble::rownames_to_column("Sample") %>%
		  dplyr::rename("values" = ".") %>%
		  dplyr::inner_join(., tcga_gtex, by = c("Sample"="sample")) %>%
		  dplyr::select("Sample", "values")
		merge_dat <- dplyr::inner_join(mut_dat, exp_dat) %>%
		  dplyr::mutate(
		    tissue = as.character(.data$tissue),
		    mut = factor(.data$mut, levels = c(1, 0), labels  = c("Mutation", "Wild"))
		  )
		if(input$Mode=="Single-cancer"){
			merge_dat_sub = subset(merge_dat, tissue==input$Cancer)
			subtest = subset(merge_dat_sub, mut=="Mutation")
			if(nrow(subtest)<=3){
				mut_tips = paste0("Warning: Less than 3 samples in Mutation group.")
                sendSweetAlert(session, title = "Warning", type = "error", text = "No valid mutation data for the gene!")
			} else {
				mut_tips = paste0("Note: ", nrow(subtest)," samples in Mutation group.")
			}
		} else {
			subtest = subset(merge_dat, mut=="Mutation")
			subtest_stat = names(table(subtest$tissue)[table(subtest$tissue)>3])
            if(length(subtest_stat)==0){
                mut_tips = paste0("Warning: No cancer type with Mutation group above 3 samples.")
                sendSweetAlert(session, title = "Warning", type = "error", text = "No valid mutation data for the gene!")
            } else {
                mut_tips = paste0("Note: ",length(subtest_stat)," cancer types with Mutation group above 3 samples.")
            }
			
		}
		mut_tips
	})

	output$mut_tip = renderPrint({
		cat(mut_tip())
	})

    plot_func <- eventReactive(input$search_bttn, {
        print(input$Mode)
        req(grep("Note", mut_tip()))
        p = switch(input$Mode,
			`Pan-cancer`=vis_toil_Mut(mut_Gene = input$mut_Gene, Gene = mol_info$molecule(),
				data_type=mol_info$profile(), Mode = input$pdist_mode,
				Show.P.value = mark_p$Show.P.value, Show.P.label = mark_p$Show.P.label,
				values = c(input$tumor_col, input$normal_col)
				) + #themes_list[[input$theme]] + 
                    ggplot2::theme(
                        axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
                        text = element_text(size = 20)),
			`Single-cancer`=vis_toil_Mut_cancer(mut_Gene = input$mut_Gene, Gene = mol_info$molecule(),
				data_type=mol_info$profile(), Mode = input$pdist_mode,
				Show.P.value = mark_p$Show.P.value, Show.P.label = mark_p$Show.P.label,
				values = c(input$tumor_col, input$normal_col), Cancer=input$Cancer, opt_pancan = opt_pancan()
				) + #themes_list[[input$theme]] +
                    ggplot2::theme(text = element_text(size = 20),
                                   legend.position = "none")
        )
        return(p)
    })

    w <- waiter::Waiter$new(id = ns("mut_plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        # check whether valid out plot
        chect_plot = is.null(plot_func()) 
        if(chect_plot){
            sendSweetAlert(session, title = "Warning", type = "error", text = "Please select a valid molecule.")
            req(chect_plot)
        }
        output$mut_plot <- renderUI({
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_mutation.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_mutation.csv")
        },
        content = function(file) {
	  	data = plot_func()$data %>%
		  	dplyr::rename('Cancer'='tissue', 'Sample'='sample',
		  		'Group'='mut', 'Expression'='expression') %>%
		  	dplyr::arrange(Sample)
        write.csv(data, file, row.names = FALSE)
        }
    )

} 