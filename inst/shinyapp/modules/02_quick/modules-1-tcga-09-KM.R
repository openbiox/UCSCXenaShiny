ui.modules_1_tcga_09 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "tcga", 
            c("mRNA","transcript","methylation","miRNA","protein", "cnv", "mutation")),
        
        h4("3. Select TCGA cancer type and endpoint type"),
        fluidRow(
            column(6,
                selectInput(ns("Cancer"), NULL,sort(tcga_names)),
            ),
            column(6,
                selectInput(ns("endpoint"), NULL,c("OS", "DSS", "DFI", "PFI"))
            )
        ),
        
        h4("4. Filter by clinical features"),
        fluidRow(
            column(6,
                virtualSelectInput(ns("sex"), "Gender:",c("FEMALE", "MALE", "Unknown"),
                            multiple=TRUE, selected = c("FEMALE", "MALE", "Unknown")),
            ),
            column(6,
                virtualSelectInput(ns("stage"), "Tumor stage:",c("I", "II", "III", "IV", "Unknown"),
                            multiple=TRUE, selected = c("I", "II", "III", "IV", "Unknown")),
            ),
        ),
        sliderInput(ns("age"), "Age", min = 0, max = 100, value = c(0, 100)),

        h4("5. Grouping by"),

        awesomeRadio(
            inputId = ns("groupby"),
            label = NULL, 
            choices = c("Median cutoff", "Optimal cutoff"),
            selected = "Median cutoff",
            inline = TRUE, checkbox = TRUE
        ),
        bsTooltip(ns("groupby"), "Only valid for continuous variable, other than molecules from Copy Number Variation/Mutation status", 
                    placement = "right", trigger = "hover", options = list(container = "body")),
        shinyWidgets::actionBttn(
          inputId = ns("submit_bt"), label = "Submit",
          style = "gradient",
          icon = icon("check"),
          color = "primary",
          block = TRUE,
          size = "sm"
        ),
        verbatimTextOutput(ns("msg"))
    )
    out_ui = tagList(
        fluidRow(
            uiOutput(ns("surplot"))
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Color palette:"),
                selectInput(ns("palette"), NULL, c("hue", "grey", "RdBu", "Blues", "npg", "aaas"),
                            selected = "aaas")
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
        style = "height:670px",
        box(main_ui,
            width = 5,
            solidHeader = TRUE,
            title = "Quick TCGA Analysis: Kaplan-Meier survival analysis(Log-rank)", 
            status = "info",
            background = "gray",
            collapsible = FALSE,
            style = "height:670px",
            footer = "TIPs: Click the bottom button to execute/update the analysis."
        ),
        box(out_ui,
            width = 7,
            solidHeader = TRUE,
            title = "Analytical results:", 
            status = "info",
            background = "gray",
            collapsible = FALSE,
            style = "height:670px",
            footer = "TIPs: Pull the sidebar to adjsut plot parameters or download results through the top-right widget.",
            sidebar = boxSidebar(
                        id = ns("sidebar"),
                        width = 50,
                        side_ui
            )
        )
    )
}

server.modules_1_tcga_09 = function(input, output, session){
    ns = session$ns

    ## Data filter
    dat_filter <- function(data, age, gender, stage, endpoint) {
        endpoint.time <- paste0(endpoint, ".time")
        dat <- data %>%
            dplyr::rename(time = !!endpoint.time, status = !!endpoint) %>%
            dplyr::filter(
            age >= !!age[1],
            age <= !!age[2],
            gender %in% !!gender,
            stage %in% !!stage,
            !is.na(time),
            !is.na(status)
            )
        message("cases after filtering: ", nrow(dat))
        return(dat)
    }

    mol_info = callModule(mol_quick_select_Server, "id", "tcga")

    sur_dat = eventReactive(input$submit_bt,{
        TCGA_cli_merged <- dplyr::full_join(
            load_data("tcga_clinical"),
            load_data("tcga_surv"),
            by = "sample"
        )
        dat1 = tcga_surv_get(
            TCGA_cohort = input$Cancer, item = mol_info$molecule(),
            profile = mol_info$profile(), TCGA_cli_data = TCGA_cli_merged
        )
        # filter
        dat2 = dat_filter(data = dat1, age = input$age,
                          gender = input$sex, stage = input$stage, 
                          endpoint = input$endpoint)
        dat2
    })


    tips = eventReactive(input$submit_bt, {
        if(mol_info$profile() == "mutation"){
            sur_dat2 = sur_dat() %>%
                    dplyr::mutate(group = value)
            t1 = "variation status"
        } else if (mol_info$profile() == "cnv") {
            sur_dat2 = sur_dat() %>%
                    dplyr::mutate(group = dplyr::case_when(
                    .data$value > 0 ~ "Duplicated",
                    .data$value < 0 ~ "Deleted",
                    TRUE ~ "Normal"
                ))
            t1 = "variation status"
        } else {
            sur_dat2 = sur_dat() %>%
                dplyr::arrange(.data$value) %>%
                dplyr::mutate(per_rank = 100 / nrow(.) * (1:nrow(.))) %>%
                dplyr::mutate(group = dplyr::case_when(
                    .data$per_rank > !!50 ~ "High",
                    .data$per_rank <= !!50 ~ "Low",
                    TRUE ~ NA_character_
                ))
            t1 = ifelse(input$groupby=="Optimal cutoff","optimal cutoff","median value")   
        }
        chech_dat = sur_dat2  %>% 
                dplyr::count(group) %>% 
                dplyr::filter(n>3) # the number of groups > 3 samples
        if(nrow(chech_dat)<=1){
            msg = "Warning: Please adjust above input for valid sample grouping."
            sendSweetAlert(session, title = "Warning", text = "No more than two groups (> 3 samples) are available!")
        } else {
            msg = paste0("Note: ", nrow(sur_dat()), " samples are grouped by ", 
                         t1, " of ", mol_info$molecule(), " ", mol_info$profile(), ".")
        }
        msg
    })

    output$msg = renderPrint({cat(tips())})




    plot_func = eventReactive(input$submit_bt, {
        req(grep("Note", tips()))
        cutoff_mode = ifelse(input$groupby=="Optimal cutoff","Auto","Custom")
        p <- tcga_surv_plot(sur_dat(),
                            cutoff_mode = cutoff_mode, #"Custom",
                            cutpoint = c(50, 50),
                            profile = mol_info$profile(),
                            palette = input$palette #"aaas"
        )
        p
    })

    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("surplot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$submit_bt,{
        shinyjs::disable("submit_bt")
        output$surplot <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
            fluidRow(
                column(10, offset = 1,
                    plotOutput(ns("plot"), height = "650px"),
                )
            )
        })   
        shinyjs::enable("submit_bt")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_tcga_surplot.", input$device)
        },
        content = function(file) {
            p <- plot_func()
            if (input$device == "pdf") {
                pdf(file, width = input$width, height = input$height, onefile = FALSE)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_tcga_surplot.csv")
        },
        content = function(file) {
            data = sur_dat() %>%
                dplyr::rename('Sample'='sampleID','Value'='value',
                'Status'='status', 'Time'='time') %>%
                dplyr::mutate(Cancer = input$Cancer,Event = input$endpoint) %>%
                dplyr::select(Cancer, Sample, Event, Status, Time, Value)
            write.csv(data, file, row.names = FALSE)
        }
    )
}