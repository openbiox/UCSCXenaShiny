ui.modules_2_pcawg_03 = function(id){
    ns = NS(id)

    main_ui = tagList(
        mol_quick_select_UI(ns("id"), "pcawg", 
                            c("mRNA","miRNA","promoter","fusion","APOBEC")),

        h4("3. Select PCAWG project (Only OS event)"),
        selectInput(ns("dataset"), NULL,sort(pcawg_names)),

        h4("4. Filter by clinical features"),
        virtualSelectInput(ns("sex"), "Gender:",choices = c("Female" = "female", "Male" = "male" ),
                    multiple=TRUE, selected = c("female", "male")),
        sliderInput(ns("age"), "Age", min = 0, max = 100, value = c(0, 100)),

        h4("5. Grouping by"),

        awesomeRadio(
            inputId = ns("groupby"),
            label = NULL, 
            choices = c("Median cutoff", "Optimal cutoff"),
            selected = "Median cutoff",
            inline = TRUE, checkbox = TRUE
        ),
        bsTooltip(ns("groupby"), "Only valid for continuous variable, other than molecules from Gene Fusion", 
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
        wellPanel(style = "height:795px",
        box(main_ui,
            width = 5,
            solidHeader = TRUE,
            title = "Quick PCAWG Analysis: Kaplan-Meier survival analysis(Log-rank)", 
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
    )



}

server.modules_2_pcawg_03 = function(input, output, session){
    ns = session$ns

    mol_info = callModule(mol_quick_select_Server, "id", "pcawg")

    filter_dat <- eventReactive(input$submit_bt, {
        val <- query_pancan_value(mol_info$molecule(), mol_info$profile(), database = "pcawg")
        val <- val$data
        val <- na.omit(val)
        
        if(length(val)<10){
            sendSweetAlert(
                session = session,
                title = "Error...",
                text = "There is too little available data (<10) for this entry.",
                type = "error"
            )
            return(NULL)
        }
        
        val_dat <- data.frame("icgc_specimen_id" = names(val),"val" = as.numeric(val))
        
        dat <- dplyr::inner_join(pcawg_info,val_dat,by="icgc_specimen_id") %>% 
            dplyr::filter(.data$dcc_project_code %in% input$dataset) %>% 
            dplyr::filter(!is.na(.data$OS.time)) %>% 
            dplyr::select( sampleID = icgc_specimen_id,
                            status = OS ,
                            time = OS.time,
                            value = val,
                            gender = donor_sex,
                            age = donor_age_at_diagnosis)

        # filter samples
        dat = dplyr::filter(dat,
                    .data$age > input$age[1],
                    .data$age < input$age[2],
                    .data$gender %in% input$sex
        )
        # group fusion profile
        if(mol_info$profile() == "fusion"){
            dat <- dplyr::mutate(dat,group = case_when(
                .data$value == 1 ~ "fusion (1)",
                .data$value == 0 ~ "non-fusion (0)"
            ))
        }
        dat
    })

    tips = eventReactive(input$submit_bt, {
        if(mol_info$profile() == "fusion"){
            t1 = "fusion status"
            sur_dat2 = filter_dat()
        } else {
            sur_dat2 = filter_dat() %>%
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
            msg = paste0("Note: ", nrow(filter_dat()), " samples are grouped by ", 
                         t1, " of ", mol_info$molecule(), " ", mol_info$profile(), ".")
        }
        msg
    })
    output$msg = renderPrint({cat(tips())})


    plot_func <- eventReactive(input$submit_bt, {
        req(grep("Note", tips()))
        id <- showNotification(h3("The task is running..."), duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id), add = TRUE)  #reactive语句执行完毕时，运行remove命令
        if (!is.null(filter_dat())) {
            if (nrow(filter_dat()) >= 10) {
                if (mol_info$profile() %in% c("mRNA", "miRNA","promoter", "APOBEC")) {
                    cutoff_mode = ifelse(input$groupby=="Optimal cutoff","Auto","Custom")

                    p <- UCSCXenaShiny:::sur_plot(filter_dat(), cutoff_mode, c(50, 50), palette = input$palette)
                } else {
                    p <- UCSCXenaShiny:::p_survplot(filter_dat(), palette = input$palette) #with group column
                }
                return(p)
            } else {
                return(NULL)
            }
        } else {
            return(NULL)
        }
    })

    return_data <- eventReactive(input$submit_bt, {
        if (!is.null(filter_dat()) & nrow(filter_dat()) >= 10) {
            # shinyjs::show(id = "save_csv")
            select_data <- dplyr::select(filter_dat(), sampleID, value, status, time)
            return(select_data)
        } else {
            return(NULL)
        }
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pcawg_surplot.", input$device)
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
            paste0(mol_info$molecule(), "_", mol_info$profile(), "_pcawg_surplot.csv")
        },
        content = function(file) {
            data = return_data() %>%
                dplyr::rename('Sample'='sampleID','Value'='value',
                'Status'='status', 'Time'='time') %>%
                dplyr::mutate(Cancer = input$dataset,Event = "OS") %>%
                dplyr::select(Cancer, Sample, Event, Status, Time, Value)
            write.csv(data, file, row.names = FALSE)
        }
    )
}