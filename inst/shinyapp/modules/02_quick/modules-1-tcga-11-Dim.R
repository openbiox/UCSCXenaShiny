ui.modules_1_tcga_11 = function(id){
    ns = NS(id)

    main_ui = tagList(
        h4("1. Select omics type"),
        selectInput(
            ns("profile"), NULL,
            choices = c("mRNA Expression", "Transcript Expression", "DNA Methylation", 
                        "Protein Expression", "miRNA Expression", "Copy Number Variation"),
            selected = "mRNA Expression"),

        h4("2. Input the molecule ids (>=3) by ?"),
        shinyWidgets::prettyRadioButtons(
            inputId = ns("input_ways"), label = NULL,
            choiceValues = c("Selection", "Pathway", "File"),
            choiceNames = c("Selection", "Pathway", "File"),
            animation = "jelly",
            inline = TRUE
        ),
        tabsetPanel(
            id = ns("input_params"),
            type = "hidden",
            tabPanel("Selection",
                virtualSelectInput(
                inputId = ns("ids_ways_1"),
                label = NULL,
                choice = NULL,
                width = "100%",
                multiple = TRUE, 
                search = TRUE,
                allowNewOption = TRUE,
                dropboxWidth = "200%")
            ),
            tabPanel("Pathway",
                fluidRow(
                    column(4,
                        virtualSelectInput(
                            inputId = ns("msigdbr_cat"),
                            label = NULL,
                            choices = msigdbr_types$gs_subcat_label, 
                            selected =  msigdbr_types$gs_subcat_label[1],
                            dropboxWidth = "200%")
                    ),
                    column(8,
                        virtualSelectInput(
                            inputId = ns("msigdbr_pw"),
                            label = NULL,
                            choices = NULL, 
                            selected =  NULL,
                            search = TRUE,
                            dropboxWidth = "200%")
                    )
                ),
                uiOutput(ns("msigdb_note.ui"))
            ),
            tabPanel("File",
                fluidRow(
                    column(8, fileInput(ns("ids_ways_3"),NULL, accept = ".txt")),
                    column(4, downloadButton(ns("dw_L3_x"), "e.g."))
                ),
                # p("Keys: (1) TXT format; (2) One column without colname")
            )
        ),  
        shinyWidgets::actionBttn(
            inputId = ns("query_data"),
            label = "Cache data",
            style = "gradient",
            color = "primary",
            block = TRUE,
            size = "sm"
        ),
		verbatimTextOutput(ns("tip_s1")),
        h4("3. Select TCGA sample range and grouping method"),
        shinyWidgets::prettyRadioButtons(
            inputId = ns("group_ways"), label = NULL,
            choiceValues = c("Preset Group", "Custom Group"),
            choiceNames = c("Preset Group", "Custom Group"),
            animation = "jelly",
            inline = TRUE
        ),
        tabsetPanel(
            id = ns("group_params"),
            type = "hidden",
            tabPanel(
                "Preset Group",
                fluidRow(
                    column(6,
                        virtualSelectInput(
                            ns("choose_cancer"), "Choose cancer(s)",
                            choices = sort(tcga_names),
                            multiple = TRUE,
                            selected = "BRCA"
                        )
                    ),
                    column(6,
                        virtualSelectInput(
                            ns("choose_group"), "Choose grouping variable",
                            choices = colnames(tcga_clinical_fine)[-1],
                            selected = "Code",
                            optionsCount = 4
                        )
                    )
                )
            ),
            tabPanel(
                "Custom Group",
                # br(),
                fluidRow(
                    column(
                        8,
                        fileInput(ns("upload_file"), NULL, accept = ".csv"),
                    ),
                    column(
                        3,
                        downloadButton(ns("eg_file"), "e.g.")
                    ),
                ),
            )
        ),
        h4("4. Select Dimension-Reduction method"),
        awesomeRadio(
            inputId = ns("method"),
            label = NULL, 
            choices = c("PCA", "UMAP", "tSNE"),
            selected = "PCA",
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
        ),
		verbatimTextOutput(ns("tip_s2"))
    )
    out_ui = tagList(
        fluidRow(
            uiOutput(ns("dim_plot"))
        )
    )
    side_ui = tagList(
        fluidRow(
            column(6,
                h4("1. Visualization parameters"),
                h5("(1) Marginal plot:"),
                selectInput(ns("margin"), "Marginal plot",
                    c('NONE', "density" ,"boxplot")),
                h5("(2) Color palette:"),
                selectInput(ns("palette"), "Color setting",
                    c("Set1", "Set2", "Set3", "Paired", "Pastel1", "Pastel2", "Accent", "Dark2"))
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
        style = "height:600px",
        box(main_ui,
            width = 5,
            solidHeader = TRUE,
            title = "Quick TCGA Analysis: Dimension reduction analysis", 
            status = "warning",
            background = "gray",
            collapsible = FALSE,
            style = "height:680px",
            footer = "TIPs: Click the bottom button to execute/update the analysis."
        ),
        box(out_ui,
            width = 7,
            solidHeader = TRUE,
            title = "Analytical results:", 
            status = "warning",
            background = "gray",
            collapsible = FALSE,
            style = "height:680px",
            footer = "TIPs: Pull the sidebar to adjsut plot parameters or download results through the top-right widget.",
            sidebar = boxSidebar(
                        id = ns("sidebar"),
                        width = 50,
                        side_ui
            )
        )
    )
}

server.modules_1_tcga_11 = function(input, output, session){
    ns = session$ns

	profile = reactive({
	  switch(input$profile,
	    `mRNA Expression`="mRNA",
	    `DNA Methylation` = "methylation",
	    `Protein Expression` = "protein",
	    `Transcript Expression` = "transcript",
	    `miRNA Expression` = "miRNA",
	    `Copy Number Variation` = "cnv",
	    list(all = "NONE", default = "NONE")
	  )
	})

	profile_choices <- reactive({
	  switch(profile(),
	    mRNA = list(all = tcga_id.list[["Gene"]], default = c("TP53", "KRAS", "PTEN")),
	    methylation = list(all = tcga_id.list[["Gene"]], default = c("TP53", "KRAS", "PTEN")),
	    protein = list(all = tcga_id.list[["Protein"]], default = c("P53", "GATA3", "PTEN")),
	    transcript = list(all = tcga_id.list[["Transcript"]], default = c("ENST00000269305","ENST00000311936","ENST00000371953")),
	    miRNA = list(all = tcga_id.list[["miRNA"]], default = c("hsa-miR-522-3p","hsa-miR-1271-5p","hsa-miR-518e-3p")),
	    cnv = list(all = tcga_id.list[["Gene"]],default = c("TP53", "KRAS", "PTEN")),
	    list(all = "NONE", default = "NONE")
	  )
	})

	observe({
	  updateVirtualSelect(
	    "ids_ways_1",
	    choices = profile_choices()$all,
	    selected = profile_choices()$default
	  )
	})

	observeEvent(input$input_ways, {
	  updateTabsetPanel(inputId = "input_params", selected = input$input_ways)
	}) 
	observeEvent(input$group_ways, {
	  updateTabsetPanel(inputId = "group_params", selected = input$group_ways)
	}) 


    ## MsigDB通路数据库
	msigdbr_query = reactive({
		category = msigdbr_types$gs_cat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		subcategory = msigdbr_types$gs_subcat[msigdbr_types$gs_subcat_label==input$msigdbr_cat]
		if(length(category)!=0){
			msigdbr_query = msigdbr(species = "Homo sapiens", 
			                      category = category, 
			                      subcategory = subcategory)
			msigdbr_query
		}
	})
	observe({
		if(!is.null(msigdbr_query())){
			msigdbr_term_stat = as.data.frame(table(msigdbr_query()$gs_name)) %>% 
			  dplyr::rename(term=Var1, size=Freq) %>% 
			  dplyr::arrange(term) %>% 
			  dplyr::mutate(term_size = paste0(term," (", size,")"))
			updateVirtualSelect(
			  "msigdbr_pw",
			  choices = msigdbr_term_stat$term_size,
			  selected = msigdbr_term_stat$term_size[1]
			)
		}
	})
	output$msigdb_note.ui = renderUI({
		# pw_sle = ifelse(is.null(input$msigdbr_pw),"HALLMARK_ADIPOGENESIS",input$msigdbr_pw)
		pw_sle = str_split(input$msigdbr_pw," ")[[1]][1]
		term_link = sprintf("https://www.gsea-msigdb.org/gsea/msigdb/human/%s.html",
                    		ifelse(is.null(pw_sle),"HALLMARK_ADIPOGENESIS",pw_sle))
		msigdb_link = "https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp"

		p("Link to ",a("the pathway", href = term_link)," from ", a("MSigDB database", href = msigdb_link), ":")

	})

    # 示例id数据下载
	output$dw_L3_x = downloadHandler(
		filename = function(){
			"sample_multiple_ids.txt"
		},
		content = function(file){
			set.seed(42)
			all_ids = tcga_id_option[["Molecular profile"]][[input$profile]]$all
			sample_ids = sample(all_ids,ifelse(length(all_ids)>10,10,length(all_ids)))
			write.table(sample_ids, file,
				quote = F, row.names = F, col.names = F)
		}
	)

    # 多个id的选择
    ids = eventReactive(input$query_data, {
    	if(input$input_ways=="Selection"){
    		ids = input$ids_ways_1
    	} else if (input$input_ways=="File") {
            req(input$ids_ways_3)
			file = input$ids_ways_3
			if(is.null(file$datapath)){  # 如果空文件
				ids = NULL
			} else {
				ids = read.table(file$datapath)[,1]
				ids = ids[ids %in% tcga_id_option[["Molecular profile"]][[input$profile]]$all]
				if(length(ids)>1000){
					L3s_x = L3s_x[1:1000]
				} 
			}
    	} else if (input$input_ways=="Pathway") {
    		ids = tcga_id_option[["Molecular profile"]][[input$profile]]$all
			pw_genes = msigdbr_query() %>% 
			  dplyr::filter(gs_name %in% str_split(input$msigdbr_pw," ")[[1]][1]) %>% 
			  dplyr::pull(gene_symbol)
			if(input$profile %in% 
				c("mRNA Expression","DNA Methylation","Mutation status","Copy Number Variation")){
				ids = ids[ids %in% pw_genes]
			} else if(L2_x() %in% c("Transcript Expression")){
				if(!exists("tcga_id_referrence")){
					message("Loading \"pancan_identifier_help\"")
					tcga_id_referrence = load_data("pancan_identifier_help")
				}
				ids = ids[ids %in% tcga_id_referrence[[1]][[5]]$Level3[tcga_id_referrence[[1]][[5]]$Symbol %in% pw_genes]]
			}
    	}
	    unique(ids)
	})

    # 缓存id数据
	cache_dat = eventReactive(input$query_data, {
        if(length(ids()))
		withProgress(message=paste0("Query ", length(ids()), " molecules one by one"),{
		  data = purrr::map(ids(), function(x) {
		    # x = ids[1]
		    data <- query_pancan_value(x, data_type=profile())
		    data = data[[1]]
		    data <- dplyr::tibble(sample = names(data), y = as.numeric(data))
		    colnames(data)[2] <- x
		    incProgress(1 / length(ids()))
		    data
		  }) %>% purrr::reduce(dplyr::full_join, by = "sample")
	    })
	    data = data[, apply(data, 2, function(m) {!all(is.na(m))})]
	    return(data)
	})

	output$tip_s1 = renderPrint({
		cat(paste0("Tips: ", ncol(cache_dat())-1, " valid identifiers are cached."))
	})



	## Group分组信息
	output$eg_file = downloadHandler(
		filename = function(){
			"example_group.csv"
		},
		content = function(file){
			group_info = tcga_clinical_fine %>% 
			  dplyr::filter(Cancer == "BRCA") %>% 
			  dplyr::filter(Code %in% c("TP","NT")) %>% 
			  dplyr::select(Sample, Code)
			colnames(group_info)[2] = "Group"
			write.csv(group_info, file, quote = F, row.names = F)
		}
	)

	group_info = eventReactive(input$search_bttn,{
		if(input$group_params=="Preset Group"){
			group_info = tcga_clinical_fine %>% 
			  dplyr::filter(Code %in% c("NT", "TP")) %>%
			  dplyr::mutate(Code = ifelse(Code=="TP","tumor","normal")) %>%
			  dplyr::mutate(Age = ifelse(Age>60, "Old(>60)", "Young(<60)")) %>%
			  dplyr::filter(Cancer == input$choose_cancer)
			# only tumor samples when non-code group
			if(input$choose_group!="Code"){
				group_info = group_info %>%
					dplyr::filter(Code=="tumor")
			}
			group_info = group_info[,c("Sample", input$choose_group)] %>% na.omit()
			colnames(group_info)[2] = "Group"


		} else if (inparams=="Custom Group"){
			if(!is.null(input$upload_file$datapath)){
				group_info = read.csv(input$upload_file$datapath) %>% na.omit()
			} else {
				group_info = data.frame("Sample"=NULL, "Group"=NULL)
			}
		}
		shiny::validate(
			need(try(nrow(group_info)>0), 
				"Error: No group infomation is detected.")
		)
		groups = length(unique(group_info$Group))
		shiny::validate(
			need(try(groups>=2 & groups<=10), 
				"Error: Too few (<2) or much (>10) groups are detected.")
		)
		group_info
	})

	output$tip_s2 = renderPrint({
		cat(paste0("Tips: ", nrow(group_info())," samples with ", 
			length(unique(group_info()$Group))," groups are detected."))
	})

	plot_func <- eventReactive(input$search_bttn, {
      print(head(group_info()))
	  p = vis_dim_dist(
		ids=ids(),
		data_type=profile(), 
		group_info = group_info(),
	  	DR_method = input$method,
	  	palette=input$palette,
	  	add_margin=if (input$margin == "NONE") NULL else input$margin
	  )
	  return(p)
	})

    # Show waiter for plot
    w <- waiter::Waiter$new(id = ns("dim_plot"), html = waiter::spin_hexdots(), color = "black")
    observeEvent(input$search_bttn,{
        shinyjs::disable("search_bttn")
        output$dim_plot <- renderUI({
            w$show()
            output$plot = renderPlot(plot_func())
            fluidRow(
                column(10, offset = 1,
                    plotOutput(ns("plot"), height = "620"),
                )
            )
        })    
        shinyjs::enable("search_bttn")
    })

    output$download_1 <- downloadHandler(
        filename = function() {
            paste0(profile(),"_",input$method,".", input$device)
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
            paste0(profile(),"_",input$method,"_DR.csv")
        },
        content = function(file) {
            data = plot_func()$data[,1:4]
            write.csv(data, file, row.names = FALSE)
        }
    )
}