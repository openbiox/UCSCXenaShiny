add_signature_UI = function(id, database = "toil"){
	ns = NS(id)

	if (!is.null(database)) {
	  if (database == "toil") {
	    choices = c("mRNA Expression","Transcript Expression","DNA Methylation",
	    						"Protein Expression","miRNA Expression","Mutation status","Copy Number Variation")
	  } else if (database == "pcawg") {
	    choices = c("mRNA Expression","Gene Fusion", "Promoter Activity",
	    						"miRNA Expression","APOBEC Mutagenesis")
	  } else if (database == "ccle") {
	    choices = c("mRNA Expression","Protein Expression",
	    						"Copy Number Variation","Mutation status")
	  }
	}
	selected = choices[1]
	tagList(
		dropMenu(
			shinyWidgets::actionBttn(
				ns("sig_edit"), "Edit",
		        style = "gradient",
		        icon = icon("arrows-to-dot"),
		        color = "primary",
		        block = TRUE,
		        size = "sm"
			),
			div(h5("Set your custom signature:"),style="width:800px;"),

			wellPanel(
				style = "background: #f7f7f7",
				h4("1. Basic information"),
				fluidRow(column(6,
					h5("(1) Signature Name"),
					textInput(ns("sig_name"), NULL, value="Signature_X"),
				)),
				fluidRow(column(6,
					h5("(2) Signature Type"),
					selectInput(ns("data_origin"),NULL,
						choices = choices, selected = selected),
				)),
			),

			wellPanel(
				style = "background: #f7f7f7",
				h4("2. Signature molecules"),
				fluidRow(
					column(
						4,
					    actionBttn(
					      inputId = ns("add_bt"),
					      label = "Select&Add molecule",
					      color = "primary",
					      style = "bordered", size = "sm",
					      block = F
					    )
					),
					column(
						4,
						virtualSelectInput(ns("add_mol"), NULL, NULL, 
						                   search = TRUE,
						                   allowNewOption = TRUE,
						                   dropboxWidth = "200%")
					)
				),
				fluidRow(column(8,
					div(style = "border-top: 1px solid #000; margin-top: 0px; margin-bottom: 0px;"),
				)),
			    fluidRow(
			    	column(3, h5("")),
			    	column(2, h5("Direction:")),
			    	column(2, h5("Absolute Coef:")),
			    	column(1, h5("Check:")),
			    ),
				fluidRow(column(8,
					div(style = "border-top: 1px dashed #000; margin-top: 0px; margin-bottom: 5px;"),
				)),
			    uiOutput(ns("multi_condi.ui")),
				fluidRow(column(8,
					div(style = "border-top: 1px solid #000; margin-top: 0px; margin-bottom: 0px;"),
				)),
				uiOutput(ns("del_reset_bt.ui")),
			    br(),
				verbatimTextOutput(ns("print_fm")),
				br(),
			),

			wellPanel(
				style = "background: #f7f7f7",
				fluidRow(column(6,h4("3. Signature data"))),
			    actionBttn(
			      inputId = ns("query_bt"),
			      label = "Query data",
			      color = "primary",
			      style = "bordered", size = "sm",
			      block = F
			    ),
			    actionBttn(
			      inputId = ns("add_custom"),
			      label = "Add/Update metadata",
			      color = "primary",
			      style = "bordered", size = "sm",
			      block = F
			    ),  
			    verbatimTextOutput(ns("sig_dat2custom_tips")),


			    dataTableOutput(ns("sig_dat_table")),
		        shinyjs::hidden(
		          wellPanel(
		            id = ns("save_csv"),
		            downloadButton(ns("downloadTable"), "Save as csv")
		          )
		        ),

			),




			placement = "right-end",
			maxWidth = "1000px"
		)

	)
}



add_signature_Server = function(input, output, session, database = "toil"){
	ns <- session$ns


	id_option = switch(database, 
			"toil"=tcga_id_option,
			"pcawg"=pcawg_id_option,
			"ccle"=ccle_id_option)

	observeEvent(input$data_origin, {
		genomic_profile_choices <- reactive({
		  genomic_profile = ifelse(is.null(input$data_origin),"mRNA Expression",input$data_origin)

		  id_option[["Molecular profile"]][[genomic_profile]]
		})
	    updateVirtualSelect(
	      "add_mol",
	      choices = genomic_profile_choices()$all,
	      selected = genomic_profile_choices()$default
	    )
	})
	dynamic_condi = reactiveValues(mols = NULL, add = 0, del = 0, sum = 0, check = NULL)

	observeEvent(input$add_bt, {
		dynamic_condi$mols = c(dynamic_condi$mols, input$add_mol)
		data_type = switch(input$data_origin,
               `mRNA Expression` = "mRNA",
               `Transcript Expression` = "transcript",
               `DNA Methylation` = "methylation",
               `Protein Expression` = "protein",
               `miRNA Expression` = "miRNA",
               `Mutation status` = "mutation",
               `Copy Number Variation` = "cnv",
               # `mRNA Expression` = "mRNA",
               `Promoter Activity` = "promoter",
               `Gene Fusion` = "fusion",
               # `miRNA Expression` = 'miRNA',
               `APOBEC Mutagenesis` = "APOBEC"
		)
		check_dat = query_pancan_value(input$add_mol, data_type = data_type, database = database)
		check_dat = all(is.na(check_dat[[1]])) #判断是否为NA值
		check_dat = ifelse(check_dat, "×", "√")
		dynamic_condi$check = c(dynamic_condi$check, check_dat)
	})

	observeEvent(input$del_bt, {
		# 删除末尾元素
		if(length(dynamic_condi$mols)>0){
			dynamic_condi$mols = dynamic_condi$mols[-1*length(dynamic_condi$mols)]
		}
	})
	observeEvent(input$reset_bt, {
		# 重置所有元素
		if(length(dynamic_condi$mols)>0){
			dynamic_condi$mols = NULL
		}
	})
	observeEvent(input$data_origin, {
		dynamic_condi$mols = NULL
	})

	output$multi_condi.ui = renderUI({
    	if(length(dynamic_condi$mols) == 0) return()
    	inputTagList <- tagList()
    	lapply(seq(dynamic_condi$mols),function(i){
    		# item1(id) -- direction(label)
    		id_condi_item1 = paste0("item1_",i)
    		# item2(id) -- coef(label)
    		id_condi_item2 = paste0("item2_",i)

    		# 添加新筛选器，不重置旧数据
			new_direc <- "+"
			if (id_condi_item1 %in% names(input)) {
				new_direc <- input[[id_condi_item1]]
			}
			new_coef = 1
			if (id_condi_item2 %in% names(input)) {
				new_coef <- input[[id_condi_item2]]
			}

			dynamic_input = fluidRow(
				column(
					3, 
					p(i,". ",strong(dynamic_condi$mols[i]))
				),
				column(
					2, 
					selectInput(ns(id_condi_item1), NULL,#label_condi_item1, 
	    						c("+","-"), selected=new_direc)),
				column(
					2, 
					numericInput(ns(id_condi_item2),  NULL,#label_condi_item3, 
	    						value = new_coef)),
				column(
					1,
					p(strong(dynamic_condi$check[i]))
				)
			)

			inputTagList <<- tagAppendChild(inputTagList,       
			                              	dynamic_input)
    	})
    	inputTagList
	})

	output$del_reset_bt.ui = renderUI({
		if(length(dynamic_condi$mols)>0){
			tagList(
			    actionBttn(
			      inputId = ns("del_bt"),
			      label = "Del last molecule",
			      color = "primary",
			      style = "bordered", size = "sm",
			      block = F
			    ),
			    actionBttn(
			      inputId = ns("reset_bt"),
			      label = "Reset all molecules",
			      color = "primary",
			      style = "bordered", size = "sm",
			      block = F
			    )
			)
		}
	})


	formula = reactive({
		if(length(dynamic_condi$mols)==0) return(NULL)
		f_left = paste0(input$sig_name, " = ")
		f_right = sapply(seq(dynamic_condi$mols), function(i){
			# i = 1
			paste0(input[[paste0("item1_",i)]],
				  " ",
				  input[[paste0("item2_",i)]],
				  "*",
				  dynamic_condi$mols[i])
		},USE.NAMES = FALSE) %>% paste0(., collapse = " ")
		paste0(f_left, f_right)
	})

	output$print_fm = renderPrint({
		cat(formula())
	})

	sig_dat = eventReactive(input$query_bt,{
		# isolate() 仅点击query/add按钮时才更新signature name/type等
		fm = str_split(isolate(formula()), " = ")[[1]][2]
		data_type = switch(isolate(input$data_origin),
               `mRNA Expression` = "mRNA",
               `Transcript Expression` = "transcript",
               `DNA Methylation` = "methylation",
               `Protein Expression` = "protein",
               `miRNA Expression` = "miRNA",
               `Mutation status` = "mutation",
               `Copy Number Variation` = "cnv",
               # `mRNA Expression` = "mRNA",
               `Promoter Activity` = "promoter",
               `Gene Fusion` = "fusion",
               # `miRNA Expression` = 'miRNA',
               `APOBEC Mutagenesis` = "APOBEC"
		)
		dat = query_pancan_value(fm, data_type = data_type, database = database)
		dat = data.frame(dat[[1]]) %>% 
		  tibble::rownames_to_column("Sample")
		colnames(dat)[2] = str_split(isolate(formula()), " = ")[[1]][1]
		dat
	})

	output$sig_dat_table = renderDataTable({
		datatable(sig_dat(),options = list(pageLength = 3,
			columnDefs = list(list(className = 'dt-center', targets="_all"))))
	})

	observeEvent(input$query_bt, {
	  if (length(dynamic_condi$mols) >= 1) {
	    shinyjs::show(id = "save_csv")
	  } else {
	    shinyjs::hide(id = "save_csv")
	  }
	})

	output$downloadTable <- downloadHandler(
	  filename = function() {
	    paste0(input$sig_name, ".csv")
	  },
	  content = function(file) {
	    write.csv(sig_dat(), file, row.names = FALSE)
	  }
	)

	sig_dat2custom = reactive({
		if(input$add_custom==0){
			NULL
		} else {
			sig_dat()
		}
	})

	sig_dat2custom_tips = eventReactive(input$add_custom,{
		ts = timestamp()
		paste0("Your latest quried signature has been added to the costum metadata.\n",ts)
	})
	output$sig_dat2custom_tips = renderPrint({
		cat(sig_dat2custom_tips())
	})

	return(sig_dat2custom)
}
