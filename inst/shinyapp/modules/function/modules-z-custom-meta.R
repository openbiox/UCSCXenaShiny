custom_meta_UI = function(id, button_name="Multi-conditions filters"){
	ns = NS(id)
	tagList(
		fluidRow(
			column(8,fileInput(ns("upload_sp_info"),"", accept = ".csv")),
			column(3,br(),downloadButton(ns("example_sp_info"), "e.g."))
		)
	)
}



custom_meta_Server = function(input, output, session, database = "toil"){
	ns <- session$ns
	custom_meta = reactive({
		file = input$upload_sp_info
		if(is.null(file$datapath)){
			NULL
		} else {
			csv_format = tools::file_ext(file$name)=="csv"
			shinyFeedback::feedbackDanger("upload_sp_info", !csv_format, "Non .csv format file")
			req(csv_format,cancelOutput = TRUE)
			read.csv(file$datapath)
		} 
	})
	output$example_sp_info = downloadHandler(
		filename = function(){
			"example_sample_info.csv"
		},
		content = function(file){
			sp_info = query_tcga_group(database = database)$data[,"Sample"]
			set.seed(42)
			scores = matrix(rnorm(nrow(sp_info)*5,mean = 1, sd = 1), ncol = 5) %>% as.data.frame()
			colnames(scores) = paste0("TF",1:5)
			sp_info = cbind(sp_info, scores)
			write.csv(sp_info, file, row.names = FALSE)
		}
	)
	return(custom_meta)
}