download_res_UI = function(id){
    ns = NS(id)
    tagList(
        fluidRow(
            column(3, 
                div(shinyjs::hidden(downloadButton(ns("save_plot_bt"), "Figure")), style="display: inline-block;vertical-align:top;"),
                div(style="display: inline-block;vertical-align:top",
                    dropMenu(  
                        actionBttn(ns("plot_opt"),label = NULL,style = "material-circle", 
                                    color = "success",icon = icon("gear")),
                        div(h3("1. Height:"),style="width:400px;"),
                        numericInput(ns("save_plot_H"), NULL ,min = 1, max = 20, value = 10, step = 0.5),
                        div(h3("2. Width:"),style="width:400px;"),
                        numericInput(ns("save_plot_W"), NULL ,min = 1, max = 20, value = 10, step = 0.5),	
                        div(h3("3. Format:"),style="width:400px;"),		
                        radioGroupButtons(
                            inputId = ns("save_plot_F"),
                            label = NULL,
                            status = "primary",
                            choices = c("PDF", "PNG"),
                            justified = TRUE
                        ),
                        placement = "top"
                    )
                )
            ),
            column(3, offset = 0, shinyjs::hidden(downloadButton(ns("save_data_raw"), "Raw data(.csv)"))),
            column(3, offset = 0, shinyjs::hidden(downloadButton(ns("save_data_res"), "Statistical data(.csv)")))
        )
    )
}


download_res_Server = function(input, output, session,
                               res1=NULL, res2=NULL, res3=NULL){
    ns <- session$ns

	# three download buttons
    observeEvent(req(res1),{
        shinyjs::show("save_plot_bt")
    })
    observeEvent(req(res2),{
        shinyjs::show("save_data_raw")
    })
    observeEvent(req(res3),{
        shinyjs::show("save_data_res")
    })

	output$save_plot_bt = downloadHandler(
		filename = function(){
			paste0("Plot", "_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".",tolower(input$save_plot_F))
		},
		content = function(file){
			# p = cor_plot_bar()
            p = res1
		    if (input$save_plot_F == "PDF") {
		      pdf(file, width = input$save_plot_W, height = input$save_plot_H, onefile = FALSE)
		      print(p)
		      dev.off()
		    } else if (input$save_plot_F == "PNG"){
		      png(file, width = input$save_plot_W, height = input$save_plot_H, res = 600, units = "in")
		      print(p)
		      dev.off()
		    }
		}
	)

	output$save_data_raw = downloadHandler(
		filename = function(){
			paste0("Rawdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			# p_raw = merge_data_bar()
            p_raw = res2
			write.csv(p_raw, file, row.names = FALSE)
		}
	)
	output$save_data_res = downloadHandler(
		filename = function(){
			paste0("Statdata_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
		},
		content = function(file){
			# p_cor = cor_data_bar()
			# p_cor$parameter1 = unique(merge_data_bar()$x_axis)
			# p_cor$parameter2 = unique(merge_data_bar()$y_axis)	
            p_stat = res3
			write.csv(p_stat, file, row.names = FALSE)
		}
	)


}