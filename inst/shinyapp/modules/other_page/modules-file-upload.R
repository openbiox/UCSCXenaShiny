ui.file_upload <- function(id, test = FALSE) {
  ns <- NS(id)

  wPanel <- wellPanel(
    # Input: Select a file ----
    fileInput(ns("upload_file"), "Choose CSV File",
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),

    # Horizontal line ----
    tags$hr(),

    # Input: Checkbox if file has header ----
    checkboxInput(ns("header"), "Header", TRUE),

    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
      choices = c(
        Comma = ",",
        Semicolon = ";",
        Tab = "\t"
      ),
      selected = ","
    ),

    # Input: Select quotes ----
    radioButtons(ns("quote"), "Quote",
      choices = c(
        None = "",
        "Double Quote" = '"',
        "Single Quote" = "'"
      ),
      selected = '"'
    ),

    # Horizontal line ----
    tags$hr(),

    # Input: Select number of rows to display ----
    radioButtons(ns("disp"), "Display",
      choices = c(
        Head = "head",
        All = "all"
      ),
      selected = "head"
    )
  )

  if (test) {
    fluidPage(
      titlePanel("Module: Upload File (here for testing only)"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          wPanel
        ),
        mainPanel = mainPanel(
          # Output: Data file ----
          tableOutput(ns("contents"))
        )
      )
    )
  } else {
    wPanel
  }
}

server.file_upload <- function(input, output, session) {
  ## https://shiny.rstudio.com/articles/modules.html
  # ns <- session$ns

  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$upload_file, message = FALSE))
    input$upload_file
  })

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    message("rendering table")

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(userFile()$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
}
