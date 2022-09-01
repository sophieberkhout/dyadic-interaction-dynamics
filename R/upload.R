uploadInputUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      4,
      p("The data file should have three columns: t, x, and y."),
      fileInput(ns("file"), "Choose CSV File",
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      hr(),
      radioButtons(ns("sep"), "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      )
    ),
    column(
      8,
      DT::dataTableOutput(ns("uploadedDataTable")),
      hr()
    )
  )
}

uploadInputServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploadedData <- reactive({
        df <- read.csv(input$file$datapath,
          header = T,
          sep = input$sep
        )

        return(df)
      })

      uploadedDataLong <- reactive({
        df_long <- uploadedData()
        df_long <- tidyr::pivot_longer(df_long,
          cols = c("x", "y"),
          names_to = "partner",
          values_to = "value"
        )
        return(df_long)
      })

      output$uploadedDataTable <- DT::renderDataTable({
        validate(need(input$file, message = "Please upload a data file."))

        dtable <- DT::datatable(uploadedData(),
          rownames = F,
          options = list(dom = "pt", pageLength = 5)
        )
        dtable
      })

      return(
        list(
          datLong = uploadedDataLong,
          dat = uploadedData,
          file = reactive({
            input$file
          })
        )
      )
    }
  )
}
