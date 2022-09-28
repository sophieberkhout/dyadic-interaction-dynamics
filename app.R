# load packages
library("shiny")
library("shinycssloaders")
library("ggplot2")
library("plotly")
library("markdown")

# source required files
source("simVARS.R")
source("plotFunctions.R")

# set options for app
options(shiny.autoreload = TRUE)
options(shiny.error = FALSE)
options(spinner.type = 7)
options(spinner.size = 0.2)
options(spinner.color = "grey")

ui <- tagList(
  tags$head(tags$style(HTML("
                            .navbar-nav {
                              float: none !important;
                            }
                            .navbar-nav > li:nth-child(1) {
                              float: right;
                            }
                            #element {
                              transform: scale(1);
                              transform-origin: top center;
                            }

                            "))),
  navbarPage("Dyadic Interaction Dynamics",
    id = "topnavbar",
    selected = "simulate",
# TERMS Of USAGE
    tabPanel(
      "By using this app you agree with the Terms of Usage.",
      fluidRow(
        column(6, includeMarkdown("Shiny/tou.Rmd"))
      )
    ),
# SIMULATE TAB
    tabPanel("Simulate",
      value = "simulate",
      h4("Simulate Data"),
      tabsetPanel(
        id = "simulateTabs",
        tabPanel("Set up & Visualization",
          value = "sim",
          br(),
# DATA GENERATING MODEL
          selectInput("model", "Choose a data generating model",
            list(
              "First-order vector autoregressive VAR(1)" = "VAR",
              "Latent VAR(1)" = "L",
              "Time-varying VAR(1)" = "TV",
              "Threshold VAR(1)" = "T",
              "Hidden Markov model" = "HMM",
              "Markov-switching VAR(1)" = "MS"
            ),
            selected = "VAR"
          ),
          hr(),
# INPUT DATA GENERATION
          fluidRow(
            column(
              2,
              style = "padding-top:2em",
              methodUI("method") # measurement occasions and seed
            ),
            column(
              10,
              fluidRow(
              # input y
                column(
                  4,
                  formulaModelUI("formula_model_y"),
                  paramsUI("innerParamsY"),
                  hr(),
                  conditionalPanel(
                    condition = "input.model == 'T'",
                    numericInput(
                      "tau_y", HTML("Threshold &#120591;"),
                      0, width = "30%"
                    )
                  ),
                  formulaUI("formula_y")
                ),
              # input x
                column(
                  4,
                  formulaModelUI("formula_model_x"),
                  paramsUI("innerParamsX"),
                  hr(),
                  conditionalPanel(
                    condition = "input.model == 'T'",
                    numericInput(
                      "tau_x", HTML("Threshold &#120591;"), 
                      0, width = "30%"
                    )
                  ),
                  formulaUI("formula_x")
                ),
              # input residuals
                column(
                  4,
                  formulaModelUI("formula_model_z"),
                  errorsUI("errors"),
                  conditionalPanel(
                    condition = "input.model == 'T'",
                    hr(),
                    h5("For all regime combinations"),
                    fluidRow(
                      column(
                        6,
                        numericInput("yx_T", "Correlation", .3, -1, 1, .1),
                      )
                    )
                  ),
                # transition probabilities
                  conditionalPanel(
                    condition = "input.model == 'MS' || input.model == 'HMM'",
                    tabsetPanel(
                      id = "transition",
                      tabPanel(
                        "Transition probabilities",
                        fluidRow(
                          style = "padding-top:5px",
                          column(
                            6,
                            numericInput(
                              "pi_o", "Stay in 1", .5, 0, 1, .1,
                              width = "60%"
                            ),
                            tableOutput("pi_ot")
                          ),
                          column(
                            6,
                            tableOutput("pi_to"),
                            numericInput(
                              "pi_t", "Stay in 2", .5, 0, 1, .1,
                              width = "60%"
                            )
                          )
                        )
                      )
                    )
                  ),
                  hr(),
                  formulaUI_z("formula_z")
                )
              )
            )
          ),
        # time-varying plots
          conditionalPanel(
            condition = "input.model == 'TV'",
            hr(),
            h4("Parameters plotted over time"),
            plotsTvUI("tvPlots"),
          ),
          hr(),
        # generated data plots
          plotsInputUI("inputPlots")
        ),
        # tabPanel("Estimation",
        #   value = "estimation",
        #   estimationUI("estimation")
        # ),
      # download generated data
        tabPanel("Download",
          value = "data",
          fluidRow(
            style = "padding-top:5px",
            column(
              9,
              withSpinner(DT::dataTableOutput("table"))
            ),
            column(
              3,
              radioButtons(
                "dataFormat", "Choose data format",
                choices = list('"Wide"' = "wide", "Long" = "long")
              ),
              downloadButton("downloadData", "Download data")
            )
          )
        )
      )
    ),
# UPLOAD TAB
    tabPanel(
      "Upload",
      h4("Upload Data"),
      uploadInputUI("uploadData"),
      hr(),
      plotsInputUI("uploadPlots")
    ),
# INFO TAB
    tabPanel(
      "Info",
      value = "info",
      h4("Info"),
      fluidRow(
        column(
          8,
          navlistPanel(
            well = FALSE,
            tabPanel("Plots", shiny::includeMarkdown("Shiny/plots.Rmd")),
            "Simulate",
            tabPanel("Set up & Visualization", shiny::includeMarkdown("Shiny/setup.Rmd")),
            tabPanel("Download", shiny::includeMarkdown("Shiny/download.Rmd")),
            "Upload",
            tabPanel("Upload Data", shiny::includeMarkdown("Shiny/upload.Rmd"))
          )
        ),
        column(
          4,
          sidebarPanel(includeMarkdown("Shiny/info.Rmd"), width = 12)
        )
      )
    )
  )
)

server <- function(input, output, session) {

#--------------------------------- SIMULATE -----------------------------------#
  method <- methodServer("method")

  formulaModelServer("formula_model_y", reactive({ input$model }), "y")
  formulaModelServer("formula_model_x", reactive({ input$model }), "x")
  formulaModelServer("formula_model_z", reactive({ input$model }), "z")

  # the sim tab should have long format data for plotting
  observeEvent(input$simulateTabs, {
    if (input$simulateTabs == "sim") {
      updateRadioButtons(session, "dataFormat", selected = "long")
    }
    if (input$simulateTabs == "data") {
      updateRadioButtons(session, "dataFormat", selected = "wide")
    }
  })

  # opposite transition probabilities
  output$pi_to <- renderTable({
      pi_to <- data.frame(1 - input$pi_t)
      colnames(pi_to) <- "Switch to 1"
      return(pi_to)
    },
    align = "l"
  )

  output$pi_ot <- renderTable({
      pi_ot <- data.frame(1 - input$pi_o)
      colnames(pi_ot) <- "Switch to 2"
      return(pi_ot)
    },
    align = "l"
  )

  params_y <- paramsServer(
    "innerParamsY", model = reactive({ input$model }), t = method$t,
    tau = reactive({ input$tau_y }), paramsOther = params_x, partner = "y"
  )

  params_x <- paramsServer(
    "innerParamsX", model = reactive({ input$model }), t = method$t,
    tau = reactive({ input$tau_x }), paramsOther = params_y, partner = "x"
  )

  errors <- errorsServer("errors", model = reactive({ input$model }))

  probs <- reactive({
    p <- NULL
    if (input$model == "MS" || input$model == "HMM") {
      p <- c(input$pi_o, input$pi_t)
    }
    return(p)
  })

  # GENERATE DATA
  dat <- reactive({
    ifelse(input$dataFormat == "long", longformat <- TRUE, longformat <- FALSE)

    set.seed(method$seed())
    dat <- simVARS(
      occasions = method$t(),
      burnin = 100,
      type = input$model,
      params_y = params_y()$coefs,
      params_x = params_x()$coefs,
      probs = probs(),
      indicators_y = params_y()$indicator,
      indicators_x = params_x()$indicator,
      errors = errors()$measurement,
      innovations = errors()$dynamic,
      longformat = longformat
    )
    dat
  })

  # estimationServer("estimation", dataFormat, dat, params, reactive({ input$model }))

  formulaServer(
    "formula_y", reactive({ input$model }), partner = "y",
    params = params_y, reactive({ input$tau_y })
  )

  formulaServer(
    "formula_x", reactive({ input$model }), partner = "x",
    params = params_x, reactive({ input$tau_x })
  )

  formulaServer_z(
    "formula_z", reactive({ input$model }), reactive({ input$yx_T }), errors
  )

  # PLOTS
  dataFormat <- reactive({
    input$dataFormat
  })

  plotsServer("inputPlots", dataFormat, reactive({ input$model }), dat, 
    tau = reactive({ list(y = input$tau_y, x = input$tau_x) })
  )
  
  plotsTvServer("tvPlots", method$t, y = params_y, x = params_x)

  # TABLE
  output$table <- DT::renderDataTable({
    dtable <- DT::datatable(dat(),
      rownames = FALSE,
      options = list(dom = "pt", pageLength = 15)
    )

    cols <- names(dplyr::select(dat(), where(is.numeric)))
    cols <- cols[-which(
      cols == "t" | cols == "regime" | cols == "regime_y" | cols == "regime_x"
    )]

    dtable <- DT::formatRound(dtable, columns = cols, digits = 3)
    dtable
  })

  # DOWNLOAD BUTTON
  output$downloadData <- downloadHandler(
    filename = "dyadic-interaction-dynamics.csv",
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )

#----------------------------------- UPLOAD -----------------------------------#
  uploaded <- uploadInputServer("uploadData")

  plotsServer("uploadPlots", dataFormat,
    model = function() { "VAR" },
    dat = uploaded$datLong, uploaded = TRUE,
    uploadedFile = uploaded$file
  )
}

shinyApp(
  ui = ui,
  server = server
)
