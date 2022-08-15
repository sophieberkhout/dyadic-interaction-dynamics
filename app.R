# install required packages
if (!("shinycssloaders" %in% installed.packages())) install.packages("shinycssloaders")
if (!("ggplot2" %in% installed.packages())) install.packages("ggplot2")
if (!("plotly" %in% installed.packages())) install.packages("plotly")
if (!("DT" %in% installed.packages())) install.packages("DT")
if (!("viridis" %in% installed.packages())) install.packages("viridis")
if (!("oddsratio" %in% installed.packages())) install.packages("oddsratio")
if (!("MASS" %in% installed.packages())) install.packages("MASS")
if (!("stringr" %in% installed.packages())) install.packages("stringr")

# load packages
library("shiny")
library("shinycssloaders")
library("ggplot2")
library("plotly")

# source required files
source("simVARS.R")
source("plotFunctions.R")
source("analysesFunctions.R")

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
    tabPanel(
      "By using this app you agree with the Terms of Usage.",
      fluidRow(
        column(6, includeMarkdown("Shiny/tou.Rmd"))
      )
    ),
    tabPanel("Simulate",
      value = "simulate",
      h4("Simulate"),
      navlistPanel(
        id = "simulateTabs", widths = c(2, 10),
        well = FALSE,
        tabPanel("Set up & Visualization",
          value = "sim",
          fluidRow(
            column(12, methodUI("method"))
          ),
          fluidRow(
            column(
              4,
              formulaModelUI("formula_model_y"),
              tabsetPanel(
                id = "yTabs",
                #  paramsUI("innerParams"),
                tabPanel(
                  "Regression coefficients",
                  inputVARUI("yParameters")
                ),
                tabPanel(
                  HTML("Intercept &#120572;"),
                  tvUI("intercept_y", type = "num")
                ),
                tabPanel(
                  HTML("Carryover &#120601;"),
                  tvUI("carryover_y")
                ),
                tabPanel(
                  HTML("Spillover &#120573;"),
                  tvUI("spillover_y")
                ),
                tabPanel(
                  "Means",
                  meansUI("means_y")
                ),
                tabPanel(
                  "Indicator",
                  indicatorUI("i_y")
                ),
                tabPanel(
                  "Regime 1",
                  inputVARUI("yFirstRegime")
                ),
                tabPanel(
                  "Regime 2",
                  inputVARUI("ySecondRegime")
                )
              ),
              hr(),
              conditionalPanel(
                condition = "input.model == 'T'",
                numericInput("tau_y", HTML("Threshold &#120591;"), 0, width = "30%"),
                ns = NS("method")
              ),
              formulaUI_y("formula_y")
            ),
            column(
              4,
              formulaModelUI("formula_model_x"),
              tabsetPanel(
                id = "xTabs",
                tabPanel(
                  "Regression coefficients",
                  inputVARUI("xParameters")
                ),
                tabPanel(
                  HTML("Intercept &#120572;"),
                  tvUI("intercept_x", type = "num")
                ),
                tabPanel(
                  HTML("Carryover &#120601;"),
                  tvUI("carryover_x")
                ),
                tabPanel(
                  HTML("Spillover &#120573;"),
                  tvUI("spillover_x")
                ),
                tabPanel(
                  "Means",
                  meansUI("means_x")
                ),
                tabPanel(
                  "Indicator",
                  indicatorUI("i_x")
                ),
                tabPanel(
                  "Regime 1",
                  inputVARUI("xFirstRegime")
                ),
                tabPanel(
                  "Regime 2",
                  inputVARUI("xSecondRegime")
                )
              ),
              hr(),
              conditionalPanel(
                condition = "input.model == 'T'",
                numericInput("tau_x", HTML("Threshold &#120591;"), 0, width = "30%"),
                ns = NS("method")
              ),
              formulaUI_x("formula_x")
            ),
            column(
              4,
              formulaModelUI("formula_model_z"),
              tabsetPanel(
                id = "errors",
                tabPanel(
                  "Innovation parameters",
                  errorsUI("innovations")
                ),
                tabPanel(
                  "Measurement error parameters",
                  errorsUI("measurementError")
                ),
                tabPanel(
                  "Measurement error regime 1",
                  errorsUI("measurementErrorFirstRegime")
                ),
                tabPanel(
                  "Measurement error regime 2",
                  errorsUI("measurementErrorSecondRegime")
                ),
                tabPanel(
                  "Innovation regime 1",
                  errorsUI("innovationsFirstRegime")
                ),
                tabPanel(
                  "Innovation regime 2",
                  errorsUI("innovationsSecondRegime")
                )
              ),
              conditionalPanel(
                condition = "input.model == 'T'",
                hr(),
                h5("For all regime combinations"),
                fluidRow(
                  column(
                    6,
                    numericInput("yx_T", "Correlation", .3, -1, 1, .1),
                  )
                ), ns = NS("method")
              ),
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
                        numericInput("pi_o", "Stay in 1", .5, 0, 1, .1, width = "60%"),
                        tableOutput("pi_ot")
                      ),
                      column(
                        6,
                        tableOutput("pi_to"),
                        numericInput("pi_t", "Stay in 2", .5, 0, 1, .1, width = "60%")
                      )
                    )
                  )
                ), ns = NS("method")
              ),
              hr(),
              formulaUI_z("formula_z")
            )
          ),
          conditionalPanel(
            condition = "input.model == 'TV'",
            hr(),
            h4("Parameters plotted over time"),
            plotstvUI("tvPlots"),
            ns = NS("method")
          ),
          hr()
        ),
        tabPanel("Estimation",
          value = "estimation",
          estimationUI("estimation")
        ),
        tabPanel("Download",
          value = "data",
          fluidRow(
            column(
              9,
              withSpinner(DT::dataTableOutput("table"))
            ),
            column(
              3,
              radioButtons("dataFormat", "Choose data format",
                choices = list('"Wide"' = "wide", "Long" = "long")
              ),
              downloadButton("downloadData", "Download data")
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.simulateTabs == 'sim'",
        plotsInputUI("inputPlots")
      )
    ),
    tabPanel(
      "Upload",
      h4("Data"),
      navlistPanel(
        id = "uploadTabs", widths = c(2, 10), well = FALSE,
        tabPanel("Upload & Visualization",
          value = "upload",
          uploadInputUI("uploadData")
        ),
        tabPanel("Estimation")
      ),
      conditionalPanel(
        condition = "input.uploadTabs == 'upload'",
        plotsInputUI("uploadPlots")
      )
    ),
    tabPanel("Info",
      value = "info",
      fluidRow(
        column(
          4,
          includeMarkdown("Shiny/help.Rmd")
        ),
        column(
          4,
          includeMarkdown("Shiny/help_data.Rmd")
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

  #-------------------------------- SIMULATE ----------------------------------#
  # DYNAMIC INPUT
  method <- methodServer("method")

  formulaModelServer("formula_model_y", method$model, "y")
  formulaModelServer("formula_model_x", method$model, "x")
  formulaModelServer("formula_model_z", method$model, "z")

  observeEvent(method$model(),
    {
      hideTab("yTabs", target = "Regime 1")
      hideTab("xTabs", target = "Regime 1")
      hideTab("yTabs", target = "Regime 2")
      hideTab("xTabs", target = "Regime 2")
      hideTab("yTabs", target = HTML("Intercept &#120572;"))
      hideTab("yTabs", target = HTML("Carryover &#120601;"))
      hideTab("yTabs", target = HTML("Spillover &#120573;"))
      hideTab("xTabs", target = HTML("Intercept &#120572;"))
      hideTab("xTabs", target = HTML("Carryover &#120601;"))
      hideTab("xTabs", target = HTML("Spillover &#120573;"))
      hideTab("yTabs", target = "Regression coefficients")
      hideTab("xTabs", target = "Regression coefficients")
      hideTab("yTabs", target = "Means")
      hideTab("xTabs", target = "Means")
      hideTab("yTabs", target = "Indicator")
      hideTab("xTabs", target = "Indicator")
      hideTab("errors", target = "Innovation parameters")
      hideTab("errors", target = "Measurement error parameters")
      hideTab("errors", target = "Innovation regime 1")
      hideTab("errors", target = "Measurement error regime 1")
      hideTab("errors", target = "Measurement error regime 2")
      hideTab("errors", target = "Innovation regime 2")

      if (method$model() == "VAR" || method$model() == "L") {
        showTab("yTabs", "Regression coefficients", select = TRUE)
        showTab("xTabs", "Regression coefficients", select = TRUE)
      }

      if (method$model() == "VAR" || method$model() == "L" || method$model() == "TV") {
        showTab("errors", "Innovation parameters", select = TRUE)
      }

      if (method$model() == "HMM") {
        showTab("errors", "Measurement error regime 1", select = TRUE)
        showTab("errors", "Measurement error regime 2")
      }

      if (method$model() == "TV") {
        showTab("yTabs", HTML("Intercept &#120572;"), select = TRUE)
        showTab("yTabs", HTML("Carryover &#120601;"))
        showTab("yTabs", HTML("Spillover &#120573;"))

        showTab("xTabs", HTML("Intercept &#120572;"), select = TRUE)
        showTab("xTabs", HTML("Carryover &#120601;"))
        showTab("xTabs", HTML("Spillover &#120573;"))
      }

      if (method$model() == "HMM") {
        showTab("yTabs", "Means", select = TRUE)
        showTab("xTabs", "Means", select = TRUE)
      }

      if (method$model() == "L") {
        showTab("yTabs", "Indicator")
        showTab("xTabs", "Indicator")
        showTab("errors", "Measurement error parameters", select = FALSE)
      }

      if (method$model() == "T" || method$model() == "MS") {
        showTab("yTabs", "Regime 1")
        showTab("xTabs", "Regime 1")
        showTab("yTabs", "Regime 2")
        showTab("xTabs", "Regime 2")
        showTab("errors", "Innovation regime 1", select = TRUE)
        showTab("errors", "Innovation regime 2")
      }
    },
    priority = 1
  )

  observeEvent(input$simulateTabs, {
    if (input$simulateTabs == "sim") {
      updateRadioButtons(session, "dataFormat", selected = "long")
    }
    if (input$simulateTabs == "data") {
      updateRadioButtons(session, "dataFormat", selected = "wide")
    }
  })

  output$pi_to <- renderTable(
    {
      pi_to <- data.frame(1 - input$pi_t)
      colnames(pi_to) <- "Switch to 1"
      return(pi_to)
    },
    align = "l"
  )

  output$pi_ot <- renderTable(
    {
      pi_ot <- data.frame(1 - input$pi_o)
      colnames(pi_ot) <- "Switch to 2"
      return(pi_ot)
    },
    align = "l"
  )

  tv_alpha_y <- tvServer("intercept_y", method$t)
  tv_phi_y <- tvServer("carryover_y", method$t)
  tv_beta_y <- tvServer("spillover_y", method$t)

  tv_alpha_x <- tvServer("intercept_x", method$t)
  tv_phi_x <- tvServer("carryover_x", method$t)
  tv_beta_x <- tvServer("spillover_x", method$t)

  means_y <- meansServer("means_y")
  means_x <- meansServer("means_x")

  i_y <- indicatorServer("i_y")
  i_x <- indicatorServer("i_x")

  innovations <- errorsServer("innovations")
  innovations_1 <- errorsServer("innovationsFirstRegime")
  innovations_2 <- errorsServer("innovationsSecondRegime")
  measurement_errors <- errorsServer("measurementError")
  measurement_errors_1 <- errorsServer("measurementErrorFirstRegime")
  measurement_errors_2 <- errorsServer("measurementErrorSecondRegime")

  params_y <- inputVARServer("yParameters")
  params_x <- inputVARServer("xParameters")
  params_y_1 <- inputVARServer("yFirstRegime")
  params_x_1 <- inputVARServer("xFirstRegime")
  params_y_2 <- inputVARServer("ySecondRegime")
  params_x_2 <- inputVARServer("xSecondRegime")

  params <- reactive({
    params_y <- list(alpha = params_y$alpha(), phi = params_y$phi(), beta = params_y$beta())
    params_x <- list(alpha = params_x$alpha(), phi = params_x$phi(), beta = params_x$beta())

    innovations <- c(innovations$y(), innovations$c_yx(), innovations$x())

    if (method$model() == "T" || method$model() == "MS") {
      params_y <- list(alpha = params_y_1$alpha(), phi = params_y_1$phi(), beta = params_y_1$beta())
      params_x <- list(alpha = params_x_1$alpha(), phi = params_x_1$phi(), beta = params_x_1$beta())

      params_y$alpha[2] <- params_y_2$alpha()
      params_y$phi[2] <- params_y_2$phi()
      params_y$beta[2] <- params_y_2$beta()
      if (method$model() == "T") params_y$tau <- input$tau_y

      params_x$alpha[2] <- params_x_2$alpha()
      params_x$phi[2] <- params_x_2$phi()
      params_x$beta[2] <- params_x_2$beta()
      if (method$model() == "T") params_x$tau <- input$tau_x

      innovations_1 <- c(innovations_1$y(), innovations_1$c_yx(), innovations_1$x())
      innovations_2 <- c(innovations_2$y(), innovations_2$c_yx(), innovations_2$x())
      if (method$model() == "T") innovations_1[2] <- input$yx_T

      innovations <- list(
        firstRegime = innovations_1,
        secondRegime = innovations_2
      )
    }
    if (method$model() == "HMM") {
      params_y <- list(mu = c(means_y$mu_1(), means_y$mu_2()))
      params_x <- list(mu = c(means_x$mu_1(), means_x$mu_2()))

      measurement_errors_1 <- c(
        measurement_errors_1$y(), measurement_errors_1$c_yx(),
        measurement_errors_1$x()
      )

      measurement_errors_2 <- c(
        measurement_errors_2$y(), measurement_errors_2$c_yx(),
        measurement_errors_2$x()
      )

      measurement_errors <- list(
        firstRegime = measurement_errors_1,
        secondRegime = measurement_errors_2
      )
    }
    if (method$model() == "TV") {
      params_y <- list(alpha = tv_alpha_y$p(), phi = tv_phi_y$p(), beta = tv_beta_y$p())
      params_x <- list(alpha = tv_alpha_x$p(), phi = tv_phi_x$p(), beta = tv_beta_x$p())
    }

    indicators_y <- NULL
    indicators_x <- NULL
    if (method$model() == "L") {
      indicators_y <- list(m = i_y$mean(), l = 1)
      indicators_x <- list(m = i_x$mean(), l = 1)

      measurement_errors <- c(
        measurement_errors$y(), measurement_errors$c_yx(),
        measurement_errors$x()
      )
    }

    probs <- NULL
    if (method$model() == "MS" || method$model() == "HMM") probs <- c(input$pi_o, input$pi_t)


    if (method$model() != "L" & method$model() != "HMM") measurement_errors <- NULL

    return(
      list(
        y = params_y,
        x = params_x,
        innovations = innovations,
        errors = measurement_errors,
        indicators_y = indicators_y,
        indicators_x = indicators_x,
        probs = probs
      )
    )
  })

  # params <- paramsServer("innerParams", model = method$model, t = method$t,
  #                        pi_o = reactive({input$pi_o}), pi_t = reactive({input$pi_t}))

  # GENERATE DATA
  dat <- reactive({
    ifelse(input$dataFormat == "long", longformat <- TRUE, longformat <- FALSE)

    set.seed(method$seed())
    dat <- simVARS(
      occasions = method$t(), burnin = 100,
      type = method$model(),
      params_y = params()$y,
      params_x = params()$x,
      probs = params()$probs,
      indicators_y = params()$indicators_y,
      indicators_x = params()$indicators_x,
      errors = params()$errors,
      innovations = params()$innovations,
      longformat = longformat
    )

    dat
  })

  estimationServer("estimation", dataFormat, dat, params)

  formulaServer_y("formula_y", method$model,
    params_y, params_y_1, params_y_2,
    reactive({
      input$tau_y
    }),
    i_y,
    means_y,
    tv = list(alpha_y = tv_alpha_y, phi_y = tv_phi_y, beta_y = tv_beta_y)
  )

  formulaServer_x("formula_x", method$model,
    params_x, params_x_1, params_x_2,
    reactive({
      input$tau_x
    }),
    i_x,
    means_x,
    tv = list(alpha_x = tv_alpha_x, phi_x = tv_phi_x, beta_x = tv_beta_x)
  )

  formulaServer_z(
    "formula_z", method$model,
    reactive({
      input$yx_T
    }),
    innovations, innovations_1, innovations_2,
    measurement_errors, measurement_errors_1, measurement_errors_2
  )

  # PLOTS
  dataFormat <- reactive({
    input$dataFormat
  })
  plotsServer("inputPlots", dataFormat, method$model, dat)
  plotstvServer("tvPlots", method$t,
    tv = list(
      alpha_y = tv_alpha_y, phi_y = tv_phi_y, beta_y = tv_beta_y,
      alpha_x = tv_alpha_x, phi_x = tv_phi_x, beta_x = tv_beta_x
    )
  )

  # TABLE
  output$table <- DT::renderDataTable({
    dtable <- DT::datatable(dat(),
      rownames = FALSE,
      options = list(dom = "pt", pageLength = 15)
    )
    if (input$dataFormat == "wide") {
      dtable <- DT::formatRound(dtable, columns = c("x", "y"), digits = 3)
    }
    dtable
  })

  # DOWNLOAD BUTTON
  output$downloadData <- downloadHandler(
    filename = "dyadic-interaction-dynamics.csv",
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )

  #---------------------------------- UPLOAD ----------------------------------#
  uploaded <- uploadInputServer("uploadData")

  plotsServer("uploadPlots", dataFormat,
    model = function() {
      "VAR"
    },
    dat = uploaded$datLong, uploaded = TRUE,
    uploadedFile = uploaded$file
  )
}

# shinyApp(ui = tagList(
#                 tags$style("
#                   body {
#                  -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
#                  zoom: 0.8; /* Other non-webkit browsers */
#                  zoom: 80%; /* Webkit browsers */
#                  }
#                  "),
#                 ui
#               ),
#          server = server)

shinyApp(
  ui = ui,
  server = server
)
