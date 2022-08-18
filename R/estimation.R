estimationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        fluidRow(
          column(
            6,
            wellPanel(
              selectInput(ns("model"), "Estimation model",
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
              actionButton(ns("estimateModel"), "Fit model")
            )
          ),
          column(
            6,
            wellPanel(
              style = "background:white; border:white; box-shadow:none;",
              strong("Data generating model"),
              br(),
              textOutput(ns("modelText"))
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        6,
        withSpinner(DT::dataTableOutput(ns("estimatesTable")))
      ),
      column(
        6,
        withSpinner(DT::dataTableOutput(ns("trueTable")))
      )
    ),
    br()
  )
}

estimationServer <- function(id, dataFormat, dat, params, trueModel) {
  moduleServer(
    id,
    function(input, output, session) {
      estimates <- reactiveValues()
      # estimates <- NA
      estimates$estimated <- NA
      # estimates$difference <- NA

      output$modelText <- renderText({
        if (trueModel() == "VAR") mod <- "First-order vector autoregressive VAR(1)"
        if (trueModel() == "L") mod <- "Latent VAR(1)"
        if (trueModel() == "TV") mod <- "Time-varying VAR(1)"
        if (trueModel() == "T") mod <- "Threshold VAR(1)"
        if (trueModel() == "HMM") mod <- "Hidden Markov model"
        if (trueModel() == "MS") mod <- "Markov-switching VAR(1)"

        return(mod)
      })

      mod <- reactive({

        # true parameters
        regY <- as.numeric(params()$y)
        regX <- as.numeric(params()$x)
        innov <- as.numeric(params()$innovations)
        indY <- as.numeric(params()$indicators_y[[1]])
        indX <- as.numeric(params()$indicators_x[[1]])
        measError <- as.numeric(params()$errors)

        # if (trueModel() != "L") {
        #   indY <- indX <- NA
        #   if(tueModel() != "HMM") measError <- rep(NA, 3)
        # }

        # parameter names
        regCoef <- c("Intercept \u03B1", "Carryover \u03D5", "Spillover \u03B2")
        errors <- c("Variance y", "Covariance", "Variance x")

        # row names
        rowY <- "Regression coefficients y"
        rowX <- "Regression coefficients x"
        rowInnov <- "Innovation parameters"
        rowMeasError <- "Measurement error parameters"

        # names from dynr output
        dynrY <- c("alpha_y", "phi_y", "beta_y")
        dynrX <- c("alpha_x", "phi_x", "beta_x")
        dynrZ <- c("zeta_y", "zeta_xy", "zeta_x")
        dynrE <- c("epsilon_y", "epsilon_xy", "epsilon_x")

        inputVAR <- c(regY, regX, innov)
        dynrNamesVAR <- c(dynrY, dynrX, dynrZ)
        parNamesVAR <- c(regCoef, regCoef, errors)
        rowNamesVAR <- c(rowY, NA, NA, rowX, NA, NA, rowInnov, NA, NA)

        inputLVAR <- c(regY, indY, regX, indX, innov, measError)
        dynrNamesLVAR <- c(dynrY, "nu_y", dynrX, "nu_x", dynrZ, dynrE)
        parNamesLVAR <- c(
          regCoef, "Mean \u03BD", regCoef, "Mean \u03BD",
          errors, errors
        )
        rowNamesLVAR <- c(
          rowY, NA, NA, "Indicator y",
          rowX, NA, NA, "Indicator x", rowInnov, NA, NA, rowMeasError, NA, NA
        )

        return(
          list(
            "VAR" = list(
              params = inputVAR,
              dynrNames = dynrNamesVAR,
              parNames = parNamesVAR,
              rowNames = rowNamesVAR
            ),
            "L" = list(
              params = inputLVAR,
              dynrNames = dynrNamesLVAR,
              parNames = parNamesLVAR,
              rowNames = rowNamesLVAR
            )
          )
        )
      })

      observeEvent(input$estimateModel, {
        # estimates <- 0
        estimates$estimated <- 0 # somehow this is needed to get the spinner to work properly
        # estimates$difference <- 0

        fit <- tryCatch(dynrEstimation(dat(),
          model = input$model,
          dataFormat = dataFormat()
        ),
        error = function(e) message("error"),
        warning = function(w) message("warning")
        )
        summaryFit <- summary(fit)
        est <- summaryFit$Coefficients[mod()[[input$model]]$dynrNames, 1]

        # dif <- est - mod()$params

        # estimates <- est
        estimates$estimated <- est
        # estimates$difference <- dif
      })

      output$trueTable <- DT::renderDataTable({
        # parNames <- c(rep(c("Intercept \u03B1", "Carryover \u03D5", "Spillover \u03B2"), 2),
        #               "Innovation variance y", "Innovation covariance", "Innovation variance x")

        df <- data.frame(
          names = mod()[[trueModel()]]$parNames,
          true = mod()[[trueModel()]]$params
        )

        table <- DT::datatable(df,
          colnames = c("Parameters", "True"),
          rownames = mod()[[trueModel()]]$rowNames,
          options = list(dom = "t", bSort = FALSE, pageLength = 15),
        )

        table <- DT::formatRound(table, columns = 2, digits = 3)
      })

      output$estimatesTable <- DT::renderDataTable({
        # parNames <- c(rep(c("Intercept \u03B1", "Carryover \u03D5", "Spillover \u03B2"), 2),
        #               "Innovation variance y", "Innovation covariance", "Innovation variance x")

        df <- data.frame(
          names = mod()[[input$model]]$parNames,
          # estimated = estimates
          estimated = estimates$estimated
        )

        table <- DT::datatable(df,
          colnames = c("Parameters", "Estimated"),
          rownames = mod()[[input$model]]$rowNames,
          options = list(dom = "t", bSort = FALSE, pageLength = 15),
        )

        table <- DT::formatRound(table, columns = 2, digits = 3)
      })

      observeEvent(params(), {
        # estimates <- NA
        estimates$estimated <- NA
        # estimates$difference <- NA
      })

      observeEvent(input$model, {
        # estimates <- NA
        estimates$estimated <- NA
        # estimates$difference <- NA
      })
    }
  )
}
