paramsUI <- function(id) {
  ns <- NS(id)

  tabsetPanel(id = ns("coefTabs"),
        tabPanel("Regression coefficients",
                inputVARUI(ns("parameters"))
        ),
        tabPanel(HTML("Intercept &#120572;"),
                tvUI(ns("tvIntercept"), type = "num"),
        ),
        tabPanel(HTML("Carryover &#120601;"),
                tvUI(ns("tvCarryover")),
        ),
        tabPanel(HTML("Spillover &#120573;"),
                tvUI(ns("tvSpillover")),
        ),
        tabPanel("Means",
                meansUI(ns("hmmMeans"))
        ),
        tabPanel("Indicator",
                indicatorUI(ns("indicator"))
        ),
        tabPanel("Regime 1",
                inputVARUI(ns("firstRegime"))
        ),
        tabPanel("Regime 2",
                inputVARUI(ns("secondRegime"))
        )
  )
}

paramsServer <- function(id, model, t, tau, paramsOther, partner) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(model(),
        {
          hideTab("coefTabs", target = "Regime 1")
          hideTab("coefTabs", target = "Regime 2")
          hideTab("coefTabs", target = HTML("Intercept &#120572;"))
          hideTab("coefTabs", target = HTML("Carryover &#120601;"))
          hideTab("coefTabs", target = HTML("Spillover &#120573;"))
          hideTab("coefTabs", target = "Means")
          hideTab("coefTabs", target = "Indicator")
          hideTab("coefTabs", target = "Regression coefficients")

          if (model() == "VAR" || model() == "L") {
            showTab("coefTabs", "Regression coefficients", select = TRUE)
          }

          if (model() == "TV") {
            showTab("coefTabs", HTML("Intercept &#120572;"), select = TRUE)
            showTab("coefTabs", HTML("Carryover &#120601;"))
            showTab("coefTabs", HTML("Spillover &#120573;"))
          }

          if (model() == "HMM") {
            showTab("coefTabs", "Means", select = TRUE)
          }

          if (model() == "L") {
            showTab("coefTabs", "Indicator")
          }

          if (model() == "T" || model() == "MS") {
            showTab("coefTabs", "Regime 1")
            showTab("coefTabs", "Regime 2")
          }
        },
        priority = 1
      )

      tv_alpha <- tvServer("tvIntercept", reactive({ t() }))
      tv_phi <- tvServer("tvCarryover", reactive({ t() }))
      tv_beta <- tvServer("tvSpillover", reactive({ t() }))
      
      tvCoefs <- list()
      tvCoefs$alpha <- tv_alpha$p
      tvCoefs$phi <- tv_phi$p
      tvCoefs$beta <- tv_beta$p

      means <- meansServer("hmmMeans")

      indicator <- indicatorServer("indicator")

      coefs <- inputVARServer("parameters",
        params = paramsOther,
        model = model, nu = indicator, partner = partner
      )

      coefs_1 <- inputVARServer("firstRegime")
      coefs_2 <- inputVARServer("secondRegime")

      params <- reactive({
        p <- list(
          alpha = coefs$alpha(),
          phi = coefs$phi(),
          beta = coefs$beta()
        )

        if (model() == "T" || model() == "MS") {
          p <- list(
            alpha = coefs_1$alpha(),
            phi = coefs_1$phi(),
            beta = coefs_1$beta()
          )

          p$alpha[2] <- coefs_2$alpha()
          p$phi[2] <- coefs_2$phi()
          p$beta[2] <- coefs_2$beta()
          if (model() == "T") p$tau <- tau()

        }
        if (model() == "HMM") {
          p <- list(mu = c(means$mu_1(), means$mu_2()))
        }
        if (model() == "TV") {
          p <- list(
            alpha = tvCoefs$alpha(), phi = tvCoefs$phi(), beta = tvCoefs$beta()
          )
        }

        indicatorValues <- NULL
        if (model() == "L") {
          indicatorValues <- list(m = indicator$mean(), l = 1)
        }

        return(
          list(
            coefs = p,
            indicator = indicatorValues,
            tv = list(alpha = tv_alpha, phi = tv_phi, beta = tv_beta)
          )
        )
      })

      return(params)
    }
  )
}

errorsUI <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("errors"),
    tabPanel(
      "Innovation parameters",
      inputErrorsUI(ns("innovations"))
    ),
    tabPanel(
      "Measurement error parameters",
      inputErrorsUI(ns("measurementError"))
    ),
    tabPanel(
      "Measurement error regime 1",
      inputErrorsUI(ns("measurementErrorFirstRegime"))
    ),
    tabPanel(
      "Measurement error regime 2",
      inputErrorsUI(ns("measurementErrorSecondRegime"))
    ),
    tabPanel(
      "Innovation regime 1",
      inputErrorsUI(ns("innovationsFirstRegime"))
    ),
    tabPanel(
      "Innovation regime 2",
      inputErrorsUI(ns("innovationsSecondRegime"))
    )
  )
}

errorsServer <- function(id, model) {
  moduleServer(
    id,
    function(input, output, session) {

    observeEvent(model(),
      {
        hideTab("errors", target = "Innovation parameters")
        hideTab("errors", target = "Measurement error parameters")
        hideTab("errors", target = "Innovation regime 1")
        hideTab("errors", target = "Measurement error regime 1")
        hideTab("errors", target = "Measurement error regime 2")
        hideTab("errors", target = "Innovation regime 2")

        if (model() == "VAR" || model() == "L" || model() == "TV") {
          showTab("errors", "Innovation parameters", select = TRUE)
        }

        if (model() == "HMM") {
          showTab("errors", "Measurement error regime 1", select = TRUE)
          showTab("errors", "Measurement error regime 2")
        }

        if (model() == "L") {
          showTab("errors", "Measurement error parameters", select = FALSE)
        }

        if (model() == "T" || model() == "MS") {
          showTab("errors", "Innovation regime 1", select = TRUE)
          showTab("errors", "Innovation regime 2")
        }
      },
      priority = 1
    )

    innovations <- inputErrorsServer("innovations")
    innovations_1 <- inputErrorsServer("innovationsFirstRegime")
    innovations_2 <- inputErrorsServer("innovationsSecondRegime")
    measurement_errors <- inputErrorsServer("measurementError")
    measurement_errors_1 <- inputErrorsServer("measurementErrorFirstRegime")
    measurement_errors_2 <- inputErrorsServer("measurementErrorSecondRegime")

    errors <- reactive({
      innovations <- c(innovations$y(), innovations$c_yx(), innovations$x())

      if (model() == "T" || model() == "MS") {
        innovations_1 <- c(
          innovations_1$y(), innovations_1$c_yx(), innovations_1$x()
        )
        innovations_2 <- c(
          innovations_2$y(), innovations_2$c_yx(), innovations_2$x()
        )

        innovations <- list(
          firstRegime = innovations_1,
          secondRegime = innovations_2
        )
      }
      if (model() == "HMM") {
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

      if (model() == "L") {
        measurement_errors <- c(
          measurement_errors$y(), measurement_errors$c_yx(),
          measurement_errors$x()
        )
      }

      if (model() != "L" & model() != "HMM") {
        measurement_errors <- NULL
      }

      return(
        list(
          dynamic = innovations,
          measurement = measurement_errors
        )
      )
    })

    return(errors)
    }
  )
}