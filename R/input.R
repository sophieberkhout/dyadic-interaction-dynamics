methodUI <- function(id) {
  ns <- NS(id)

  wellPanel(
    width = 12,
    # h4("Method"),
    fluidRow(
      column(
        4,
        selectInput(ns("model"), "Data generating model",
          list(
            "First-order vector autoregressive VAR(1)" = "VAR",
            "Latent VAR(1)" = "L",
            "Time-varying VAR(1)" = "TV",
            "Threshold VAR(1)" = "T",
            "Hidden Markov model" = "HMM",
            "Markov-switching VAR(1)" = "MS"
          ),
          selected = "VAR"
        )
      ),
      column(
        3,
        numericInput(ns("t"), "Measurement occasions", 300, min = 2, step = 50) # 1 does not work
      ),
      column(
        3,
        numericInput(ns("seed"), "Seed", 1, min = 1, max = .Machine$integer.max)
      ),
      column(
        2,
        actionButton(ns("randomSeed"), "Random Seed")
      )
    )
  )
}

methodServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$seed, {
        if (!is.integer(input$seed)) {
          newSeed <- round(input$seed)
          updateNumericInput(session, "seed", value = newSeed)
        }
      })

      observeEvent(input$randomSeed, {
        newSeed <- sample.int(1e5, 1)
        updateNumericInput(session, "seed", value = newSeed)
      })

      observeEvent(input$t, {
        if (!is.integer(input$t)) {
          newT <- round(input$t)
          updateNumericInput(session, "t", value = newT)
        }
      })

      return(
        list(
          model = reactive({
            input$model
          }),
          t = reactive({
            input$t
          }),
          seed = reactive({
            input$seed
          })
        )
      )
    }
  )
}

inputVARUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "padding-top:5px",
      column(
        6,
        numericInput(ns("alpha"), HTML("Intercept &#120572;"), 0, width = "60%")
      ),
      column(
        6,
        conditionalPanel(
          condition = "input.model == 'VAR' || input.model == 'L'",
          strong(uiOutput(ns("meanTextVAR"))), ns = NS("method"),
          textOutput(ns("meanVAR"))
        )
      )
    ),
    sliderInput(ns("phi"), HTML("Carryover &#120601;"), -1, 1, .5, .1),
    sliderInput(ns("beta"), HTML("Spillover &#120573;"), -1, 1, .2, .1)
  )
}

inputVARServer <- function(id, params = NULL, model = NULL, nu = NULL, 
partner = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      output$meanVAR <- renderText({
        num <- input$alpha * (1 - params$phi()) + input$beta * params$alpha()
        den <- (1 - input$phi) * (1 - params$phi()) - params$beta() * input$beta
        m <- num / den

        if (model() == "L") m <- m + nu$mean()

        m <- round(m, 2)
        as.character(m)
      })

      output$meanTextVAR <- renderUI({
        text <- sprintf("Mean $%s_t$", partner)
        withMathJax(text)
      })

      return(
        list(
          alpha = reactive({
            input$alpha
          }),
          phi = reactive({
            input$phi
          }),
          beta = reactive({
            input$beta
          })
        )
      )
    }
  )
}

errorsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    style = "padding-top:5px",
    column(
      6,
      numericInput(ns("y"), "Variance y", .1, 0, 1, .05, width = "60%"),
      conditionalPanel(
        condition = "input.model != 'T'",
        numericInput(ns("yx"), "Correlation", .3, -1, 1, .1, width = "60%"),
        ns = NS("method")
      )
    ),
    column(
      6,
      numericInput(ns("x"), "Variance x", .1, 0, 1, .05, width = "60%")
    )
  )
}

errorsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      cov_yx <- reactive({
        validate(
          need(input$yx, "Loading..."),
          need(input$y, " "),
          need(input$x, " ")
        )

        c_yx <- input$yx * sqrt(input$y) * sqrt(input$x)
        return(c_yx)
      })
      return(
        list(
          y = reactive({
            input$y
          }),
          x = reactive({
            input$x
          }),
          c_yx = cov_yx
        )
      )
    }
  )
}

tvUI <- function(id, type = "slider") {
  ns <- NS(id)

  if (type == "num") {
    stablePanel <- conditionalPanel(
      condition = "input.tv == 'stable'",
      numericInput(ns("stable"), "Value", 0, width = "30%"),
      ns = ns
    )
    step <- 1
    def_amp <- 1
  } else {
    stablePanel <- conditionalPanel(
      condition = "input.tv == 'stable'",
      sliderInput(ns("stable"), "Value", -1, 1, .2, .1),
      ns = ns
    )
    step <- 0.1
    def_amp <- 0.3
  }

  fluidRow(
    style = "padding-top:5px",
    column(
      12,
      radioButtons(ns("tv"), "", list("Stable" = "stable", "Time-varying" = "tv"),
        selected = "stable", inline = T
      ),
      stablePanel,
      conditionalPanel(
        condition = "input.tv == 'tv'",
        fluidRow(
          column(
            3,
            radioButtons(ns("change"), "",
              list("Linear" = "linear", "Sine" = "sine"),
              selected = "linear"
            )
          ),
          conditionalPanel(
            condition = "input.change == 'linear'",
            column(
              4,
              numericInput(ns("int"), "Intercept", 0, step = 0.1),
            ),
            column(
              4,
              numericInput(ns("slope"), "Slope", 0, step = 0.001)
            ),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.change == 'sine'",
            column(
              4,
              numericInput(ns("amp"), "Amplitude", def_amp, step = step),
              numericInput(ns("phase"), "Phase", 0),
            ),
            column(
              4,
              numericInput(ns("freq"), "Frequency", 1),
              numericInput(ns("dev"), "Deviation", 0, step = step)
            ),
            ns = ns
          ),
        ), ns = ns
      )
    )
  )
}


tvServer <- function(id, t) {
  moduleServer(
    id,
    function(input, output, session) {
      tv_par <- reactive({
        validate(
          need(input$tv, "Loading...")
        )

        if (input$tv == "stable") {
          p <- input$stable
        } else if (input$change == "linear") {
          p <- change_linear(input$int, input$slope, t())
        } else if (input$change == "sine") {
          p <- change_sine(
            amplitude = input$amp, freq = input$freq,
            phase = input$phase, deviation = input$dev,
            t = t()
          )
        }
        return(p)
      })

      return(
        list(
          p = tv_par,
          tv = reactive({
            input$tv
          }),
          plot = reactive({
            input$plot
          }),
          stable = reactive({
            input$stable
          }),
          change = reactive({
            input$change
          }),
          int = reactive({
            input$int
          }),
          slope = reactive({
            input$slope
          }),
          amp = reactive({
            input$amp
          }),
          freq = reactive({
            input$freq
          }),
          phase = reactive({
            input$phase
          }),
          dev = reactive({
            input$dev
          }),
          t = t
        )
      )
    }
  )
}

meansUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    style = "padding-top:5px",
    column(
      12,
      numericInput(ns("mean_1"), "First regime", 0, width = "30%"),
      numericInput(ns("mean_2"), "Second regime", 0, width = "30%")
    )
  )
}

meansServer <- function(id) {
  moduleServer(
    id,
    function(input, output, server) {
      return(
        list(
          mu_1 = reactive({
            input$mean_1
          }),
          mu_2 = reactive({
            input$mean_2
          })
        )
      )
    }
  )
}

indicatorUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    style = "padding-top:5px",
    column(
      12,
      numericInput(ns("mean"), HTML("Intercept &#120584;"), 0, width = "30%")
    )
  )
}

indicatorServer <- function(id) {
  moduleServer(
    id,
    function(input, output, server) {
      return(
        list(
          mean = reactive({
            input$mean
          })
        )
      )
    }
  )
}
