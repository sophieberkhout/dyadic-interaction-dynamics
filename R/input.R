methodUI <- function(id){
  ns <- NS(id)
  
  sidebarPanel(width = 3,
               h4("Method"),
               selectInput(ns("model"), "Data generating model",
                           list("First-order Vector Autoregressive VAR(1)" = "VAR",
                                "Latent VAR(1)" = "L",
                                "Time-Varying VAR(1)" = "TV",
                                "Threshold VAR(1)" = "T",
                                "Hidden Markov Model" = "HMM",
                                "Markov-Switching VAR(1)" = "MS"), selected = 1, width = "95%"),
               numericInput(ns("t"), "Measurement occasions", 300, min = 2, step = 50, width = "60%"), # 1 does not work
               numericInput(ns("burnin"), "Burnin", 20, min = 0, step = 10, width = "60%"),
               numericInput(ns("seed"), "Seed", 1, min = 1, max = .Machine$integer.max, width = "60%")
  )
}

methodServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$seed, {
        if(!is.integer(input$seed)){
          newSeed <- round(input$seed)
          updateNumericInput(session, "seed", value = newSeed)
        }
      })
      
      observeEvent(input$burnin, {
        if(!is.integer(input$burnin)){
          newBurnin <- round(input$burnin)
          updateNumericInput(session, "burnin", value = newBurnin)
        }
      })
      
      observeEvent(input$t, {
        if(!is.integer(input$t)){
          newT <- round(input$t)
          updateNumericInput(session, "t", value = newT)
        }
      })
      
      return(
        list(
          model  = reactive({ input$model }),
          t      = reactive({ input$t }),
          burnin = reactive({ input$burnin }),
          seed   = reactive({ input$seed })
        )
      )
    }
  )
}

# Server <- function(id){
#   moduleServer(
#     id,
#     function(input, output, server){
#       
#     }
#   )
# }


inputVARUI <- function(id){
  ns <- NS(id)
  
  tagList(
    numericInput(ns("alpha"), "Intercept", 0, width = "50%"),
    sliderInput(ns("phi"), "Carryover", -1, 1, .5, .1),
    sliderInput(ns("beta"), "Spillover", -1, 1, .2, .1)
  )
}

inputVARServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      return(
        list(
          # alpha = reactive({ input$alpha }),
          # phi   = reactive({ input$phi }),
          # beta  = reactive({ input$beta })
          alpha = input$alpha,
          phi   = input$phi,
          beta  = input$beta
          
        )
      )
    }
  )
}

errorsUI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(4,
           numericInput(ns("y"), "Variance y", .1, 0, 1, .05),
           numericInput(ns("yx"), "Correlation", .3, -1, 1, .1)
    ),
    column(4,
           numericInput(ns("x"), "Variance x", .1, 0, 1, .05)
    )
  )
}

errorsServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      cov_yx <- input$yx * sqrt(input$y) * sqrt(input$x)
      return(
        c(input$y, cov_yx, input$x)
      )
    }
  )
}

tvUI <- function(id){
  ns <- NS(id)
  
  tagList(
    radioButtons("tv", "", list("Stable", "Time-varying"), inline = T),
    conditionalPanel(condition = "input.tv == 'Stable'",
                     numericInput("stable", "Value", 0, width = "50%")
    ),
    conditionalPanel(condition = "input.tv == 'Time-varying'",
                     fluidRow(
                       column(3,
                              radioButtons("change", "", list("Linear", "Sine"))
                       ),
                       conditionalPanel(condition = "input.change == 'Linear'",
                                        column(3,
                                               numericInput("from", "From", 0),
                                        ),
                                        column(3,
                                               numericInput("to", "To", 0)
                                        )
                       ),
                       conditionalPanel(condition = "input.change == 'Sine'",
                                        column(3,
                                               numericInput("amp", "Amplitude", 1),
                                               numericInput("phase", "Phase", 0),
                                        ),
                                        column(3,
                                               numericInput("freq", "Frequency", 1),
                                               numericInput("dev", "Deviation", 0)
                                        )
                       ),
                     ),
                     checkboxInput("plot", "Plot intercept")
    )
  )
}


tvServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      return(
        list(
          tv       = reactive({ input$tv }),
          change   = reactive({ input$change }),
          beta     = reactive({ input$beta }),
          from     = reactive({ input$from }),
          to       = reactive({ input$to }),
          amp      = reactive({ input$amp }),
          phase    = reactive({ input$phase }),
          freq     = reactive({ input$freq }),
          dev      = reactive({ input$dev }),
          plot     = reactive({ input$plot })
        )
      )
    }
  )
}