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
                                "Markov-Switching VAR(1)" = "MS"), selected = "VAR", width = "95%"),
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
  
  fluidRow(style = "padding-top:5px",
    column(12,
      numericInput(ns("alpha"), "Intercept", 0, width = "50%"),
      sliderInput(ns("phi"), "Carryover", -1, 1, .5, .1),
      sliderInput(ns("beta"), "Spillover", -1, 1, .2, .1)
    )
  )
}

inputVARServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      return(
        list(
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
  
  fluidRow(style = "padding-top:5px",
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
  
  fluidRow(style = "padding-top:5px",
    column(12,
      radioButtons(ns("tv"), "", list("Stable" = "stable", "Time-varying" = "tv"), 
                   selected = "stable", inline = T),
      conditionalPanel(condition = "input.tv == 'stable'",
                       numericInput(ns("stable"), "Value", 0, width = "50%"),
                       ns = ns
      ),
      conditionalPanel(condition = "input.tv == 'tv'",
                       fluidRow(
                         column(3,
                                radioButtons(ns("change"), "", 
                                             list("Linear" = "linear", "Sine" = "sine"), 
                                             selected = "linear")
                         ),
                         conditionalPanel(condition = "input.change == 'linear'",
                                          column(3,
                                                 numericInput(ns("from"), "From", 0),
                                          ),
                                          column(3,
                                                 numericInput(ns("to"), "To", 0)
                                          ),
                                          ns = ns
                         ),
                         conditionalPanel(condition = "input.change == 'sine'",
                                          column(3,
                                                 numericInput(ns("amp"), "Amplitude", 1),
                                                 numericInput(ns("phase"), "Phase", 0),
                                          ),
                                          column(3,
                                                 numericInput(ns("freq"), "Frequency", 1),
                                                 numericInput(ns("dev"), "Deviation", 0)
                                          ),
                                          ns = ns
                         ),
                       ),
                       checkboxInput(ns("plot"), "Plot"), ns = ns
      )
    )
  )
}


tvServer <- function(id, t){
  moduleServer(
    id,
    function(input, output, session){
      tv_par <- reactive({
        if(input$tv == "stable"){
          p <- input$stable
        } else if(input$change == "linear"){
            p <- change_linear(input$from, input$to, t())
        } else if(input$change == "sine"){
            p <- change_sine(amplitude = input$amp, freq = input$freq,
                             phase = input$phase, deviation = input$dev,
                             t = t())
        }
        return(p)
      })
      
      return(
        list(
          p    = tv_par,
          tv   = reactive({ input$tv }),
          plot = reactive({ input$plot })
        )
      )
    }
  )
}