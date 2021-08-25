methodUI <- function(id){
  ns <- NS(id)
  
  sidebarPanel(width = 3,
               h4("Method"),
               selectInput("model", "Data generating model", 
                           list("First-order Vector Autoregressive VAR(1)" = "VAR", 
                                "Latent VAR(1)" = "L", 
                                "Time-Varying VAR(1)" = "TV", 
                                "Threshold VAR(1)" = "T", 
                                "Hidden Markov Model" = "HMM", 
                                "Markov-Switching VAR(1)" = "MS"), selected = 1, width = "95%"),
               numericInput("t", "Measurement occasions", 300, min = 2, step = 50, width = "60%"), # 1 does not work
               numericInput("burnin", "Burnin", 20, min = 0, step = 10, width = "60%"),
               numericInput("seed", "Seed", 1, min = 1, max = .Machine$integer.max, width = "60%")
  )
}


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
    function(input, output, server){
      return(
        list(
          alpha = input$alpha,
          phi = input$phi,
          beta = input$beta
        )
      )
    }
  )
}

errorsUI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(6,
           numericInput(ns("y"), "Variance y", .1, 0, 1, .05),
           numericInput(ns("yx"), "Covariance", .05, 0, 1, .05)
    ),
    column(6,
           numericInput(ns("x"), "Variance x", .1, 0, 1, .05)
    )
  )
}

errorsServer <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      return(
        c(input$y, input$yx, input$yx, input$x)
      )
    }
  )
}
