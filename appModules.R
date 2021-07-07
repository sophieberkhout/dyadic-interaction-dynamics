inputUI_VAR <- function(id){
  ns <- NS(id)
  
  fluidRow(
    box(width = 12, collapsible = T,
        title = "Input",
        fluidRow(
          column(4,
                 h4("Method"),
                 sliderInput(ns("t"), "Measurement occasions", 1, 500, 250), # 1 does not work
                 sliderInput(ns("burnin"), "Burnin", 1, 100, 20),
                 numericInput(ns("seed"), "Seed", 1, 1)
          ),
          column(4,
                 h4("y"),
                 sliderInput(ns("alpha_y"), "Intercept", -1, 1, 0, .1),
                 sliderInput(ns("phi_y"), "Carryover", -1, 1, .5, .1),
                 sliderInput(ns("beta_y"), "Spillover", -1, 1, .2, .1)
          ),
          column(4,
                 h4("x"),
                 sliderInput(ns("alpha_x"), "Intercept", -1, 1, 0, .1),
                 sliderInput(ns("phi_x"), "Carryover", -1, 1, .5, .1),
                 sliderInput(ns("beta_x"), "Spillover", -1, 1, .2, .1)
          )
        )
    )
  )
  
}

plotsUI_VAR <- function(id){
  ns <- NS(id)
  
  tabItem(  
    tabName = "VAR",
    h2("First-order vector autoregression"),
    inputUI_VAR(ns("input_VAR")),
    fluidRow(
      box(title = "Time series", plotOutput(ns("ts")), width = 12)
    ),
    fluidRow(
      box(title = "Carryover y state space", plotOutput(ns("carryover_y"))),
      box(title = "Carryover x state space", plotOutput(ns("carryover_x")))
    ),
    fluidRow(
      box(title = "Spillover y state space", plotOutput(ns("spillover_y"))),
      box(title = "Spillover x state space", plotOutput(ns("spillover_x")))
    ),
    fluidRow(
      box(title = "Cross-correlation function", plotOutput(ns("ccf")))
    )
    
  )
}

inputServer <- function(id){
  moduleServer(id,
               function(input, output, session){
                 dat <- reactive({
                      dat <- simVARS(occasions = input$t, burnin = input$burnin,
                      type = "VAR",
                      params_y = c(input$alpha_y, input$phi_y, input$beta_y),
                      params_x = c(input$alpha_x, input$phi_x, input$beta_x),
                      seed = input$seed)
                   
                      dat
                  })
               })
}

plotsServer <- function(id){
  moduleServer(id, function(input, output, session){
    dat <- inputServer("input_VAR")
    
    output$ts <- renderPlot({
      myTS(dat())
    })
    
    output$carryover_y <- renderPlot({
      mySSP(dat(), type = "carryover", partner = "y")
    })
    
    output$carryover_x <- renderPlot({
      mySSP(dat(), type = "carryover", partner = "x")
    })
    
    output$spillover_y <- renderPlot({
      mySSP(dat(), type = "spillover", partner = "y")
    })
    
    output$spillover_x <- renderPlot({
      mySSP(dat(), type = "spillover", partner = "x")
    })
    
    output$ccf <- renderPlot({
      myCCF(dat())
    })
  }
  )
}
