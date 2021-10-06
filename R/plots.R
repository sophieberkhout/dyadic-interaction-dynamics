plotsInputUI <- function(id){
  ns <- NS(id)

  fluidRow(
    sidebarPanel(width = 3,
                 h4("Plots"),
                 fluidRow(column(6, h5("Time series")),
                          column(3,
                                 checkboxInput(ns("showTSy"), "y", value = T)
                          ),
                          column(3,
                                 checkboxInput(ns("showTSx"), "x", value = T)
                          )
                 ),
                 fluidRow(column(6, h5("Carryover")),
                          column(3,
                                 checkboxInput(ns("showCarryoverY"), "y")
                          ),
                          column(3,
                                 checkboxInput(ns("showCarryoverX"), "x")
                          )
                 ),
                 fluidRow(column(6, h5("Spillover")),
                          column(3,
                                 checkboxInput(ns("showSpilloverY"), "y")
                          ),
                          column(3,
                                 checkboxInput(ns("showSpilloverX"), "x")
                          )
                 ),
                 fluidRow(column(6, h5("3D state space")),
                          column(3,
                                 checkboxInput(ns("show3Dy"), "y")
                          ),
                          column(3,
                                 checkboxInput(ns("show3Dx"), "x")
                          )
                 ),
                 fluidRow(column(6, h5("Autocorrelation function")),
                          column(3,
                                 checkboxInput(ns("showACFy"), "y")
                          ),
                          column(3,
                                 checkboxInput(ns("showACFx"), "x")
                          )
                 ),
                 fluidRow(column(6, h5("Cross-correlation function")),
                          column(3,
                                 checkboxInput(ns("showCCFy"), "(y * x)")
                          ),
                          column(3,
                                 checkboxInput(ns("showCCFx"), "(x * y)")
                          )
                 )
    ),
    conditionalPanel(condition = "input.showTSy | input.showTSx",
                     column(8, plotOutput(ns("ts"))), ns = ns
    ),
    # column(8, plotOutput(ns("ts"))),
    conditionalPanel(condition = "input.showCarryoverY | input.showCarryoverX",
                     column(4, plotOutput(ns("carryover"))), ns = ns
    ),
    conditionalPanel(condition = "input.showSpilloverY | input.showSpilloverX",
                     column(4, plotOutput(ns("spillover"))), ns = ns
    ),
    conditionalPanel(condition = "input.showACFy | input.showACFx",
                     column(4, plotOutput(ns("acf"))), ns = ns
    ),       
    conditionalPanel(condition = "input.showCCFy | input.showCCFx",
                     column(4, plotOutput(ns("ccf"))), ns = ns
    ),
    conditionalPanel(condition = "input.show3Dy | input.show3Dx", ns = ns,
                     column(11, plotly::plotlyOutput(ns("plotly"), height = 600))
    ),
    conditionalPanel(condition = "input['plot']",
                     column(4, plotOutput(ns("alpha"))),
                     ns = NS("intercept_y")
    )
  )
}

plotsServer <- function(id, dataFormat, method, dat, intercept_y){
  moduleServer(
    id,
    function(input, output, session){
      output$ts <- renderPlot({
        req(dataFormat() == "long")
        if(method() == "T"){
          regime <- T
          regimeType <- "points"
        } else {
          regime <- F
          regimeType <- NULL
        }
        if(input$showTSy) p <- myTS(dat(), partner = "y", regime = regime, shiny = T)
        if(input$showTSx) p <- myTS(dat(), partner = "x", regime = regime, shiny = T)
        if(input$showTSy && input$showTSx) 
          p <- myTS(dat(), regime = regime, regimeType = regimeType, shiny = T)
        return(p)
      })
      
      output$carryover <- renderPlot({
        req(dataFormat() == "long")
        if(input$showCarryoverY) p <- mySSP(dat(), partner = "y", type = "carryover", shiny = T)
        if(input$showCarryoverX) p <- mySSP(dat(), partner = "x", type = "carryover", shiny = T)
        if(input$showCarryoverY && input$showCarryoverX) p <- mySSP(dat(), type = "carryover", shiny = T)
        return(p)
      })

      output$spillover <- renderPlot({
        req(dataFormat() == "long")
        if(input$showSpilloverY) p <- mySSP(dat(), partner = "y", type = "spillover", shiny = T)
        if(input$showSpilloverX) p <- mySSP(dat(), partner = "x", type = "spillover", shiny = T)
        if(input$showSpilloverY && input$showSpilloverX) p <- mySSP(dat(), type = "spillover", shiny = T)
        return(p)
      })

      output$ccf <- renderPlot({
        req(dataFormat() == "long")
        if(input$showCCFy) p <- myCF(dat(), partner = "y", type = "CCF", shiny = T)
        if(input$showCCFx) p <- myCF(dat(), partner = "x", type = "CCF", shiny = T)
        if(input$showCCFy && input$showCCFx) p <- myCF(dat(), type = "CCF", shiny = T)
        return(p)
      })

      output$acf <- renderPlot({
        req(dataFormat() == "long")
        if(input$showACFy) p <- myCF(dat(), partner = "y", type = "ACF", shiny = T)
        if(input$showACFx) p <- myCF(dat(), partner = "x", type = "ACF", shiny = T)
        if(input$showACFy && input$showACFx) p <- myCF(dat(), type = "ACF", shiny = T)
        return(p)
      })

      output$plotly <- plotly::renderPlotly({
        req(dataFormat() == "long")
        if(input$show3Dy) p <- my3D(dat(), partner = "y")
        if(input$show3Dx) p <- my3D(dat(), partner = "x")
        if(input$show3Dy && input$show3Dx) p <- my3D(dat())
        return(p)
      })

      tv_alpha <- reactive({
        if(intercept_y()$tv == "Stable"){
          a <- intercept_y()$stable
        } else {
          if(intercept_y()$change == "Linear"){
            a <- change_linear(intercept_y()$from, intercept_y()$to, method()$t)
          } else if(input$change == "Sine"){
            a <- change_sine(amplitude = intercept_y()$amp, freq = intercept_y()$freq,
                             phase = intercept_y()$phase, deviation = intercept_y()$dev,
                             t = method()$t)
          }
        }
        return(a)
      })

      output$alpha <- renderPlot({
        if(intercept_y()$tv == "Time-varying"){
          myTSsimple(1:method()$t, tv_alpha(), ylab = "Intercept", shiny = T)
        }
      })
    }
  )
}