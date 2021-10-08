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
    conditionalPanel(condition = "input['intercept_y-plot'] & input['intercept_y-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("alpha_y")))
    ),
    conditionalPanel(condition = "input['carryover_y-plot'] & input['carryover_y-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("phi_y")))
    ),
    conditionalPanel(condition = "input['spillover_y-plot'] & input['spillover_y-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("beta_y")))
                     
    ),
    conditionalPanel(condition = "input['intercept_x-plot'] & input['intercept_x-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("alpha_x")))
                     
    ),
    conditionalPanel(condition = "input['carryover_x-plot'] & input['carryover_x-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("phi_x")))
                     
    ),
    conditionalPanel(condition = "input['spillover_x-plot'] & input['spillover_x-tv'] == 'tv' & input['method-model'] == 'TV'",
                     column(4, plotOutput(ns("beta_x")))
                     
    )
  )
}

plotsServer <- function(id, dataFormat, model, t, dat, tv){
  moduleServer(
    id,
    function(input, output, session){
      output$ts <- renderPlot({
        req(dataFormat() == "long")
        if(model() == "T" || model() == "MS" || model() == "HMM"){
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

      output$alpha_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$alpha_y$p(), ylab = "Intercept y", shiny = T)
        return(p)
      })
      output$phi_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$phi_y$p(), ylab = "Carryover y", shiny = T)
        return(p)
      })
      output$beta_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$beta_y$p(), ylab = "Spillover y", shiny = T)
        return(p)
      })
      
      output$alpha_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$alpha_x$p(), ylab = "Intercept x", shiny = T)
        return(p)
      })
      output$phi_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$phi_x$p(), ylab = "Carryover x", shiny = T)
        return(p)
      })
      output$beta_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$beta_x$p(), ylab = "Spillover x", shiny = T)
        return(p)
      })
    }
  )
}