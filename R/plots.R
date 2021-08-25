plotsInputUI <- function(id){
  ns <- NS(id)
  
  sidebarPanel(width = 3,
               h4("Plots"),
               fluidRow(column(6, h5("Time series")),
                        column(3,
                               checkboxInput("showTSy", "y", value = T)
                        ),
                        column(3,
                               checkboxInput("showTSx", "x", value = T)
                        )
               ),
               fluidRow(column(6, h5("Carryover")),
                        column(3,
                               checkboxInput("showCarryoverY", "y")
                        ),
                        column(3,
                               checkboxInput("showCarryoverX", "x")
                        )
               ),
               fluidRow(column(6, h5("Spillover")),
                        column(3,
                               checkboxInput("showSpilloverY", "y")
                        ),
                        column(3,
                               checkboxInput("showSpilloverX", "x")
                        )
               ),
               fluidRow(column(6, h5("3D state space")),
                        column(3,
                               checkboxInput("show3Dy", "y")
                        ),
                        column(3,
                               checkboxInput("show3Dx", "x")
                        )
               ),
               fluidRow(column(6, h5("Autocorrelation function")),
                        column(3,
                               checkboxInput("showACFy", "y")
                        ),
                        column(3,
                               checkboxInput("showACFx", "x")
                        )
               ),
               fluidRow(column(6, h5("Cross-correlation function")),
                        column(3,
                               checkboxInput("showCCFy", "(y * x)")
                        ),
                        column(3,
                               checkboxInput("showCCFx", "(x * y)")
                        )
               )
  )
}

plotsOutputUI <- function(id){
  ns <- NS(id)
  
  tagList(
    conditionalPanel(condition = "input.showTSy | input.showTSx",
                     column(8, plotOutput("ts"))
    ),
    conditionalPanel(condition = "input.showCarryoverY | input.showCarryoverX",
                     column(4, plotOutput("carryover"))
    ),
    conditionalPanel(condition = "input.showSpilloverY | input.showSpilloverX",
                     column(4, plotOutput("spillover"))
    ),
    conditionalPanel(condition = "input.showACFy | input.showACFx",
                     column(4, plotOutput("acf"))
    ),       
    conditionalPanel(condition = "input.showCCFy | input.showCCFx",
                     column(4, plotOutput("ccf"))
    ),
    conditionalPanel(condition = "input.show3Dy | input.show3Dx",
                     column(11, plotly::plotlyOutput("plotly", height = 600))
    ),
    conditionalPanel(condition = "input.plot_alpha",
                     column(4, plotOutput("alpha"))
    )
  )
}

plotsServer <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      output$ts <- renderPlot({ 
        req(input$dataFormat == "long")
        ifelse(input$model == "T", regime <- T, regime <- F)
        if(input$showTSy) p <- myTS(dat(), partner = "y", regime = regime, shiny = T)
        if(input$showTSx) p <- myTS(dat(), partner = "x", regime = regime, shiny = T)
        if(input$showTSy && input$showTSx) p <- myTS(dat(), shiny = T)
        return(p)
      })
      
      output$carryover <- renderPlot({ 
        req(input$dataFormat == "long")
        if(input$showCarryoverY) p <- mySSP(dat(), partner = "y", type = "carryover", shiny = T)
        if(input$showCarryoverX) p <- mySSP(dat(), partner = "x", type = "carryover", shiny = T)
        if(input$showCarryoverY && input$showCarryoverX) p <- mySSP(dat(), type = "carryover", shiny = T)
        return(p)
      })
      
      output$spillover <- renderPlot({ 
        req(input$dataFormat == "long")
        if(input$showSpilloverY) p <- mySSP(dat(), partner = "y", type = "spillover", shiny = T)
        if(input$showSpilloverX) p <- mySSP(dat(), partner = "x", type = "spillover", shiny = T)
        if(input$showSpilloverY && input$showSpilloverX) p <- mySSP(dat(), type = "spillover", shiny = T)
        return(p)
      })
      
      output$ccf <- renderPlot({
        req(input$dataFormat == "long")
        if(input$showCCFy) p <- myCF(dat(), partner = "y", type = "CCF", shiny = T)
        if(input$showCCFx) p <- myCF(dat(), partner = "x", type = "CCF", shiny = T)
        if(input$showCCFy && input$showCCFx) p <- myCF(dat(), type = "CCF", shiny = T)
        return(p)
      })
      
      output$acf <- renderPlot({ 
        req(input$dataFormat == "long")
        if(input$showACFy) p <- myCF(dat(), partner = "y", type = "ACF", shiny = T)
        if(input$showACFx) p <- myCF(dat(), partner = "x", type = "ACF", shiny = T)
        if(input$showACFy && input$showACFx) p <- myCF(dat(), type = "ACF", shiny = T)
        return(p)
      })
      
      output$plotly <- plotly::renderPlotly({ 
        req(input$dataFormat == "long")
        if(input$show3Dy) p <- my3D(dat(), partner = "y")
        if(input$show3Dx) p <- my3D(dat(), partner = "x")
        if(input$show3Dy && input$show3Dx) p <- my3D(dat())
        return(p)
      })
      
      tv_alpha <- reactive({
        if(input$alpha_change == "Linear"){
          a <- change_linear(input$alpha_from, input$alpha_to, input$t, input$burnin)
        } else if(input$alpha_change == "Sine"){
          a <- change_sine(amplitude = input$alpha_amp, freq = input$alpha_freq,
                           phase = input$alpha_phase, deviation = input$alpha_dev,
                           t = input$t)
        }
        return(a)
      })
      
      output$alpha <- renderPlot({
        plot(tv_alpha(), type = "l")
      })
    }
  )
}