plotsInputUI <- function(id){
  ns <- NS(id)

  fluidRow(
    column(2,
           div(style = "margin: 0px -15px 0px -15px;",
      sidebarPanel(width = 12,
                   h4("Plots"),
                   h5("Time-series"),
                   fluidRow(style = "padding-top:5px",
                            column(6,
                                   checkboxInput(ns("showTSy"), "y", value = T)
                            ),
                            column(6,
                                   checkboxInput(ns("showTSx"), "x", value = T)
                            )
                   ),
                   h5("Scatter plot auto"),
                   fluidRow(
                            column(6,
                                   checkboxInput(ns("showCarryoverY"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showCarryoverX"), "x")
                            )
                   ),
                   h5("Scatter plot cross-lagged"),
                   fluidRow(
                            column(6,
                                   checkboxInput(ns("showSpilloverY"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showSpilloverX"), "x")
                            )
                   ),
                   h5("3D state space"),
                   fluidRow(
                            column(6,
                                   checkboxInput(ns("show3Dy"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("show3Dx"), "x")
                            )
                   ),
                   h5("Autocorrelation function"),
                   fluidRow(
                            column(6,
                                   checkboxInput(ns("showACFy"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showACFx"), "x")
                            )
                   ),
                   h5("Cross-correlation function"),
                   fluidRow(
                            column(6,
                                   checkboxInput(ns("showCCFy"), "y * x")
                            ),
                            column(6,
                                   checkboxInput(ns("showCCFx"), "x * y")
                            )
                   )
      )
    )),
    column(10,
      fluidRow(
        conditionalPanel(condition = "input.showTSy | input.showTSx",
                         column(8, h4("Time-series"),
                                withSpinner(plotOutput(ns("ts"), height = "250px"))), ns = ns
        ),
        conditionalPanel(condition = "input.showCarryoverY | input.showCarryoverX",
                         column(4,  h4("Scatter plot auto"),
                                withSpinner(plotOutput(ns("carryover"), height = "250px"))), ns = ns
        ),
        conditionalPanel(condition = "input.showSpilloverY | input.showSpilloverX",
                         column(4, h4("Scatter plot cross-lagged"),
                                withSpinner(plotOutput(ns("spillover"), height = "250px"))), ns = ns
        ),
        conditionalPanel(condition = "input.show3Dy | input.show3Dx", ns = ns,
                         column(8, h4("3D state space"),
                                withSpinner(plotly::plotlyOutput(ns("plotly"), height = "300px")))
        ),
        conditionalPanel(condition = "input.showACFy | input.showACFx",
                         column(4, h4("Autocorrelation function"),
                                withSpinner(plotOutput(ns("acf"), height = "250px"))), ns = ns
        ),       
        conditionalPanel(condition = "input.showCCFy | input.showCCFx",
                         column(4, h4("Cross-correlation function"),
                                withSpinner(plotOutput(ns("ccf"), height = "250px"))), ns = ns
        )
      )
    )
  )
}

plotsServer <- function(id, dataFormat, model, dat, uploaded = F, uploadedFile = NULL){
  moduleServer(
    id,
    function(input, output, session){

      output$ts <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        legend.position <- c(.05, .9)
        if(input$showTSy && input$showTSx) partner <- NULL
          else if(input$showTSy) partner <- "y"
          else if(input$showTSx) partner <- "x"
        if(model() == "T" || model() == "MS" || model() == "HMM"){
          regime <- T
          regimeType <- "points"
          if(is.null(partner) & model() == "T") legend.position <- c(.15, .9)
            else legend.position <- c(.1, .9)
        } else {
          regime <- F
          regimeType <- NULL
        }
        p <- myTS(dat(), partner = partner, regime = regime, regimeType = regimeType,
                  shiny = T, legend.position = legend.position)
        return(p)
      })
      
      output$carryover <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$showCarryoverY) p <- mySSP(dat(), partner = "y", type = "carryover", cor = T, shiny = T)
        if(input$showCarryoverX) p <- mySSP(dat(), partner = "x", type = "carryover", cor = T, shiny = T)
        if(input$showCarryoverY && input$showCarryoverX) p <- mySSP(dat(), type = "carryover", cor = T, shiny = T, legend.position = c(.25, .9))
        return(p)
      })

      output$spillover <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$showSpilloverY) p <- mySSP(dat(), partner = "y", type = "spillover", cor = T, shiny = T)
        if(input$showSpilloverX) p <- mySSP(dat(), partner = "x", type = "spillover", cor = T, shiny = T)
        if(input$showSpilloverY && input$showSpilloverX) p <- mySSP(dat(), type = "spillover", cor = T, shiny = T,
                                                                    legend.position = c(.35, .9))
        return(p)
      })

      output$ccf <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$showCCFy) p <- myCF(dat(), partner = "y", type = "CCF", shiny = T)
        if(input$showCCFx) p <- myCF(dat(), partner = "x", type = "CCF", shiny = T)
        if(input$showCCFy && input$showCCFx) p <- myCF(dat(), type = "CCF", shiny = T)
        return(p)
      })

      output$acf <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$showACFy) p <- myCF(dat(), partner = "y", type = "ACF", shiny = T)
        if(input$showACFx) p <- myCF(dat(), partner = "x", type = "ACF", shiny = T)
        if(input$showACFy && input$showACFx) p <- myCF(dat(), type = "ACF", shiny = T)
        return(p)
      })

      output$plotly <- plotly::renderPlotly({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$show3Dy) p <- my3D(dat(), partner = "y")
        if(input$show3Dx) p <- my3D(dat(), partner = "x")
        if(input$show3Dy && input$show3Dx) p <- my3D(dat())
        return(p)
      })
    }
  )
}


plotstvUI <- function(id){
  ns <- NS(id)
  fluidRow(
   column(2, plotOutput(ns("alpha_y"), height = "150px")),
   column(2, plotOutput(ns("phi_y"), height = "150px")),
   column(2, plotOutput(ns("beta_y"), height = "150px"),
          style = 'border-right:1px solid; border-color:LightGrey;'),
   column(2, plotOutput(ns("alpha_x"), height = "150px")),
   column(2, plotOutput(ns("phi_x"), height = "150px")),
   column(2, plotOutput(ns("beta_x"), height = "150px"))
  )
}

plotstvServer <- function(id, t, tv){
  moduleServer(
    id,
    function(input, output, session){
      output$alpha_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$alpha_y$p(), ylab = bquote(Intercept ~ italic(alpha["y,t"])), shiny = T)
        return(p)
      })
      output$phi_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$phi_y$p(), ylab = bquote(Carryover ~ italic(phi["y,t"])), shiny = T)
        return(p)
      })
      output$beta_y <- renderPlot({
        p <- myTSsimple(1:t(), tv$beta_y$p(), ylab = bquote(Spillover ~ italic(beta["y,t"])), shiny = T)
        return(p)
      })
      
      output$alpha_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$alpha_x$p(), ylab = bquote(Intercept ~ italic(alpha["x,t"])), shiny = T)
        return(p)
      })
      output$phi_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$phi_x$p(), ylab = bquote(Carryover ~ italic(phi["x,t"])), shiny = T)
        return(p)
      })
      output$beta_x <- renderPlot({
        p <- myTSsimple(1:t(), tv$beta_x$p(), ylab = bquote(Spillover ~ italic(beta["x,t"])), shiny = T)
        return(p)
      })
    }
  )
}