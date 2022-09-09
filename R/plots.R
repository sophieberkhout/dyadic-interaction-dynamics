plotsInputUI <- function(id){
  ns <- NS(id)

  fluidRow(
    column(2,
      wellPanel(width = 12,
                   h4("Plots"),
                   h5("Time-series"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showTSy"), "y", value = T)
                            ),
                            column(6,
                                   checkboxInput(ns("showTSx"), "x", value = T)
                            )
                   ),
                   h5("Distribution"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showDistributionY"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showDistributionX"), "x")
                            )
                   ),
                   h5("Scatter plot auto"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showCarryoverY"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showCarryoverX"), "x")
                            )
                   ),
                   h5("Scatter plot cross-lagged"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showSpilloverY"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showSpilloverX"), "x")
                            )
                   ),
                   h5("3D state space"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("show3Dy"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("show3Dx"), "x")
                            )
                   ),
                   h5("Autocorrelation function"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showACFy"), "y")
                            ),
                            column(6,
                                   checkboxInput(ns("showACFx"), "x")
                            )
                   ),
                   h5("Cross-correlation function"),
                   fluidRow(style = "margin-top:-1em; margin-bottom:-1em",
                            column(6,
                                   checkboxInput(ns("showCCFy"), "y * x")
                            ),
                            column(6,
                                   checkboxInput(ns("showCCFx"), "x * y")
                            )
                   )
    )),
    column(10,
      fluidRow(
        conditionalPanel(condition = "input.showTSy | input.showTSx",
                         column(8, h4("Time-series"),
                                withSpinner(plotOutput(ns("ts"), height = "250px"))), ns = ns
        ),
        conditionalPanel(condition = "input.showDistributionY | input.showDistributionX",
                         column(4,  h4("Distribution"),
                                withSpinner(plotOutput(ns("distribution"), height = "250px"))), ns = ns
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

plotsServer <- function(id, dataFormat, model, dat, uploaded = F, uploadedFile = NULL, tau){
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

      output$distribution <- renderPlot({
        if(!uploaded) req(dataFormat() == "long")
        if(uploaded) req(uploadedFile())
        if(input$showDistributionY)
          p <- myDistribution(dat(), partner = "y", shiny = T)
        if(input$showDistributionX)
          p <- myDistribution(dat(), partner = "x", shiny = T)
        if(input$showDistributionY && input$showDistributionX)
          p <- myDistribution(dat(), shiny = T)
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
        if (!uploaded) req(dataFormat() == "long")
        if (uploaded) req(uploadedFile())
        type <- "spillover"
        if (model() == "T") type <- "spillover_threshold"
        if (input$showSpilloverY) p <- mySSP(dat(), partner = "y", type = type, cor = T, shiny = T, tau = tau()$y)
        if (input$showSpilloverX) p <- mySSP(dat(), partner = "x", type = type, cor = T, shiny = T, tau = tau()$x)
        if (input$showSpilloverY && input$showSpilloverX) {
          p <- mySSP(dat(),
            type = "spillover", cor = T, shiny = T,
            legend.position = c(.35, .9)
          )
        }
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
        legend.position <- c(.9, .9)
        if (input$showACFy) {
          p <- myCF(dat(), partner = "y", type = "ACF", shiny = T)
        }
        if (input$showACFx) {
          p <- myCF(dat(), partner = "x", type = "ACF", shiny = T)
        }
        if (input$showACFy && input$showACFx) {
          p <- myCF(dat(),
            type = "ACF", shiny = T,
            legend.position = legend.position
          )
        }
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


plotsTvUI <- function(id){
  ns <- NS(id)

  fluidRow(
    column(10,
      fluidRow(
        column(11,
          offset = 1,
          fluidRow(
            column(3,
              plotOutput(ns("mean_y"), height = "150px"),
              plotOutput(ns("phi_y"), height = "150px")
            ),
            column(3,
              plotOutput(ns("alpha_y"), height = "150px"),
              plotOutput(ns("beta_y"), height = "150px"),
              style = "border-right:1px solid; border-color:LightGrey;"
            ),
            column(3,
              plotOutput(ns("mean_x"), height = "150px"),
              plotOutput(ns("phi_x"), height = "150px")
            ),
            column(3,
              plotOutput(ns("alpha_x"), height = "150px"),
              plotOutput(ns("beta_x"), height = "150px")
            )
          )
        )
      )
    )
  )
}

plotsTvServer <- function(id, t, x, y){
  moduleServer(
    id,
    function(input, output, session){
      output$alpha_y <- renderPlot({
        p <- myTSsimple(1:t(), y()$coefs$alpha, ylab = bquote(Intercept ~ italic(alpha["y,t"])), shiny = T)
        return(p)
      })
      output$phi_y <- renderPlot({
        p <- myTSsimple(1:t(), y()$coefs$phi, ylab = bquote(Carryover ~ italic(phi["y,t"])), shiny = T)
        return(p)
      })
      output$beta_y <- renderPlot({
        p <- myTSsimple(1:t(), y()$coefs$beta, ylab = bquote(Spillover ~ italic(beta["y,t"])), shiny = T)
        return(p)
      })
      
      output$alpha_x <- renderPlot({
        p <- myTSsimple(1:t(), x()$coefs$alpha, ylab = bquote(Intercept ~ italic(alpha["x,t"])), shiny = T)
        return(p)
      })
      output$phi_x <- renderPlot({
        p <- myTSsimple(1:t(), x()$coefs$phi, ylab = bquote(Carryover ~ italic(phi["x,t"])), shiny = T)
        return(p)
      })
      output$beta_x <- renderPlot({
        p <- myTSsimple(1:t(), x()$coefs$beta, ylab = bquote(Spillover ~ italic(beta["x,t"])), shiny = T)
        return(p)
      })

      tvMean <- reactive({
        mean_y <- numeric(t())
        mean_x <- numeric(t())
        for(i in 1:t()) {
          m <- .meanVar1(
            coefs_y = list(
              alpha = y()$coefs$alpha[i],
              phi = y()$coefs$phi[i],
              beta = y()$coefs$beta[i]
            ),
            coefs_x = list(
              alpha = x()$coefs$alpha[i],
              phi = x()$coefs$phi[i],
              beta = x()$coefs$beta[i]
            )
          )
          mean_y[i] <- m[1, 1]
          mean_x[i] <- m[2, 1]
        }
        return(list(y = mean_y, x = mean_x))
      })

      output$mean_y <- renderPlot({
        p <- myTSsimple(1:t(), tvMean()$y, ylab = bquote(Mean ~ italic("y"[t])), shiny = T)
        return(p)
      })

      output$mean_x <- renderPlot({
        p <- myTSsimple(1:t(), tvMean()$x, ylab = bquote(Mean ~ italic("x"[t])), shiny = T)
        return(p)
      })

    }
  )
}