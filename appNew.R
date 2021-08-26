library(shiny)
options(shiny.autoreload = TRUE)
source("simVARS.R")
source("plotFunctions.R")

ui <- navbarPage("Dyadic Interactions", id = "navbar",
  tabPanel("Simulation", value = "sim",
     fluidRow(
       methodUI("method"),
       column(3,
              tabsetPanel(id = "yTabs")
       ),
       column(3,
              tabsetPanel(id = "xTabs"
              )
       ),
       column(3,
              tabsetPanel(id = "errors"
              ),
              conditionalPanel(condition = "input.model == 'MS' || input.model == 'HMM'",
                tabsetPanel(id = "transition",
                  tabPanel("Transition probabilities",
                    fluidRow(
                      column(6,
                             numericInput("pi_o", "Stay in 1", .5, 0, 1, .1),
                             tableOutput("pi_ot")
                      ),
                      column(6,
                             tableOutput("pi_to"),
                             numericInput("pi_t", "Stay in 2", .5, 0, 1, .1)
                      )
                    )
                  )                            
                ), ns = NS("method")
              )
       )
     ),
     hr(),
     fluidRow(
       plotsInputUI("inputPlots"),
       plotsOutputUI("outputPlots")
     ),
  ),
  tabPanel("Data", value = "data",
    fluidRow(
      column(9,
             DT::dataTableOutput("table")
      ),
      column(3,
            radioButtons("dataFormat", "Choose data format",
                         choices = list("Wide" = "wide", "Long" = "long")
            ),
            downloadButton("downloadData", "Download data")
      )
    )
  )
)

server <- function(input, output, session) {
  # DYNAMIC INPUT
  method <- reactive({ methodServer("method") })
  
  observeEvent(method()$model, {
    removeTab("yTabs", target = "Second regime")
    removeTab("xTabs", target = "Second regime")
    removeTab("yTabs", target = "Intercept")
    removeTab("yTabs", target = "Parameters y")
    removeTab("xTabs", target = "Parameters x")
    removeTab("yTabs", target = "Means y")
    removeTab("xTabs", target = "Means x")
    removeTab("errors", target = "Innovations")
    removeTab("errors", target = "Measurement error")
    removeTab("errors", target = "Second regime")
    
    # method <- methodServer("method")
    
    if(method()$model != "TV" && method()$model != "HMM"){
      appendTab("yTabs",
        tabPanel("Parameters y",
                 inputVARUI("yParameters"),
                 # conditionalPanel(condition = "input.model == 'T' || input.model == 'MS'",
                 if(method()$model == "T" || method()$model == "MS"){
                   fluidRow(
                     column(6,
                       checkboxInput("add_regime_y", "Add second regime", value = T)
                     ),
                     if(method()$model == "T"){
                       conditionalPanel(condition = "input.add_regime_y",
                         column(6,
                                numericInput("tau_y", "Threshold", 0, width = "50%")
                         )
                       )                       
                     }
                   )
                 }
        ), select = T
      )
      appendTab("xTabs",
        tabPanel("Parameters x",
                 inputVARUI("xParameters"),
                 if(method()$model == "T" || method()$model == "MS"){
                   fluidRow(
                     column(6,
                            checkboxInput("add_regime_x", "Add second regime", value = T)
                     ),
                     if(method()$model == "T"){
                       conditionalPanel(condition = "input.add_regime_x",
                                        column(6,
                                               numericInput("tau_x", "Threshold", 0, width = "50%")
                                        )
                       )
                     }
                   )
                 }
        ), select = T
      )
    }
    
    if(method()$model != "HMM"){
      appendTab("errors",
        tabPanel("Innovations",
                 errorsUI("innovations")
        ), select = T
      )
    }
    
    if(method()$model == "L" | method()$model == "HMM"){
      appendTab("errors",
                tabPanel("Measurement error",
                         errorsUI("measurementError")
                ), select = ifelse(method()$model == "L", F, T)
      )
      if(method()$model == "HMM"){
        appendTab("errors",
                  tabPanel("Second regime",
                           errorsUI("measurementErrorSecondRegime")
                  )
        )
      }
    }
    
    if(method()$model == "TV"){
      appendTab("yTabs",
        tabPanel("Intercept",
                 radioButtons("alpha_y_tv", "", list("Stable", "Time-varying"), inline = T),
                 # fluidRow(
                 #   column(6,
                 #          radioButtons("alpha_y_tv", "", list("Stable", "Time-varying")),
                 #   ),
                 #   column(6,
                 #          numericInput("alpha_y_stable", "", 0)
                 #   )
                 # ),
                 conditionalPanel(condition = "input.alpha_y_tv == 'Stable'",
                   numericInput("alpha_y_stable", "Value", 0, width = "50%")
                 ),
                 conditionalPanel(condition = "input.alpha_y_tv == 'Time-varying'",
                   fluidRow(
                     column(3,
                       radioButtons("alpha_change", "", list("Linear", "Sine"))
                     ),
                     conditionalPanel(condition = "input.alpha_change == 'Linear'",
                       column(3,
                              numericInput("alpha_from", "From", 0),
                       ),
                       column(3,
                              numericInput("alpha_to", "To", 0)
                       )
                     ),
                     conditionalPanel(condition = "input.alpha_change == 'Sine'",
                       column(3,
                              numericInput("alpha_amp", "Amplitude", 1),
                              numericInput("alpha_phase", "Phase", 0),
                       ),
                       column(3,
                              numericInput("alpha_freq", "Frequency", 1),
                              numericInput("alpha_dev", "Deviation", 0)
                       )
                     ),
                    ),
                   checkboxInput("plot_alpha", "Plot intercept")
                  )
                ), select = T,
      )
    }
    
    if(method()$model == "HMM"){
      appendTab("yTabs",
        tabPanel("Means y",
                 numericInput("mean_y_1", "First regime", 0, width = "50%"),
                 numericInput("mean_y_2", "Second regime", 0, width = "50%")
        ), select = T
      )
      appendTab("xTabs",
        tabPanel("Means x",
                 numericInput("mean_x_1", "First regime", 0, width = "50%"),
                 numericInput("mean_x_2", "Second regime", 0, width = "50%")
        ), select = T
      )
    }
    
    if(method()$model == "T" || method()$model == "MS"){
      observeEvent(input$add_regime_y, {
        removeTab("yTabs", target = "Second regime")
        if(input$add_regime_y){
          appendTab("yTabs",
            tabPanel("Second regime",
                     inputVARUI("ySecondRegime")
            )
          )          
        }
      })

      observeEvent(input$add_regime_x, {
        removeTab("xTabs", target = "Second regime")
        if(input$add_regime_x){
          appendTab("xTabs",
            tabPanel("Second regime",
                     inputVARUI("xSecondRegime")
            )
          )
        }
      })
      
      observeEvent({
        input$add_regime_y
        input$add_regime_x}, {
        removeTab("errors", target = "Second regime")
        if(input$add_regime_y | input$add_regime_x){
          appendTab("errors",
                    tabPanel("Second regime",
                             errorsUI("innovationsSecondRegime")
                    )
          )
        }
      })
    }
  })
  
  observeEvent(input$navbar, {
    if(input$navbar == "sim"){
      updateRadioButtons(session, "dataFormat", selected = "long")
    }
    if(input$navbar == "data"){
      updateRadioButtons(session, "dataFormat", selected = "wide")
    }
  })
  
  output$pi_to <- renderTable({
    pi_to <- data.frame(1 - input$pi_t)
    colnames(pi_to) <- "Switch to 1"
    return(pi_to)
  }, align = "l")
  
  output$pi_ot <- renderTable({
    pi_ot <- data.frame(1 - input$pi_o)
    colnames(pi_ot) <- "Switch to 2"
    return(pi_ot)
  }, align = "l")
  
  
  # GENERATE DATA
  dat <- reactive({
    
    # method <- methodServer("method")
    
    params_y <- inputVARServer("yParameters")
    params_x <- inputVARServer("xParameters")
    
    if(method()$model == "T" || method()$model == "MS"){
      if(input$add_regime_y){
        params_y2 <- inputVARServer("ySecondRegime")
        params_y$alpha[2] <- params_y2$alpha
        params_y$phi[2]   <- params_y2$phi
        params_y$beta[2]  <- params_y2$beta
        if(method()$model == "T") params_y$tau <- input$tau_y        
      }


      if(input$add_regime_x){
        params_x2 <- inputVARServer("xSecondRegime")
        params_x$alpha[2] <- params_x2$alpha
        params_x$phi[2]   <- params_x2$phi
        params_x$beta[2]  <- params_x2$beta
        if(method()$model == "T") params_x$tau <- input$tau_x
      }
    }
    if(method()$model == "HMM"){
      params_y <- list(mu = c(input$mean_y_1, input$mean_y_2))
      params_x <- list(mu = c(input$mean_x_1, input$mean_x_2))
    }
    
    ###### TO DO:
    ###### TVAR second regime innovations
    ###### Latent both measurement error and innovations
    if(method()$model == "VAR" || method()$model == "TV" || method()$model == "T"){
      errors <- errorsServer("innovations")
    }
    if(method()$model == "L") errors <- errorsServer("measurementError")
    if(method()$model == "HMM"){
      errors <- list(firstRegime = errorsServer("measurementError"),
                     secondRegime = errorsServer("measurementErrorSecondRegime"))
    }
    if(method()$model == "MS"){
      errors <- list(firstRegime = errorsServer("innovations"),
                     secondRegime = errorsServer("innovationsSecondRegime"))
    }
    
    probs <- NULL
    if(method()$model == "MS" || method()$model == "HMM") probs <- c(input$pi_o, input$pi_t)
    
    ifelse(input$dataFormat == "long", longformat <- T, longformat <- F)
    
    set.seed(method()$seed)
    dat <- simVARS(occasions = method()$t, burnin = method()$burnin,
                   type = method()$model,
                   params_y = params_y,
                   params_x = params_x,
                   probs = probs,
                   innovations = errors,
                   longformat = longformat)
    
    dat
  })
  
  # TABLE
  output$table <- DT::renderDataTable({
    dtable <- DT::datatable(dat(), rownames = F) 
    if(input$dataFormat == "wide"){
      dtable <- DT::formatRound(dtable, columns = c("x", "y"), digits = 3)
    }
    dtable
  })
  
  # PLOTS
  # plotsServer("inputPlots")
  output$ts <- renderPlot({ 
    req(input$dataFormat == "long")
    ifelse(method()$model == "T", regime <- T, regime <- F)
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
      a <- change_linear(input$alpha_from, input$alpha_to, method()$t)
    } else if(input$alpha_change == "Sine"){
      a <- change_sine(amplitude = input$alpha_amp, freq = input$alpha_freq,
                       phase = input$alpha_phase, deviation = input$alpha_dev,
                       t = method()$t)
    }
    return(a)
  })
  
  output$alpha <- renderPlot({
    myTSsimple(1:method()$t, tv_alpha(), ylab = "Intercept", shiny = T)
    # plot(tv_alpha(), type = "l")
  })
  
  # DOWNLOAD BUTTON
  output$downloadData <- downloadHandler(
    filename = "dyadicinteractions.csv",
    content = function(file){
      write.csv(dat(), file, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)