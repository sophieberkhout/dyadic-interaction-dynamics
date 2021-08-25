library(shiny)
options(shiny.autoreload = TRUE)
source("simVARS.R")
source("plotFunctions.R")

ui <- navbarPage("Dyadic Interactions", id = "navbar",
  tabPanel("Simulation", value = "sim",
           testUI("test"),
     fluidRow(
       methodUI("method"),
       column(3,
              tabsetPanel(id = "yTabs")
       ),
       column(3,
              tabsetPanel(id = "xTabs"
              )
       ),
       column(2,
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
                )
              )
       )
     ),
     hr(),
     fluidRow(
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
       ),
       conditionalPanel(condition = "input.showTSy | input.showTSx",
         column(8,
                plotOutput("ts")
         )
       ),
       conditionalPanel(condition = "input.showCarryoverY | input.showCarryoverX",
         column(4,
                plotOutput("carryover")
         )
       ),
       conditionalPanel(condition = "input.showSpilloverY | input.showSpilloverX",
         column(4,
                plotOutput("spillover")
         )
       ),
       conditionalPanel(condition = "input.showACFy | input.showACFx",
         column(4,
                plotOutput("acf")
         )
       ),       conditionalPanel(condition = "input.showCCFy | input.showCCFx",
         column(4,
                plotOutput("ccf")
         )
       ),
       conditionalPanel(condition = "input.show3Dy | input.show3Dx",
         column(11,
                plotly::plotlyOutput("plotly", height = 600)
         )
       ),
       conditionalPanel(condition = "input.plot_alpha",
                        column(4,
                               plotOutput("alpha")
                        )
       )
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
  
  observeEvent(input$yNoSwitch, {
    if(input$yNoSwitch){
      updateNumericInput(session, "alpha_y_2", value = input$alpha_y)
      updateNumericInput(session, "phi_y_2", value = input$phi_y)
      updateNumericInput(session, "beta_y_2", value = input$beta_y)
      
    }
  })
  
  observeEvent(input$xNoSwitch, {
    if(input$xNoSwitch){
      updateNumericInput(session, "alpha_x_2", value = input$alpha_x)
      updateNumericInput(session, "phi_x_2", value = input$phi_x)
      updateNumericInput(session, "beta_x_2", value = input$beta_x)
      
    }
  })
  
  observeEvent(input$model, {
    removeTab("yTabs", target = "Second regime")
    removeTab("xTabs", target = "Second regime")
    removeTab("yTabs", target = "Intercept")
    removeTab("yTabs", target = "Parameters y")
    removeTab("xTabs", target = "Parameters x")
    removeTab("yTabs", target = "Means y")
    removeTab("xTabs", target = "Means x")
    removeTab("errors", target = "Innovations")
    removeTab("errors", target = "Measurement error")
    
    if(input$model != "TV" && input$model != "HMM"){
      appendTab("yTabs",
        tabPanel("Parameters y",
                 inputVARUI("yParameters")
        ), select = T
      )
      appendTab("xTabs",
        tabPanel("Parameters x",
                 inputVARUI("xParameters")
        ), select = T
      )
    }
    
    if(input$model != "HMM"){
      appendTab("errors",
        tabPanel("Innovations",
                 errorsUI("inv")
        ), select = T
      )
    }
    
    if(input$model == "L" | input$model == "HMM"){
      appendTab("errors",
                tabPanel("Measurement error",
                         errorsUI("me")
                ), select = ifelse(input$model == "L", F, T)
      )
    }
    
    if(input$model == "TV"){
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
    
    if(input$model == "HMM"){
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
    
    if(input$model == "T" || input$model == "MS"){
      appendTab("yTabs",
        tabPanel("Second regime",
                 inputVARUI("ySecondRegime"),
                 fluidRow(
                   column(6,
                          actionButton("yNoSwitch", HTML("Same as first regime <br> (no regime-switching)"))
                   ),
                   if(input$model == "T"){
                     column(6,
                            numericInput("tau_y", "Threshold", 0, width = "50%")
                     )                     
                   }

                 )
        )
      )
      appendTab("xTabs",
        tabPanel("Second regime",
                 inputVARUI("xSecondRegime"),
                 fluidRow(
                   column(6,
                          actionButton("xNoSwitch", HTML("Same as first regime <br> (no regime-switching)"))
                   ),
                   if(input$model == "T"){
                   column(6,
                          numericInput("tau_x", "Threshold", 0, width = "50%")
                   )
                   }
                 )
        )
      )
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
    
    params_y <- inputVARServer("yParameters")
    params_x <- inputVARServer("xParameters")
    
    if(input$model == "T" || input$model == "MS"){
      params_y2 <- inputVARServer("ySecondRegime")
      params_y$alpha[2] <- params_y2$alpha
      params_y$phi[2]   <- params_y2$phi
      params_y$beta[2]  <- params_y2$beta
      if(input$model == "T") params_y$tau <- input$tau_y

      params_x2 <- inputVARServer("xSecondRegime")
      params_x$alpha[2] <- params_x2$alpha
      params_x$phi[2]   <- params_x2$phi
      params_x$beta[2]  <- params_x2$beta
      if(input$model == "T") params_x$tau <- input$tau_x
    }
    if(input$model == "HMM"){
      params_y <- list(mu = c(input$mean_y_1, input$mean_y_2))
      params_x <- list(mu = c(input$mean_x_1, input$mean_x_2))
    }
    
    if(input$model != "HMM" && input$model != "L"){
      errors <- errorsServer("inv")
    } else {
      errors <- errorsServer("me")
    }
    
    probs <- NULL
    if(input$model == "MS" || input$model == "HMM") probs <- c(input$pi_o, input$pi_t)
    
    ifelse(input$dataFormat == "long", longformat <- T, longformat <- F)
    
    set.seed(input$seed)
    dat <- simVARS(occasions = input$t, burnin = input$burnin,
                   type = input$model,
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
  
  # DOWNLOAD BUTTON
  output$downloadData <- downloadHandler(
    filename = "dyadicinteractions.csv",
    content = function(file){
      write.csv(dat(), file, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)