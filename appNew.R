library(shiny)
options(shiny.autoreload = TRUE)
source("simVARS.R")
source("plotFunctions.R")

ui <- navbarPage("Dyadic Interactions", id = "navbar",
  tabPanel("Simulation", value = "sim",
     fluidRow(
       column(3,
              h4("Method"),
              selectInput("model", "Data Generating Model", 
                          list("VAR" = "VAR", "LVAR" = "L", "TV-VAR" = "TV", 
                               "TVAR" = "T", "HMM" = "HMM", "MSVAR" = "MS"), selected = 1),
              numericInput("t", "Measurement occasions", 300, min = 2, step = 50), # 1 does not work
              numericInput("burnin", "Burnin", 20, min = 0, step = 10),
              numericInput("seed", "Seed", 1, min = 1, max = .Machine$integer.max)
       ),
       column(3,
              tabsetPanel(id = "yTabs",
                          tabPanel("y",
                                   numericInput("alpha_y", "Intercept", 0, width = "50%"),
                                   sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                                   sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                          )
              )
       ),
       column(3,
              tabsetPanel(id = "xTabs",
                          tabPanel("x",
                                   numericInput("alpha_x", "Intercept", 0, width = "50%"),
                                   sliderInput("phi_x", "Carryover", -1, 1, .5, .1),
                                   sliderInput("beta_x", "Spillover", -1, 1, .2, .1)
                          )
              )
       ),
       column(3,
              h4("Plots"),
              fluidRow(column(6, h5("Time Series")),
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
              fluidRow(column(6, h5("3D State Space")),
                       column(3,
                              checkboxInput("show3Dy", "y")
                       ),
                       column(3,
                              checkboxInput("show3Dx", "x")
                       )
              ),
              fluidRow(column(6, h5("Autocorrelation Function")),
                column(3,
                       checkboxInput("showACFy", "y")
                ),
                column(3,
                       checkboxInput("showACFx", "x")
                )
              ),
              fluidRow(column(6, h5("Cross-correlation Function")),
                       column(3,
                              checkboxInput("showCCFy", "(y * x)")
                       ),
                       column(3,
                              checkboxInput("showCCFx", "(x * y)")
                       )
              )
       )
     ),
     hr(),
     fluidRow(
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
  
  observeEvent(input$model, {
    if(input$model == "T"){
      appendTab("yTabs",
        tabPanel("y regime 2",
                 numericInput("alpha_y_2", "Intercept", 0, width = "50%"),
                 sliderInput("phi_y_2", "Carryover", -1, 1, .5, .1),
                 sliderInput("beta_y_2", "Spillover", -1, 1, .2, .1),
                 fluidRow(
                   column(6,
                          checkboxInput("yRegime2", "Add regime")
                   ),
                   column(6,
                          numericInput("tau_y", "Threshold", 0, width = "50%")
                   )
                 )
        )
      )
      appendTab("xTabs",
        tabPanel("x regime 2",
                 numericInput("alpha_x_2", "Intercept", 0, width = "50%"),
                 sliderInput("phi_x_2", "Carryover", -1, 1, .5, .1),
                 sliderInput("beta_x_2", "Spillover", -1, 1, .2, .1),
                 fluidRow(
                   column(6,
                          checkboxInput("xRegime2", "Add regime")
                   ),
                   column(6,
                          numericInput("tau_x", "Threshold", 0, width = "50%")
                   )
                 )
        )
      )
    } else {
      removeTab("yTabs", target = "y regime 2")
      removeTab("xTabs", target = "x regime 2")
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
  
  # GENERATE DATA
  dat <- reactive({
    params_y <- list(alpha = input$alpha_y, phi = input$phi_y, beta = input$beta_y)
    params_x <- list(alpha = input$alpha_x, phi = input$phi_x, beta = input$beta_x)

    if(input$model == "T"){
      if(input$yRegime2){
        params_y$alpha[2] <- input$alpha_y_2
        params_y$phi[2]   <- input$phi_y_2
        params_y$beta[2]  <- input$beta_y_2
        params_y$tau      <- input$tau_y
      }
      if(input$xRegime2){
        params_x$alpha[2] <- input$alpha_x_2
        params_x$phi[2]   <- input$phi_x_2
        params_x$beta[2]  <- input$beta_x_2
        params_x$tau      <- input$tau_x
      }
    }
    
    ifelse(input$dataFormat == "long", longformat <- T, longformat <- F)
      
    dat <- simVARS(occasions = input$t, burnin = input$burnin,
                   type = input$model,
                   params_y = params_y,
                   params_x = params_x,
                   seed = input$seed,
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
  
  # DOWNLOAD BUTTON
  output$downloadData <- downloadHandler(
    filename = "dyadicinteractions.csv",
    content = function(file){
      write.csv(dat(), file, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)