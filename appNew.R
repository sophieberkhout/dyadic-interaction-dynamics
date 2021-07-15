library(shiny)
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
       column(4,
              tabsetPanel(id = "yTabs",
                          tabPanel("y",
                                   numericInput("alpha_y", "Intercept", 0, width = "50%"),
                                   sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                                   sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                          )
              )
       ),
       column(4,
              tabsetPanel(id = "xTabs",
                          tabPanel("x",
                                   numericInput("alpha_x", "Intercept", 0, width = "50%"),
                                   sliderInput("phi_x", "Carryover", -1, 1, .5, .1),
                                   sliderInput("beta_x", "Spillover", -1, 1, .2, .1)
                          )
              )
       )
     ),
     hr(),
     fluidRow(
       column(6,
              plotOutput("ts")
       ),
       column(6,
              plotOutput("ssp")
       )
     )
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
  
  output$table <- DT::renderDataTable({
    dtable <- DT::datatable(dat(), rownames = F) 
    if(input$dataFormat == "wide"){
      dtable <- DT::formatRound(dtable, columns = c("x", "y"), digits = 3)
    }
    dtable
  })
  
  output$ts <- renderPlot({ 
    req(input$dataFormat == "long")
    myTS(dat(), shiny = T)
  })
  
  output$ssp <- renderPlot({ 
    req(input$dataFormat == "long")
    mySSP(dat(), type = "carryover", shiny = T)
  })
  
  output$downloadData <- downloadHandler(
    filename = "dyadicinteractions.csv",
    content = function(file){
      write.csv(dat(), file, row.names = F)
    }
  )
}


shinyApp(ui = ui, server = server)
