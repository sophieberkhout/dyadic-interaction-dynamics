library(shiny)
options(shiny.autoreload = TRUE)
source("simVARS.R")
source("plotFunctions.R")
options(shiny.error = F)

ui <- navbarPage("Dyadic Interactions", id = "navbar",
  tabPanel("Simulation", value = "sim",
     fluidRow(
       methodUI("method"),
       column(3,
              h3(em("y"), align = "right"),
              tabsetPanel(id = "yTabs"),
              conditionalPanel(condition =  "input.model == 'T'",
                               hr(),
                               fluidRow(
                                 column(3,
                                        numericInput("tau_y", "Threshold", 0)
                                 )
                               ), ns = NS("method")
              )
       ),
       column(3,
              h3(em("x"), align = "right"),
              tabsetPanel(id = "xTabs"),
              conditionalPanel(condition =  "input.model == 'T'",
                               hr(),
                               fluidRow(
                                 column(3,
                                        numericInput("tau_x", "Threshold", 0)
                                 )
                               ), ns = NS("method")
              )
       ),
       column(3, style = "padding-top:57px",
              tabsetPanel(id = "errors"
              ),
              conditionalPanel(condition = "input.model == 'T'",
                               hr(),
                               h5("For all regime combinations"),
                               fluidRow(
                                 column(4,
                                        numericInput("yx_T", "Correlation", .3, -1, 1, .1),
                                 )
                               ), ns = NS("method")
              ),
              conditionalPanel(condition = "input.model == 'MS' || input.model == 'HMM'",
                tabsetPanel(id = "transition",
                  tabPanel("Transition probabilities",
                    fluidRow(style = "padding-top:5px",
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
     plotsInputUI("inputPlots"),
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
  method <- methodServer("method")
  
  observeEvent(method$model(), {
    removeTab("yTabs", target = "Second regime")
    removeTab("xTabs", target = "Second regime")
    removeTab("yTabs", target = "Intercept")
    removeTab("yTabs", target = "Carryover")
    removeTab("yTabs", target = "Spillover")
    removeTab("xTabs", target = "Intercept")
    removeTab("xTabs", target = "Carryover")
    removeTab("xTabs", target = "Spillover")
    removeTab("yTabs", target = "Parameters")
    removeTab("xTabs", target = "Parameters")
    removeTab("yTabs", target = "Means")
    removeTab("xTabs", target = "Means")
    removeTab("yTabs", target = "Indicator")
    removeTab("xTabs", target = "Indicator")
    removeTab("errors", target = "Innovations")
    removeTab("errors", target = "Measurement error")
    removeTab("errors", target = "Second regime")
    
    if(method$model() != "TV" && method$model() != "HMM"){
      appendTab("yTabs",
        tabPanel("Parameters",
                 inputVARUI("yParameters")
        ), select = T
      )
      appendTab("xTabs",
        tabPanel("Parameters",
                 inputVARUI("xParameters")
        ), select = T
      )
    }
    
    if(method$model() != "HMM"){
      appendTab("errors",
        tabPanel("Innovations",
                 errorsUI("innovations")
        ), select = T
      )
    }
    
    if(method$model() == "L" | method$model() == "HMM"){
      appendTab("errors",
                tabPanel("Measurement error",
                         errorsUI("measurementError")
                ), select = ifelse(method$model() == "L", F, T)
      )
      if(method$model() == "HMM"){
        appendTab("errors",
                  tabPanel("Second regime",
                           errorsUI("measurementErrorSecondRegime")
                  )
        )
      }
    }
    
    if(method$model() == "TV"){
      appendTab("yTabs",
        tabPanel("Intercept",
                 tvUI("intercept_y"),
                ), select = T
      )
      appendTab("yTabs",
                tabPanel("Carryover",
                         tvUI("carryover_y"),
                )
      )
      appendTab("yTabs",
                tabPanel("Spillover",
                         tvUI("spillover_y"),
                )
      )
      
      appendTab("xTabs",
                tabPanel("Intercept",
                         tvUI("intercept_x"),
                ), select = T
      )
      appendTab("xTabs",
                tabPanel("Carryover",
                         tvUI("carryover_x"),
                )
      )
      appendTab("xTabs",
                tabPanel("Spillover",
                         tvUI("spillover_x"),
                )
      )
    }
    
    if(method$model() == "HMM"){
      appendTab("yTabs",
        tabPanel("Means",
                 meansUI("means_y")
        ), select = T
      )
      appendTab("xTabs",
        tabPanel("Means",
                 meansUI("means_x")
        ), select = T
      )
    }
    
    if(method$model() == "L"){
      appendTab("yTabs",
                tabPanel("Indicator",
                         indicatorUI("i_y")
                )
      )
      appendTab("xTabs",
                tabPanel("Indicator",
                         indicatorUI("i_x")
                )
      )
    }
    
    if(method$model() == "T" || method$model() == "MS"){
      removeTab("yTabs", target = "Second regime")
      appendTab("yTabs",
        tabPanel("Second regime",
                 inputVARUI("ySecondRegime")
        )
      )          

      removeTab("xTabs", target = "Second regime")
      appendTab("xTabs",
        tabPanel("Second regime",
                 inputVARUI("xSecondRegime")
        )
      )

      removeTab("errors", target = "Second regime")
      appendTab("errors",
                tabPanel("Second regime",
                         errorsUI("innovationsSecondRegime")
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
  
  tv_alpha_y <- tvServer("intercept_y", method$t)
  tv_phi_y   <- tvServer("carryover_y", method$t)
  tv_beta_y  <- tvServer("spillover_y", method$t)
  
  tv_alpha_x <- tvServer("intercept_x", method$t)
  tv_phi_x   <- tvServer("carryover_x", method$t)
  tv_beta_x  <- tvServer("spillover_x", method$t)
  
  means_y <- meansServer("means_y")
  means_x <- meansServer("means_x")
  
  i_y <- indicatorServer("i_y")
  i_x <- indicatorServer("i_x")
  
  innovations <- errorsServer("innovations")
  innovations_2 <- errorsServer("innovationsSecondRegime")
  measurement_errors <- errorsServer("measurementError")
  measurement_errors_2 <- errorsServer("measurementErrorSecondRegime")

  params_y <- inputVARServer("yParameters")
  params_x <- inputVARServer("xParameters")
  params_y_2 <- inputVARServer("ySecondRegime")
  params_x_2 <- inputVARServer("xSecondRegime")

  # GENERATE DATA
  dat <- reactive({
    params_y <- list(alpha = params_y$alpha(), phi = params_y$phi(), beta = params_y$beta())
    params_x <- list(alpha = params_x$alpha(), phi = params_x$phi(), beta = params_x$beta())
    
    if(method$model() == "T" || method$model() == "MS"){
      params_y$alpha[2] <- params_y_2$alpha()
      params_y$phi[2]   <- params_y_2$phi()
      params_y$beta[2]  <- params_y_2$beta()
      if(method$model() == "T") params_y$tau <- input$tau_y        

      params_x$alpha[2] <- params_x_2$alpha()
      params_x$phi[2]   <- params_x_2$phi()
      params_x$beta[2]  <- params_x_2$beta()
      if(method$model() == "T") params_x$tau <- input$tau_x
    }
    if(method$model() == "HMM"){
      params_y <- list(mu = c(means_y$mu_1(), means_y$mu_2()))
      params_x <- list(mu = c(means_x$mu_1(), means_x$mu_2()))
    }
    if(method$model() == "TV"){
      params_y <- list(alpha = tv_alpha_y$p(), phi = tv_phi_y$p(), beta = tv_beta_y$p())
      params_x <- list(alpha = tv_alpha_x$p(), phi = tv_phi_x$p(), beta = tv_beta_x$p())
    }
    
    innovations <- c(innovations$y(), innovations$c_yx(), innovations$x())

    indicators_y <- NULL
    indicators_x <- NULL
    if(method$model() == "L" | method$model() == "HMM"){
      measurement_errors <- c(measurement_errors$y(), measurement_errors$c_yx(), 
                              measurement_errors$x())
    }
    
    if(method$model() == "L") {
      indicators_y <- list(m = i_y$mean(), l = 1)
      indicators_x <- list(m = i_x$mean(), l = 1)
    }
    if(method$model() == "HMM"){
      measurement_errors_2 <- c(measurement_errors_2$y(), measurement_errors_2$c_yx(), 
                                measurement_errors_2$x())
      errors <- list(firstRegime = measurement_errors,
                     secondRegime = measurement_errors_2)
      measurement_errors <- errors
    }
    if(method$model() == "MS" || method$model() == "T"){
      innovations_2 <- c(innovations_2$y(), innovations_2$c_yx(), innovations_2$x())
      if(method$model() == "T") innovations[2] <- input$yx_T
      errors <- list(firstRegime = innovations,
                     secondRegime = innovations_2)
      innovations <- errors      
    }
    
    probs <- NULL
    if(method$model() == "MS" || method$model() == "HMM") probs <- c(input$pi_o, input$pi_t)

    ifelse(input$dataFormat == "long", longformat <- T, longformat <- F)
    
    if(method$model() != "L" & method$model() != "HMM") measurement_errors <- NULL
    
    set.seed(method$seed())
    dat <- simVARS(occasions = method$t(), burnin = method$burnin(),
                   type = method$model(),
                   params_y = params_y,
                   params_x = params_x,
                   probs = probs,
                   indicators_y = indicators_y,
                   indicators_x = indicators_x,
                   errors = measurement_errors,
                   innovations = innovations,
                   longformat = longformat)
    
    dat
  })
  
  # PLOTS
  dataFormat <- reactive({ input$dataFormat })
  plotsServer("inputPlots", dataFormat, method$model, method$t, dat,
              tv = list(alpha_y = tv_alpha_y, phi_y = tv_phi_y, beta_y = tv_beta_y,
                        alpha_x = tv_alpha_x, phi_x = tv_phi_x, beta_x = tv_beta_x))
  
  # TABLE
  output$table <- DT::renderDataTable({
    dtable <- DT::datatable(dat(), rownames = F,
                            options = list(dom = "pt", pageLength = 15)) 
    if(input$dataFormat == "wide"){
      dtable <- DT::formatRound(dtable, columns = c("x", "y"), digits = 3)
    }
    dtable
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