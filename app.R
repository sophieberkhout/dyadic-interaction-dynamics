library(shiny)
if(!("shinycssloaders" %in% installed.packages())){
  install.packages("shinycssloaders")
}
if(!("ggplot2" %in% installed.packages())){
  install.packages("ggplot2")
}
if(!("plotly" %in% installed.packages())){
  install.packages("plotly")
}
if(!("DT" %in% installed.packages())){
  install.packages("DT")
}
if(!("viridis" %in% installed.packages())){
  install.packages("viridis")
}
if(!("oddsratio" %in% installed.packages())){
  install.packages("oddsratio")
}
if(!("MASS" %in% installed.packages())){
  install.packages("MASS")
}
if(!("stringr" %in% installed.packages())){
  install.packages("stringr")
}
library("shinycssloaders")
library("ggplot2")
library("plotly")
options(shiny.autoreload = TRUE)
source("simVARS.R")
source("plotFunctions.R")
options(shiny.error = F)
options(spinner.type = 7)
options(spinner.size = 0.2)
options(spinner.color = "grey")

ui <- navbarPage("Dyadic Interactions", id = "navbar",
  tabPanel("Simulation", value = "sim",
     fluidRow(
       methodUI("method"),
       column(3, style = 'border-right:1px solid; border-color:LightGrey;',
              h3(em("y"), align = "right"),
              tabsetPanel(id = "yTabs",
                          tabPanel("Parameters",
                                   inputVARUI("yParameters")
                          ),
                          tabPanel("Intercept",
                                   tvUI("intercept_y", type = "num"),
                          ),
                          tabPanel("Carryover",
                                   tvUI("carryover_y"),
                          ),
                          tabPanel("Spillover",
                                   tvUI("spillover_y"),
                          ),
                          tabPanel("Means",
                                   meansUI("means_y")
                          ),
                          tabPanel("Indicator",
                                   indicatorUI("i_y")
                          ),
                          tabPanel("Second regime",
                                   inputVARUI("ySecondRegime")
                          )
              ),
              conditionalPanel(condition =  "input.model == 'T'",
                               hr(),
                               fluidRow(
                                 column(3,
                                        numericInput("tau_y", "Threshold", 0)
                                 )
                               ), ns = NS("method")
              )
       ),
       column(3, style = 'border-right:1px solid; border-color:LightGrey;',
              h3(em("x"), align = "right"),
              tabsetPanel(id = "xTabs",
                tabPanel("Parameters",
                         inputVARUI("xParameters")
                ),
                tabPanel("Intercept",
                         tvUI("intercept_x", type = "num"),
                ),
                tabPanel("Carryover",
                         tvUI("carryover_x"),
                ),
                tabPanel("Spillover",
                         tvUI("spillover_x"),
                ),
                tabPanel("Means",
                         meansUI("means_x")
                ),
                tabPanel("Indicator",
                         indicatorUI("i_x")
                ),
                tabPanel("Second regime",
                         inputVARUI("xSecondRegime")
                )
              ),
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
              tabsetPanel(id = "errors",
                          tabPanel("Innovations",
                                   errorsUI("innovations")
                          ),
                          tabPanel("Measurement error",
                                   errorsUI("measurementError")
                          ),
                          tabPanel("Second regime",
                                   errorsUI("measurementErrorSecondRegime")
                          ),
                          tabPanel("Second regime ",
                                   errorsUI("innovationsSecondRegime")
                          )
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
     formulaUI("formula"),
     hr(),
     plotsInputUI("inputPlots"),
  ),
  tabPanel("Data", value = "data",
    fluidRow(
      column(9,
             withSpinner(DT::dataTableOutput("table"))
      ),
      column(3,
            radioButtons("dataFormat", "Choose data format",
                         choices = list("Wide" = "wide", "Long" = "long")
            ),
            downloadButton("downloadData", "Download data")
      )
    )
  ),
  tabPanel("Info", value = "info",
           fluidRow(
             column(6,
                    includeMarkdown("info.Rmd")
             ),
             column(6,
                    includeMarkdown("help.Rmd")
             )
           )
  )
)

server <- function(input, output, session) {
  
  # DYNAMIC INPUT
  method <- methodServer("method")
  
  observeEvent(method$model(), {
    hideTab("yTabs", target = "Second regime")
    hideTab("xTabs", target = "Second regime")
    hideTab("yTabs", target = "Intercept")
    hideTab("yTabs", target = "Carryover")
    hideTab("yTabs", target = "Spillover")
    hideTab("xTabs", target = "Intercept")
    hideTab("xTabs", target = "Carryover")
    hideTab("xTabs", target = "Spillover")
    hideTab("yTabs", target = "Parameters")
    hideTab("xTabs", target = "Parameters")
    hideTab("yTabs", target = "Means")
    hideTab("xTabs", target = "Means")
    hideTab("yTabs", target = "Indicator")
    hideTab("xTabs", target = "Indicator")
    hideTab("errors", target = "Innovations")
    hideTab("errors", target = "Measurement error")
    hideTab("errors", target = "Second regime")
    hideTab("errors", target = "Second regime ")
    
    
    if(method$model() != "TV" && method$model() != "HMM"){
      showTab("yTabs", "Parameters",
              select = T
      )
      showTab("xTabs", "Parameters",
              select = T
      )
    }
    
    if(method$model() != "HMM"){
      showTab("errors", "Innovations",
              select = T
      )
    }
    
    if(method$model() == "L" | method$model() == "HMM"){
      showTab("errors", "Measurement error",
                select = ifelse(method$model() == "L", F, T)
      )
      if(method$model() == "HMM"){
        showTab("errors", "Second regime",
        )
      }
    }
    
    if(method$model() == "TV"){
      showTab("yTabs", "Intercept",
                select = T
      )
      showTab("yTabs", "Carryover"
      )
      showTab("yTabs", "Spillover"
      )
      
      showTab("xTabs", "Intercept",
                select = T
      )
      showTab("xTabs", "Carryover"
      )
      showTab("xTabs", "Spillover"
      )
    }
    
    if(method$model() == "HMM"){
      showTab("yTabs", "Means",
                 select = T
      )
      showTab("xTabs", "Means",
                select = T
      )
    }
    
    if(method$model() == "L"){
      showTab("yTabs", "Indicator"
      )
      showTab("xTabs", "Indicator"
      )
    }
    
    if(method$model() == "T" || method$model() == "MS"){
      showTab("yTabs", "Second regime"
      )          
      showTab("xTabs", "Second regime"
      )
      showTab("errors", "Second regime "
      )
    }
  }, priority = 1)
  
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

  formulaServer("formula", method$model,
                params_y, params_x, params_y_2, params_x_2,
                reactive({input$tau_y}), reactive({input$tau_x}),
                reactive({input$yx_T}),
                i_y, i_x,
                means_y, means_x,
                innovations, innovations_2,
                measurement_errors, measurement_errors_2)
  
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