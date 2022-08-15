estimationUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(5,
           sidebarPanel(
            # h4("test"),
            selectInput(ns("model"), "Estimation model",
                        list("First-order vector autoregressive VAR(1)" = "VAR",
                             "Latent VAR(1)" = "L",
                             "Time-varying VAR(1)" = "TV",
                             "Threshold VAR(1)" = "T",
                             "Hidden Markov model" = "HMM",
                             "Markov-switching VAR(1)" = "MS"), selected = "VAR"),
            hr(),
            actionButton(ns("estimateModel"), "Fit model"),
            width = 12
           )
    ),
    column(7,
           withSpinner(DT::dataTableOutput(ns("estimatesTable")))
    )
  )
}

estimationServer <- function(id, dataFormat, dat, params){
  moduleServer(
    id,
    function(input, output, session){
      estimates <- reactiveValues()
      estimates$estimated   <- NA
      estimates$difference  <- NA
      
      trueParams <- reactive({
        as.numeric(c(params()$y, params()$x, params()$innovations))
      })
      
      observeEvent(input$estimateModel, {
        estimates$estimated   <- 0 # somehow this is needed to get the spinner to work properly
        estimates$difference  <- 0
        
        fit <- estVAR1(dat(), dataFormat = dataFormat())
        summaryFit <- summary(fit)
        est <- summaryFit$Coefficients[c("alpha_y", "phi_y", "beta_y",
                                         "alpha_x", "phi_x", "beta_x",
                                         "z_y", "z_xy", "z_x"), 1]
        
        dif   <- est - trueParams()
        
        estimates$estimated   <- est
        estimates$difference  <- dif
      })
      
      output$estimatesTable <- DT::renderDataTable({
        parNames <- c(rep(c("Intercept \u03B1", "Carryover \u03D5", "Spillover \u03B2"), 2),
                      "Innovation variance y", "Innovation covariance", "Innovation variance x")
        
        df <- data.frame(names = parNames, true = trueParams(), 
                         estimated = estimates$estimated, 
                         difference = estimates$difference)
        
        table <- DT::datatable(df,
                               colnames = c("Parameters", "True", "Estimated", "Bias"),
                               rownames = c("Regression coefficients y", rep(NA, 2),
                                            "Regression coefficients x", rep(NA, 2), 
                                            "Innovation parameters", rep(NA, 2)),
                               options = list(dom = "t", bSort = F))
        
        table <- DT::formatRound(table, columns = 2:4, digits = 3)
      })
      
      observeEvent(params()$y, {
        estimates$estimated <- NA
        estimates$difference <- NA
        
      })
    }
  )
}