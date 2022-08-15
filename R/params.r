paramsUI <- function(id) {
    tabsetPanel(id = "yTabs",

        tabPanel("Regression coefficients",
                inputVARUI("yParameters")
        ),
        tabPanel(HTML("Intercept &#120572;"),
                tvUI("intercept_y", type = "num"),
        ),
        tabPanel(HTML("Carryover &#120601;"),
                tvUI("carryover_y"),
        ),
        tabPanel(HTML("Spillover &#120573;"),
                tvUI("spillover_y"),
        ),
        tabPanel("Means",
                meansUI("means_y")
        ),
        tabPanel("Indicator",
                indicatorUI("i_y")
        ),
        tabPanel("Regime 1",
                inputVARUI("yFirstRegime")
        ),
        tabPanel("Regime 2",
                inputVARUI("ySecondRegime")
        )
    )
}

paramsServer <- function(id, model, t, pi_o, pi_t) {
  moduleServer(
    id,
    function(input, output, session){

        # tv_alpha_y <- tvServer("intercept_y", t())
        # tv_phi_y   <- tvServer("carryover_y", t())
        # tv_beta_y  <- tvServer("spillover_y", t())
        
        # tv_alpha_x <- tvServer("intercept_x", t())
        # tv_phi_x   <- tvServer("carryover_x", t())
        # tv_beta_x  <- tvServer("spillover_x", t())
        
        means_y <- meansServer("means_y")
        means_x <- meansServer("means_x")
        
        i_y <- indicatorServer("i_y")
        i_x <- indicatorServer("i_x")
        
        innovations <- errorsServer("innovations")
        innovations_1 <- errorsServer("innovationsFirstRegime")
        innovations_2 <- errorsServer("innovationsSecondRegime")
        measurement_errors <- errorsServer("measurementError")
        measurement_errors_1 <- errorsServer("measurementErrorFirstRegime")
        measurement_errors_2 <- errorsServer("measurementErrorSecondRegime")

        params_y <- inputVARServer("yParameters")
        params_x <- inputVARServer("xParameters")
        params_y_1 <- inputVARServer("yFirstRegime")
        params_x_1 <- inputVARServer("xFirstRegime")
        params_y_2 <- inputVARServer("ySecondRegime")
        params_x_2 <- inputVARServer("xSecondRegime")
        
        params <- reactive({
            tv_alpha_y <- tvServer("intercept_y", t())
            tv_phi_y   <- tvServer("carryover_y", t())
            tv_beta_y  <- tvServer("spillover_y", t())
            
            tv_alpha_x <- tvServer("intercept_x", t())
            tv_phi_x   <- tvServer("carryover_x", t())
            tv_beta_x  <- tvServer("spillover_x", t())

            params_y <- list(alpha = params_y$alpha(), phi = params_y$phi(), beta = params_y$beta())
            params_x <- list(alpha = params_x$alpha(), phi = params_x$phi(), beta = params_x$beta())
            
            innovations <- c(innovations$y(), innovations$c_yx(), innovations$x())
            
            if(model() == "T" || model() == "MS"){
            params_y <- list(alpha = params_y_1$alpha(), phi = params_y_1$phi(), beta = params_y_1$beta())
            params_x <- list(alpha = params_x_1$alpha(), phi = params_x_1$phi(), beta = params_x_1$beta())
            
            params_y$alpha[2] <- params_y_2$alpha()
            params_y$phi[2]   <- params_y_2$phi()
            params_y$beta[2]  <- params_y_2$beta()
            if(model() == "T") params_y$tau <- input$tau_y        
            
            params_x$alpha[2] <- params_x_2$alpha()
            params_x$phi[2]   <- params_x_2$phi()
            params_x$beta[2]  <- params_x_2$beta()
            if(model() == "T") params_x$tau <- input$tau_x
            
            innovations_1 <- c(innovations_1$y(), innovations_1$c_yx(), innovations_1$x())
            innovations_2 <- c(innovations_2$y(), innovations_2$c_yx(), innovations_2$x())
            if(model() == "T") innovations_1[2] <- input$yx_T
            
            innovations <- list(firstRegime = innovations_1,
                                secondRegime = innovations_2) 
            }
            if(model() == "HMM"){
            params_y <- list(mu = c(means_y$mu_1(), means_y$mu_2()))
            params_x <- list(mu = c(means_x$mu_1(), means_x$mu_2()))
            
            measurement_errors_1 <- c(measurement_errors_1$y(), measurement_errors_1$c_yx(), 
                                        measurement_errors_1$x())
            
            measurement_errors_2 <- c(measurement_errors_2$y(), measurement_errors_2$c_yx(), 
                                        measurement_errors_2$x())
            
            measurement_errors <- list(firstRegime = measurement_errors_1,
                                        secondRegime = measurement_errors_2)
            }
            if(model() == "TV"){
            params_y <- list(alpha = tv_alpha_y$p(), phi = tv_phi_y$p(), beta = tv_beta_y$p())
            params_x <- list(alpha = tv_alpha_x$p(), phi = tv_phi_x$p(), beta = tv_beta_x$p())
            }
            
            indicators_y <- NULL
            indicators_x <- NULL
            if(model() == "L"){
            indicators_y <- list(m = i_y$mean(), l = 1)
            indicators_x <- list(m = i_x$mean(), l = 1)
            
            measurement_errors <- c(measurement_errors$y(), measurement_errors$c_yx(), 
                                    measurement_errors$x())
            }
            
            probs <- NULL
            if(model() == "MS" || model() == "HMM") probs <- c(pi_o(), pi_t())
            

            if(model() != "L" & model() != "HMM") measurement_errors <- NULL
            
            return(
              list(
                  y = params_y,
                  x = params_x,
                  innovations = innovations,
                  errors = measurement_errors,
                  indicators_y = indicators_y,
                  indicators_x = indicators_x,
                  probs = probs
              )
            )
        })
        
        return(params)
    }
  )
}