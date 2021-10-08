formulaUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("formula"))
}

formulaServer <- function(id, params_y, params_x){
  moduleServer(
    id,
    function(input, output, server){
      output$formula <- renderUI({
        alpha_y <- params_y$alpha()
        phi_y <- params_y$phi()
        beta_y <- params_y$beta()
        alpha_x <- params_x$alpha()
        phi_x <- params_x$phi()
        beta_x <- params_x$beta()
        withMathJax(
          sprintf("$$y = %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t$$", 
                  alpha_y, phi_y, beta_y),
          sprintf("$$x = %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t$$", 
                  alpha_x, phi_x, beta_x)
        )
      })
    }
  )
}