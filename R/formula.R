formulaUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("formula"))
}

formulaServer <- function(id, model, 
                          params_y, params_x, params_y_2, params_x_2,
                          tau_y, tau_x,
                          i_y, i_x,
                          means_y, means_x,
                          innovations){
  moduleServer(
    id,
    function(input, output, server){
      output$formula <- renderUI({
        if(model() == "VAR") {
          myFormula <- sprintf(
            "$$
                y_t = %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\quad
                \\quad x_t = %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t
             $$", 
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_x$alpha(), params_x$phi(), params_x$beta()
          )
          zFormula <- sprintf(
            "\\begin{aligned}
              Z &\\sim \\mathcal{N}(0, \\Sigma) \\\\
              \\Sigma &=
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{aligned}",
            innovations$y(), innovations$c_yx(), innovations$x()
          )
        }
        if(model() == "L") {
          myFormula <- sprintf(
            "\\begin{equation}
              \\begin{aligned}
                  \\eta_{y,t} &= %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                  y_t &= %.1f + \\varepsilon_t
              \\end{aligned}
              \\quad \\quad \\!
              \\begin{aligned}
                  \\eta_{x,t} &= %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\\\
                  x_t &= %.1f + \\varepsilon_t
             \\end{aligned}
            \\end{equation}", 
            params_y$alpha(), params_y$phi(), params_y$beta(), i_y$mean(),
            params_x$alpha(), params_x$phi(), params_x$beta(), i_x$mean()
          )
        }
        if(model() == "TV") {
          myFormula <- sprintf(
            "$$
                y_t = \\alpha_{y, t} + \\phi_{y, t} y_{t-1} + \\beta_{y, t} x_{t-1} + \\zeta_t \\quad
                \\quad x_t = \\alpha_{x, t} + \\phi_{x, t} x_{t-1} + \\beta_{x, t} y_{t-1} + \\zeta_t
             $$"
          )
        }
        if(model() == "T") {
          myFormula <- sprintf(
            "\\begin{aligned}
                y_t = 
                \\begin{cases}
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\quad & \\text{if} \\quad x_{t-1} \\leq %.1f \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\quad & \\text{if} \\quad x_{t-1} > %.1f \\\\
                \\end{cases}
                \\quad \\quad x_t =
                \\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\quad & \\text{if} \\quad y_{t-1} \\leq %.1f \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\quad & \\text{if} \\quad y_{t-1} > %.1f
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(), tau_y(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta(), tau_y(),            
            params_x$alpha(), params_x$phi(), params_x$beta(), tau_x(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta(), tau_x()
          )
        }
        if(model() == "HMM") {
          myFormula <- sprintf(
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                %.1f + \\varepsilon_t \\quad && \\text{if} \\quad s_t = 1 \\\\
                %.1f + \\varepsilon_t \\quad && \\text{if} \\quad s_t = 2 \\\\
                \\end{cases}
                \\quad \\quad x_t =
                \\begin{cases}
                %.1f + \\varepsilon_t \\quad && \\text{if} \\quad s_t = 1 \\\\
                %.1f + \\varepsilon_t \\quad && \\text{if} \\quad s_t = 2
                \\end{cases}
             \\end{aligned}", 
            means_y$mu_1(), means_y$mu_2(), means_x$mu_1(), means_x$mu_2()
          )
        }
        if(model() == "MS") {
          myFormula <- sprintf(
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\quad && \\text{if} \\quad s_t = 1 \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\quad && \\text{if} \\quad s_t = 2 \\\\
                \\end{cases}
                \\quad \\quad x_t =
                \\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\quad && \\text{if} \\quad s_t = 1 \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\quad && \\text{if} \\quad s_t = 2
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta(),            
            params_x$alpha(), params_x$phi(), params_x$beta(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta()
          )
        }
        
        withMathJax(myFormula, zFormula)
      })
    }
  )
}