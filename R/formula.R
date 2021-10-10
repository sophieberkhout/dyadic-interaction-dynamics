formulaUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(3, offset = 3,
           uiOutput(ns("formula_y"))
    ),
    column(3,
           uiOutput(ns("formula_x"))
    ),
    column(3,
           uiOutput(ns("formula_z"))
    )
  )
}

formulaServer <- function(id, model, 
                          params_y, params_x, params_y_2, params_x_2,
                          tau_y, tau_x, cov_T,
                          i_y, i_x,
                          means_y, means_x,
                          innovations, innovations_2,
                          measurement_errors, measurement_errors_2){
  moduleServer(
    id,
    function(input, output, server){
      
      covs <- reactive({
        v <- c(innovations$y(), innovations$x(), 
               innovations_2$y(), innovations_2$x())
        v <- sqrt(v)
        y1x1 <- v[1] * v[2] * cov_T()
        y1y1 <- v[1] * v[3] * cov_T()
        y1x2 <- v[1] * v[4] * cov_T()
        x1y2 <- v[2] * v[3] * cov_T()
        x1x1 <- v[2] * v[4] * cov_T()
        y2x2 <- v[3] * v[4] * cov_T()
        
        return(list(y1x1 = y1x1, y1y1 = y1y1, y1x2 = y1x2,
                    x1y2 = x1y2, x1x1 = x1x1, y2x2 = y2x2))
      })
      
      tex <- reactive({
        if(model() == "VAR") {
          formula_y <- sprintf(
            "$$ y_t = %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t $$", 
            params_y$alpha(), params_y$phi(), params_y$beta()
          )
          
          formula_x <- sprintf(
            "$$ x_t = %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t $$", 
            params_x$alpha(), params_x$phi(), params_x$beta()
          )
          
          formula_z <- sprintf(
            "\\begin{equation}
              Z \\sim \\mathcal{N}(0, \\Sigma), \\quad
              \\Sigma =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{equation}",
            innovations$y(), innovations$c_yx(), innovations$x()
          )
        }
        if(model() == "L") {
          formula_y <- sprintf(
            "\\begin{aligned}
                \\eta_{y,t} &= %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                y_t &= %.1f + \\varepsilon_t
             \\end{aligned}", 
            params_y$alpha(), params_y$phi(), params_y$beta(), i_y$mean()
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                \\eta_{x,t} &= %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\\\
                x_t &= %.1f + \\varepsilon_t
             \\end{aligned}", 
            params_x$alpha(), params_x$phi(), params_x$beta(), i_x$mean()
          )
          
          formula_z <- sprintf(
            "\\begin{aligned}
              Z &\\sim \\mathcal{N}(0, \\Sigma_\\zeta), && \\Sigma_\\zeta =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              E &\\sim \\mathcal{N}(0, \\Sigma_\\varepsilon), && \\Sigma_\\varepsilon =
              \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
             \\end{aligned}",
            innovations$y(), innovations$c_yx(), innovations$x(),
            measurement_errors$y(), measurement_errors$c_yx(), measurement_errors$x()
          )
        }
        if(model() == "TV") {
          formula_y <- sprintf(
            "$$ y_t = \\alpha_{y, t} + \\phi_{y, t} y_{t-1} + \\beta_{y, t} x_{t-1} + \\zeta_t $$"
          )
          
          formula_x <- sprintf(
            "$$ x_t = \\alpha_{x, t} + \\phi_{x, t} x_{t-1} + \\beta_{x, t} y_{t-1} + \\zeta_t $$"
          )
          
          formula_z <- sprintf(
            "\\begin{equation}
              Z \\sim \\mathcal{N}(0, \\Sigma), \\quad
              \\Sigma =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{equation}",
            innovations$y(), innovations$c_yx(), innovations$x()
          )
        }
        
        if(model() == "T") {
          formula_y <- sprintf(
            "\\begin{aligned}
                y_t = 
                &\\begin{cases}
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                \\end{cases} \\\\
                &\\begin{cases}
                \\text{if} \\quad x_{t-1} \\leq %7$.1f \\\\
                \\text{if} \\quad x_{t-1} > %7$.1f 
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta(), tau_y()        
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                &\\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t
                \\end{cases} \\\\
                &\\begin{cases}
                \\text{if} \\quad y_{t-1} \\leq %7$.1f \\\\
                \\text{if} \\quad y_{t-1} > %7$.1f 
                \\end{cases}
             \\end{aligned}",
            params_x$alpha(), params_x$phi(), params_x$beta(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta(), tau_x()
          )
          
          ###############################
          ###############################
          ###############################
          # cov_T = correlation, not covariance!
          # formula_z <- sprintf(
          #   "\\begin{aligned}
          #     Z &\\sim \\mathcal{N}(0, \\Sigma), && \\Sigma =
          #     \\begin{cases}
          #     \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
          #     \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
          #     \\end{cases}
          #    \\end{aligned}",
          #   innovations$y(), cov_T(), innovations$x(),
          #   innovations_2$y(), cov_T(), innovations_2$x()
          # )
          
          formula_z <- sprintf(
            "\\begin{array}{c}
              Z \\sim \\mathcal{N}(0, \\Sigma), \\\\
              \\begin{matrix}
              & & y_1 & & x_1 & & y_2 & & x_2
              \\end{matrix} \\\\
              \\Sigma =
              \\begin{bmatrix} 
              %1$.2f & %5$.2f & %6$.2f  & %7$.2f \\\\
              %5$.2f & %2$.2f & %8$.2f  & %9$.2f \\\\
              %6$.2f & %8$.2f & %3$.2f  & %10$.2f \\\\
              %7$.2f & %9$.2f & %10$.2f & %4$.2f
              \\end{bmatrix}
             \\end{array}",
            innovations$y(), innovations$x(), innovations_2$y(), innovations_2$x(),
            covs()$y1x1, covs()$y1y1, covs()$y1x2,
            covs()$x1y2, covs()$x1x1, covs()$y2x2
          )
        }
        if(model() == "HMM") {
          formula_y <- sprintf(
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                %.1f + \\varepsilon_t \\\\
                %.1f + \\varepsilon_t \\\\
                \\end{cases}
             \\end{aligned}", 
            means_y$mu_1(), means_y$mu_2()
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                %.1f + \\varepsilon_t\\\\
                %.1f + \\varepsilon_t
                \\end{cases}
             \\end{aligned}", 
            means_x$mu_1(), means_x$mu_2()
          )
          
          # formula_z <- sprintf(
          #   "\\begin{aligned}
          #     E_1 &\\sim \\mathcal{N}(0, \\Sigma_1), && \\Sigma_1 =
          #     \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
          #     E_2 &\\sim \\mathcal{N}(0, \\Sigma_2), && \\Sigma_2 =
          #     \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
          #    \\end{aligned}",
          #   measurement_errors$y(), measurement_errors$c_yx(), measurement_errors$x(),
          #   measurement_errors_2$y(), measurement_errors_2$c_yx(), measurement_errors_2$x()
          # )
          formula_z <- sprintf(
            "\\begin{aligned}
              E &\\sim \\mathcal{N}(0, \\Sigma), && \\Sigma =
              \\begin{cases}
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
              \\end{cases}
             \\end{aligned}",
            measurement_errors$y(), measurement_errors$c_yx(), measurement_errors$x(),
            measurement_errors_2$y(), measurement_errors_2$c_yx(), measurement_errors_2$x()
          )
        }
        if(model() == "MS") {
          formula_y <- sprintf(
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_t \\\\
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta()           
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_t
                \\end{cases}
             \\end{aligned}",
            params_x$alpha(), params_x$phi(), params_x$beta(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta()
          )
          
          # formula_z <- sprintf(
          #   "\\begin{aligned}
          #     Z_1 &\\sim \\mathcal{N}(0, \\Sigma_1), && \\Sigma_1 =
          #     \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
          #     Z_2 &\\sim \\mathcal{N}(0, \\Sigma_2), && \\Sigma_2 =
          #     \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
          #    \\end{aligned}",
          #   innovations$y(), innovations$c_yx(), innovations$x(),
          #   innovations_2$y(), innovations_2$c_yx(), innovations_2$x()
          # )
          
          formula_z <- sprintf(
            "\\begin{aligned}
              Z &\\sim \\mathcal{N}(0, \\Sigma), && \\Sigma =
              \\begin{cases}
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
              \\end{cases}
             \\end{aligned}",
            innovations$y(), innovations$c_yx(), innovations$x(),
            innovations_2$y(), innovations_2$c_yx(), innovations_2$x()
          )
        }
        
        return(list(formula_y = formula_y, formula_x = formula_x, formula_z = formula_z))
      })
      
      output$formula_y <- renderUI({
        withMathJax(tex()$formula_y)
      })
      output$formula_x <- renderUI({
        withMathJax(tex()$formula_x)
      })
      output$formula_z <- renderUI({
        withMathJax(tex()$formula_z)
      })
    }
  )
}