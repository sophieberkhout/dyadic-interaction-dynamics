formulaModelUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_model"))
  )
}

formulaModelServer <- function(id, model, columnName){
  moduleServer(
    id,
    function(input, output, server){
      
      tex <- reactive({
        if(model() == "VAR") {
          formula_y <- "$$ y_t = \\alpha_y + \\phi_y y_{t-1} + \\beta_y x_{t-1} + \\zeta_{y, t} $$" 
          
          formula_x <- "$$ x_t = \\alpha_x + \\phi_x x_{t-1} + \\beta_y y_{t-1} + \\zeta_{x, t} $$"
          
          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }
        
        if(model() == "L") {
          formula_y <-
            "\\begin{aligned}
                \\eta_{t} &= \\alpha_y + \\phi_y \\eta_{t-1} + \\beta_y \\xi_{t-1} + \\zeta_{y, t} \\\\
                y_t &= \\mu_y + \\eta_{t} + \\varepsilon_{y, t}
             \\end{aligned}"
          
          formula_x <-
            "\\begin{aligned}
                \\xi_{t} &= \\alpha_x + \\phi_x \\xi_{t-1} + \\beta_x \\eta_{t-1} + \\zeta_{x, t} \\\\
                x_t &= \\mu_x + \\xi_{t} + \\varepsilon_{x, t}
             \\end{aligned}"
          
          formula_z <-
            "\\begin{aligned}
              Z &\\sim \\mathcal{N}(0, \\Sigma_\\zeta) \\\\
              E &\\sim \\mathcal{N}(0, \\Sigma_\\varepsilon)
             \\end{aligned}"
        }
        
        if(model() == "TV") {
          formula_y <- "$$ y_t = \\alpha_{y, t} + \\phi_{y, t} y_{t-1} + \\beta_{y, t} x_{t-1} + \\zeta_{y, t} $$"
          
          formula_x <- "$$ x_t = \\alpha_{x, t} + \\phi_{x, t} x_{t-1} + \\beta_{x, t} y_{t-1} + \\zeta_{x, t} $$"
          
          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }
        
        if(model() == "T") {
          formula_y <-
            "$$
             \\begin{aligned}
                y_t = 
                &\\begin{cases}
                \\alpha_{y, 1} + \\phi_{y, 1} y_{t-1} + \\beta_{y, 1} x_{t-1} + \\zeta_{y, t} \\enspace \\text{if} \\: x_{t-1} \\leq \\tau_y \\\\
                \\alpha_{y, 2} + \\phi_{y, 2} y_{t-1} + \\beta_{y, 2} x_{t-1} + \\zeta_{y, t} \\enspace \\text{if} \\: x_{t-1} > \\tau_y
                \\end{cases}
             \\end{aligned}
             $$"
          
          formula_x <-
            "$$
              \\begin{aligned}
                x_t =
                &\\begin{cases}
                \\alpha_{x, 1} + \\phi_{x, 1} x_{t-1} + \\beta_{x, 1} y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} \\leq \\tau_x \\\\
                \\alpha_{x, 2} + \\phi_{x, 2} x_{t-1} + \\beta_{x, 2} y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} > \\tau_x
                \\end{cases}
             \\end{aligned}
             $$"
          
          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
          
        }
        if(model() == "HMM") {
          formula_y <-
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                \\mu_{y, 1} + \\varepsilon_{y, t} \\\\
                \\mu_{y, 2} + \\varepsilon_{y, t}
                \\end{cases}
             \\end{aligned}"
          
          formula_x <-
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                \\mu_{x, 1} + \\varepsilon_{x, t} \\\\
                \\mu_{x, 2} + \\varepsilon_{x, t}
                \\end{cases}
             \\end{aligned}"
          
          formula_z <- "$$ E \\sim \\mathcal{N}(0, \\Sigma) $$"
        }
        if(model() == "MS") {
          formula_y <-
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                \\alpha_{y, 1} + \\phi_{y, 1} y_{t-1} + \\beta_{y, 1} x_{t-1} + \\zeta_{y, t} \\\\
                \\alpha_{y, 2} + \\phi_{y, 2} y_{t-1} + \\beta_{y, 2} x_{t-1} + \\zeta_{y, t} \\\\
                \\end{cases}
             \\end{aligned}"
          
          formula_x <-
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                \\alpha_{x, 1} + \\phi_{x, 1} x_{t-1} + \\beta_{x, 1} y_{t-1} + \\zeta_{x, t} \\\\
                \\alpha_{x, 2} + \\phi_{x, 2} x_{t-1} + \\beta_{x, 2} y_{t-1} + \\zeta_{x, t}
                \\end{cases}
             \\end{aligned}"
          
          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }
        
        if(columnName == "y") return(formula_y)
        if(columnName == "x") return(formula_x)
        if(columnName == "z") return(formula_z)
        
      })
      
      output$formula_model <- renderUI({
        withMathJax(tex())
      })
    }
  )
}

# formulaUI <- function(id){
#   ns <- NS(id)
#   fluidRow(
#     column(3, offset = 3, uiOutput(ns("formula_y"))),
#     column(3, uiOutput(ns("formula_x"))),
#     column(3, uiOutput(ns("formula_z")))
#   )
# }

formulaUI_y <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_y"))
  )
}

formulaUI_x <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_x"))
  )
}

formulaUI_z <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_z"))
  )
}

tvFormula <- function(tv) {
  if(tv$tv() == "stable") {
    tv_f <- sprintf("%.1f", tv$stable())
  } else {
    if(tv$change() == "linear") {
      tv_f <- sprintf("%.1f + %.3f t", tv$int(), tv$slope())
    } else if(tv$change() == "sine") {
      tv_f <- sprintf("%.1f \\: sin(2\\pi \\frac{%.1f}{%.0f} (t + %.1f)) + %.1f",
                      tv$amp(), tv$freq(), tv$t(), tv$phase(), tv$dev())
    }
  }
}

formulaServer <- function(id, model, 
                          params_y, params_x, params_y_2, params_x_2,
                          tau_y, tau_x, cov_T,
                          i_y, i_x,
                          means_y, means_x,
                          innovations, innovations_2,
                          measurement_errors, measurement_errors_2,
                          tv){
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
      
      tvPars <- reactive({
        f_alpha_y <- tvFormula(tv$alpha_y)
        f_phi_y   <- tvFormula(tv$phi_y)
        f_beta_y  <- tvFormula(tv$beta_y)
        
        f_alpha_x <- tvFormula(tv$alpha_x)
        f_phi_x   <- tvFormula(tv$phi_x)
        f_beta_x  <- tvFormula(tv$beta_x)
        
        return(
          list(
            alpha_y = f_alpha_y,
            phi_y   = f_phi_y,
            beta_y  = f_beta_y,
            alpha_x = f_alpha_x,
            phi_x   = f_phi_x,
            beta_x  = f_beta_x
          )
        )
      })
      
      tex <- reactive({
        if(model() == "VAR") {
          formula_y <- sprintf(
            "$$ y_t = %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_{y, t} $$", 
            params_y$alpha(), params_y$phi(), params_y$beta()
          )
          
          formula_x <- sprintf(
            "$$ x_t = %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_{x, t} $$", 
            params_x$alpha(), params_x$phi(), params_x$beta()
          )
          
          formula_z <- sprintf(
            "\\begin{equation}
              \\Sigma =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{equation}",
            innovations$y(), innovations$c_yx(), innovations$x()
          )
        }
        if(model() == "L") {
          formula_y <- sprintf(
            "\\begin{aligned}
                \\eta_{t} &= %.1f + %.1f \\eta_{t-1} + %.1f \\xi_{t-1} + \\zeta_{y, t} \\\\
                y_t &= %.1f + \\eta_{t} + \\varepsilon_t
             \\end{aligned}", 
            params_y$alpha(), params_y$phi(), params_y$beta(), i_y$mean()
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                \\xi_{t} &= %.1f + %.1f \\xi_{t-1} + %.1f \\eta_{t-1} + \\zeta_{x, t} \\\\
                x_t &= %.1f + \\xi_{t} + \\varepsilon_t
             \\end{aligned}", 
            params_x$alpha(), params_x$phi(), params_x$beta(), i_x$mean()
          )
          
          formula_z <- sprintf(
            "\\begin{aligned}
              \\Sigma_\\zeta &=
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\Sigma_\\varepsilon &=
              \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
             \\end{aligned}",
            innovations$y(), innovations$c_yx(), innovations$x(),
            measurement_errors$y(), measurement_errors$c_yx(), measurement_errors$x()
          )
        }
        if(model() == "TV") {
          formula_y <- sprintf(
            "\\begin{align}
              \\alpha_{y, t} &= %s \\\\
              \\phi_{y, t} &= %s \\\\
              \\beta_{y, t} &= %s
             \\end{align}",
            tvPars()$alpha_y, tvPars()$phi_y, tvPars()$beta_y
          )
          
          formula_x <- sprintf(
            "\\begin{align}
              \\alpha_{x, t} &= %s \\\\
              \\phi_{x, t} &= %s \\\\
              \\beta_{x, t} &= %s
             \\end{align}",
            tvPars()$alpha_x, tvPars()$phi_x, tvPars()$beta_x
          )
          
          formula_z <- sprintf(
            "\\begin{equation}
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
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_{y, t} \\enspace \\text{if} \\: x_{t-1} \\leq %7$.1f \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_{y, t} \\enspace \\text{if} \\: x_{t-1} > %7$.1f
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta(), tau_y()        
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                &\\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} \\leq %7$.1f \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} > %7$.1f
                \\end{cases}
             \\end{aligned}",
            params_x$alpha(), params_x$phi(), params_x$beta(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta(), tau_x()
          )
          
          formula_z <- sprintf(
            "$$ \\Sigma =
              \\begin{align}
              &\\begin{matrix}
              y1 \\\\
              x1
              \\end{matrix}
              \\begin{bmatrix} 
              %1$.2f & %5$.2f  \\\\
              %5$.2f & %2$.2f 
              \\end{bmatrix}
              \\begin{matrix}
              y1 \\\\
              x2
              \\end{matrix}
              \\begin{bmatrix} 
              %1$.2f & %6$.2f  \\\\
              %6$.2f & %4$.2f 
              \\end{bmatrix} \\\\
              &\\begin{matrix}
              y2 \\\\
              x1
              \\end{matrix}
              \\begin{bmatrix} 
              %3$.2f & %7$.2f  \\\\
              %7$.2f & %2$.2f 
              \\end{bmatrix}
              \\begin{matrix}
              y2 \\\\
              x2
              \\end{matrix}
              \\begin{bmatrix} 
              %3$.2f & %8$.2f  \\\\
              %8$.2f & %4$.2f 
              \\end{bmatrix}
             \\end{align}
             $$",
            innovations$y(), innovations$x(), innovations_2$y(), innovations_2$x(),
            covs()$y1x1, covs()$y1x2, covs()$x1y2, covs()$y2x2
          )
        }
        if(model() == "HMM") {
          formula_y <- sprintf(
            "\\begin{aligned}
                y_t =
                \\begin{cases}
                %.1f + \\varepsilon_{y, t} \\\\
                %.1f + \\varepsilon_{y, t} \\\\
                \\end{cases}
             \\end{aligned}", 
            means_y$mu_1(), means_y$mu_2()
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                %.1f + \\varepsilon_{x, t}\\\\
                %.1f + \\varepsilon_{x, t}
                \\end{cases}
             \\end{aligned}", 
            means_x$mu_1(), means_x$mu_2()
          )
          
          formula_z <- sprintf(
            "\\begin{aligned}
              \\Sigma =
              \\begin{cases}
              \\begin{matrix}
              y1 \\\\
              x1
              \\end{matrix}
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\begin{matrix}
              y2 \\\\
              x2
              \\end{matrix}
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
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_{y, t} \\\\
                %.1f + %.1f y_{t-1} + %.1f x_{t-1} + \\zeta_{y, t} \\\\
                \\end{cases}
             \\end{aligned}",
            params_y$alpha(), params_y$phi(), params_y$beta(),
            params_y_2$alpha(), params_y_2$phi(), params_y_2$beta()           
          )
          
          formula_x <- sprintf(
            "\\begin{aligned}
                x_t =
                \\begin{cases}
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_{x. t} \\\\
                %.1f + %.1f x_{t-1} + %.1f y_{t-1} + \\zeta_{x, t}
                \\end{cases}
             \\end{aligned}",
            params_x$alpha(), params_x$phi(), params_x$beta(),
            params_x_2$alpha(), params_x_2$phi(), params_x_2$beta()
          )
          
          formula_z <- sprintf(
            "\\begin{aligned}
              \\Sigma =
              \\begin{cases}
              \\begin{matrix}
              y1 \\\\
              x1
              \\end{matrix}
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\begin{matrix}
              y2 \\\\
              x2
              \\end{matrix}
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