formulaModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_model"))
  )
}

formulaModelServer <- function(id, model, columnName) {
  moduleServer(
    id,
    function(input, output, server) {
      tex <- reactive({
        if (model() == "VAR") {
          formula_y <-
            "$$ y_t = \\alpha_y + \\phi_y y_{t-1} + \\beta_y x_{t-1} + \\zeta_{y, t} $$"

          formula_x <- "$$ x_t = \\alpha_x + \\phi_x x_{t-1} + \\beta_x y_{t-1} + \\zeta_{x, t} $$"

          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }

        if (model() == "L") {
          formula_y <-
            "\\begin{aligned}
                \\eta_{t} &= \\alpha_y + \\phi_y \\eta_{t-1} + \\beta_y \\xi_{t-1} + \\zeta_{y, t} \\\\
                y_t &= \\nu_y + \\eta_{t} + \\varepsilon_{y, t}
             \\end{aligned}"

          formula_x <-
            "\\begin{aligned}
                \\xi_{t} &= \\alpha_x + \\phi_x \\xi_{t-1} + \\beta_x \\eta_{t-1} + \\zeta_{x, t} \\\\
                x_t &= \\nu_x + \\xi_{t} + \\varepsilon_{x, t}
             \\end{aligned}"

          formula_z <-
            "\\begin{aligned}
              Z &\\sim \\mathcal{N}(0, \\Sigma_\\zeta) \\\\
              E &\\sim \\mathcal{N}(0, \\Sigma_\\varepsilon)
             \\end{aligned}"
        }

        if (model() == "TV") {
          formula_y <- "$$ y_t = \\alpha_{y, t} + \\phi_{y, t} y_{t-1} + \\beta_{y, t} x_{t-1} + \\zeta_{y, t} $$"

          formula_x <- "$$ x_t = \\alpha_{x, t} + \\phi_{x, t} x_{t-1} + \\beta_{x, t} y_{t-1} + \\zeta_{x, t} $$"

          formula_z <- "$$ Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }

        if (model() == "T") {
          formula_y <-
            "$$
            \\small
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
            \\small
              \\begin{aligned}
                x_t =
                &\\begin{cases}
                \\alpha_{x, 1} + \\phi_{x, 1} x_{t-1} + \\beta_{x, 1} y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} \\leq \\tau_x \\\\
                \\alpha_{x, 2} + \\phi_{x, 2} x_{t-1} + \\beta_{x, 2} y_{t-1} + \\zeta_{x, t} \\enspace \\text{if} \\: y_{t-1} > \\tau_x
                \\end{cases}
             \\end{aligned}
             $$"

          formula_z <- "$$ \\small Z \\sim \\mathcal{N}(0, \\Sigma) $$"
        }
        if (model() == "HMM") {
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
        if (model() == "MS") {
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

        if (columnName == "y") {
          return(formula_y)
        }
        if (columnName == "x") {
          return(formula_x)
        }
        if (columnName == "z") {
          return(formula_z)
        }
      })

      output$formula_model <- renderUI({
        withMathJax(tex())
      })
    }
  )
}

formulaUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula"))
  )
}

formulaUI_z <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("formula_z"))
  )
}

tvFormula <- function(tv) {
  if (tv$tv() == "stable") {
    tv_f <- sprintf("%.1f", tv$stable())
  } else {
    if (tv$change() == "linear") {
      tv_f <- sprintf("%.1f + %.3f t", tv$int(), tv$slope())
    } else if (tv$change() == "sine") {
      tv_f <- sprintf(
        "%.1f \\: sin(2\\pi \\frac{%.1f}{%.0f} (t + %.1f)) + %.1f",
        tv$amp(), tv$freq(), tv$t(), tv$phase(), tv$dev()
      )
    }
  }
}

formulaServer_z <- function(id, model,
                            cov_T,
                            errors) {
  moduleServer(
    id,
    function(input, output, server) {
      innovations <- reactive({
        errors()$dynamic
      })

      measurement_errors <- reactive({
        errors()$measurement
      })

      covs <- reactive({
        v <- c(
          innovations()[[1]][1], innovations()[[1]][3],
          innovations()[[2]][1], innovations()[[2]][3]
        )
        v <- sqrt(v)
        y1x1 <- v[1] * v[2] * cov_T()
        y1y1 <- v[1] * v[3] * cov_T()
        y1x2 <- v[1] * v[4] * cov_T()
        x1y2 <- v[2] * v[3] * cov_T()
        x1x1 <- v[2] * v[4] * cov_T()
        y2x2 <- v[3] * v[4] * cov_T()

        return(list(
          y1x1 = y1x1, y1y1 = y1y1, y1x2 = y1x2,
          x1y2 = x1y2, x1x1 = x1x1, y2x2 = y2x2
        ))
      })

      tex <- reactive({
        if (model() == "VAR") {
          formula_z <- sprintf(
            "\\begin{equation}
              \\Sigma =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{equation}",
            innovations()[1], innovations()[2], innovations()[3]
          )
        }
        if (model() == "L") {
          formula_z <- sprintf(
            "\\begin{aligned}
              \\Sigma_\\zeta &=
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix} \\\\
              \\Sigma_\\varepsilon &=
              \\begin{bmatrix} %4$.2f & %5$.2f \\\\ %5$.2f & %6$.2f \\end{bmatrix}
             \\end{aligned}",
            innovations()[1], innovations()[2], innovations()[3],
            measurement_errors()[1], measurement_errors()[2], 
            measurement_errors()[3]
          )
        }
        if (model() == "TV") {
          formula_z <- sprintf(
            "\\begin{equation}
              \\Sigma =
              \\begin{bmatrix} %1$.2f & %2$.2f \\\\ %2$.2f & %3$.2f \\end{bmatrix}
             \\end{equation}",
            innovations()[1], innovations()[2], innovations()[3]
          )
        }

        if (model() == "T") {
          formula_z <- sprintf(
            "$$ \\small
            \\Sigma =
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
            innovations()[[1]][1], innovations()[[1]][3],
            innovations()[[2]][1], innovations()[[1]][3],
            covs()$y1x1, covs()$y1x2, covs()$x1y2, covs()$y2x2
          )
        }
        if (model() == "HMM") {
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
            measurement_errors()[[1]][1], measurement_errors()[[1]][2],
            measurement_errors()[[1]][3],
            measurement_errors()[[2]][1], measurement_errors()[[2]][2],
            measurement_errors()[[2]][3]
          )
        }
        if (model() == "MS") {
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
            innovations()[[1]][1], innovations()[[1]][2], innovations()[[1]][3],
            innovations()[[2]][1], innovations()[[2]][2], innovations()[[2]][3]
          )
        }

        return(formula_z)
      })

      output$formula_z <- renderUI({
        withMathJax(tex())
      })
    }
  )
}

formulaServer <- function(id, model, partner, params, tau) {
  moduleServer(
    id,
    function(input, output, server) {
      coefs <- reactive({
        params()$coefs
      })

      indicator <- reactive({
        params()$indicator
      })

      tvPars <- reactive({
        f_alpha <- tvFormula(params()$tv$alpha)
        f_phi <- tvFormula(params()$tv$phi)
        f_beta <- tvFormula(params()$tv$beta)

        return(
          list(
            alpha = f_alpha,
            phi   = f_phi,
            beta  = f_beta
          )
        )
      })

      tex <- reactive({
        other <- ifelse(partner == "y", "x", "y")
        if (model() == "VAR") {
          formula <- sprintf(
            "$$ %1$s_t = %3$.1f + %4$.1f %1$s_{t-1} + %5$.1f %2$s_{t-1} + \\zeta_{%1$s, t} $$",
            partner, other, coefs()$alpha, coefs()$phi, coefs()$beta
          )
        }
        if (model() == "L") {
          latentPartner <- ifelse(partner == "y", "eta", "xi")
          latentOther <- ifelse(partner == "y", "xi", "eta")
          formula <- sprintf(
            "\\begin{aligned}
                \\%1$s_{t} &= %4$.1f + %5$.1f \\%1$s_{t-1} + %6$.1f \\%2$s_{t-1} + \\zeta_{%3$s, t} \\\\
                %3$s_t &= %7$.1f + \\%1$s_{t} + \\varepsilon_{%3$s, t}
             \\end{aligned}",
            latentPartner, latentOther, partner,
            coefs()$alpha, coefs()$phi, coefs()$beta, indicator()$m
          )
        }
        if (model() == "TV") {
          formula <- sprintf(
            "\\begin{align}
              \\alpha_{%1$s, t} &= %2$s \\\\
              \\phi_{%1$s, t} &= %3$s \\\\
              \\beta_{%1$s, t} &= %4$s
             \\end{align}",
            partner, tvPars()$alpha, tvPars()$phi, tvPars()$beta
          )
        }

        if (model() == "T") {
          formula <- sprintf(
            "$$ \\small
            \\begin{aligned}
                %8$s_t =
                &\\begin{cases}
                %1$.1f + %2$.1f %8$s_{t-1} + %3$.1f %9$s_{t-1} + \\zeta_{%8$s, t} \\enspace \\text{if} \\: %9$s_{t-1} \\leq %7$.1f \\\\
                %4$.1f + %5$.1f %8$s_{t-1} + %6$.1f %9$s_{t-1} + \\zeta_{%8$s, t} \\enspace \\text{if} \\: %9$s_{t-1} > %7$.1f
                \\end{cases}
             \\end{aligned} $$",
            coefs()$alpha[1], coefs()$phi[1], coefs()$beta[1],
            coefs()$alpha[2], coefs()$phi[2], coefs()$beta[2], tau(),
            partner, other
          )
        }
        if (model() == "HMM") {
          formula <- sprintf(
            "\\begin{aligned}
                %1$s_t =
                \\begin{cases}
                %2$.1f + \\varepsilon_{%1$s, t} \\\\
                %3$.1f + \\varepsilon_{%1$s, t} \\\\
                \\end{cases}
             \\end{aligned}",
            partner, coefs()$mu[1], coefs()$mu[2]
          )
        }
        if (model() == "MS") {
          formula <- sprintf(
            "\\begin{aligned}
                %1$s_t =
                \\begin{cases}
                %2$.1f + %3$.1f y_{t-1} + %4$.1f x_{t-1} + \\zeta_{%1$s, t} \\\\
                %5$.1f + %6$.1f y_{t-1} + %7$.1f x_{t-1} + \\zeta_{%1$s, t} \\\\
                \\end{cases}
             \\end{aligned}",
            partner, coefs()$alpha[1], coefs()$phi[1], coefs()$beta[1],
            coefs()$alpha[2], coefs()$phi[2], coefs()$beta[2]
          )
        }

        return(formula)
      })

      output$formula <- renderUI({
        withMathJax(tex())
      })
    }
  )
}