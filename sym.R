symVARS <- function(occasions, burnin, 
                    type, probs, 
                    params_y, params_x, # list(alpha =, phi = , beta =, k = )
                    longformat = T){
  
  source("modelFunctions.R", local = T)
  
  o_bi <- occasions + burnin

  m <- matrix(c(.5, .3, .3, .5), 2, 2, dimnames = list(c("y", "x"), c("y", "x"))) # too high?
  z <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m))
  if(type == "MSVAR" | type == "HMM"){
    m2 <- matrix(c(.3, .1, .1, .3), 2, 2, dimnames = list(c("y", "x"), c("y", "x")))
    z2 <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m2))
    z <- list(z, z2)
  }

  if(type == "VAR"){
    dat <- VAR1(o_bi, params_y, params_x, z)
  }
  if(type == "TV-VAR"){
    dat <- TVVAR1(o_bi, burnin, params_y, params_x, z)
  }
  if(type == "TVAR"){
    dat <- TVAR1(o_bi, params_y, params_x, z)
  }
  if(type == "MSVAR"){
    dat <- MSVAR1(o_bi, params_y, params_x, probs, z)
  }
  if(type == "HMM"){
    dat <- HMM1(o_bi, params_y, params_x, probs, z)
  }

  dat <- dat[1:occasions + burnin, ]
  
  if(longformat){
    behavior <- c(dat$y, dat$x)
    partner <- rep(c("y", "x"), each = occasions)
    t_total <- rep(1:occasions, 2)
    dat <- data.frame(partner = partner, behavior = behavior, t = t_total)
  } 
  return(dat)
}