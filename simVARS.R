simVARS <- function(occasions, burnin, 
                    type, probs, 
                    params_y, params_x, # list(alpha =, phi = , beta =, tau = )
                    innovations = c(.5, .3, .3, .5), # too high?
                    longformat = T){
  
  source("modelFunctions.R", local = T)
  
  o_bi <- occasions + burnin # add burnin
  
  # COVARYING INNOVATIONS
  ifelse(!is.list(innovations), covs <- innovations, covs <- innovations[[1]]) # get vector indicating (co)variances
  m <- matrix(covs, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))   # create covariance matrix
  z <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m))          # simulate innovations for each occasion
  
  if(type == "MS" | type == "HMM"){ # Markov-switching and HMM models have different variances between regimes
    ifelse(!is.list(innovations), covs2 <- innovations, covs2 <- innovations[[2]])
    m2 <- matrix(covs2, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))
    z2 <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m2))
    z  <- list(z, z2)
  }

  if(type == "VAR"){
    dat <- VAR1(o_bi, params_y, params_x, z)
  }
  if(type == "TV"){
    dat <- TVVAR1(o_bi, burnin, params_y, params_x, z)
  }
  if(type == "T"){
    dat <- TVAR1(o_bi, params_y, params_x, z, longformat)
  }
  if(type == "MS"){
    dat <- MSVAR1(o_bi, params_y, params_x, probs, z)
  }
  if(type == "HMM"){
    dat <- HMM1(o_bi, params_y, params_x, probs, z)
  }

  dat <- dat[1:occasions + burnin, ] # remove burnin
  dat$t <- 1:occasions
  
  if(longformat){ # longformat option for plotting
    dat$t <- 1:occasions
    if(type != "T") {
      dat <- tidyr::gather(dat, partner, behavior, x, y)
    } else {
      dat <- reshape(dat, idvar = "t", 
                     varying = stringr::str_subset(names(dat), ("y|x")), 
                     direction = "long", sep = "_", timevar = "partner")
      dat$regime <- as.factor(dat$regime)
    }
  }
  row.names(dat) <- NULL
  return(dat)
}

## Time-varying
change_linear <- function(from, to, t, burnin = NULL){
  x <- seq(from, to, length.out = t)
  return(x)
}

change_sine <- function(amplitude, freq, phase, deviation, t){
  n <- 1:t
  x <- amplitude  * sin(2 * pi * (freq/t) * n + phase) + deviation
  return(x)
}
