symVARS <- function(occasions, burnin, 
                    type, probs, 
                    params_y, params_x, # list(alpha =, phi = , beta =, k = )
                    innovations = c(.5, .3, .3, .5), # too high?
                    longformat = T){
  
  source("modelFunctions.R", local = T)
  
  o_bi <- occasions + burnin # add burnin
  
  # COVARYING INNOVATIONS
  ifelse(!is.list(innovations), covs <- innovations, covs <- innovations[[1]]) # get vector indicating (co)variances
  m <- matrix(covs, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))   # create covariance matrix
  z <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m))          # simulate innovations for each occasion
  
  if(type == "Markov-swithing" | type == "HMM"){ # Markov-switching and HMM models have different variances between regimes
    ifelse(!is.list(innovations), covs2 <- innovations, covs2 <- innovations[[2]])
    m2 <- matrix(covs2, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))
    z2 <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m2))
    z  <- list(z, z2)
  }

  if(type == "VAR"){
    dat <- VAR1(o_bi, params_y, params_x, z)
  }
  if(type == "time-varying"){
    dat <- TVVAR1(o_bi, burnin, params_y, params_x, z)
  }
  if(type == "threshold"){
    dat <- TVAR1(o_bi, params_y, params_x, z)
  }
  if(type == "Markov-switching"){
    dat <- MSVAR1(o_bi, params_y, params_x, probs, z)
  }
  if(type == "HMM"){
    dat <- HMM1(o_bi, params_y, params_x, probs, z)
  }

  dat <- dat[1:occasions + burnin, ] # remove burnin
  
  if(longformat){ # longformat option for plotting
    behavior  <- c(dat$y, dat$x)
    partner   <- rep(c("y", "x"), each = occasions)
    t_total   <- rep(1:occasions, 2)
    dat       <- data.frame(partner = partner, behavior = behavior, t = t_total)
  } 
  return(dat)
}