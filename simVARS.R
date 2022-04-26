source("modelFunctions.R")

simVARS <- function(occasions, burnin, 
                    type, probs, 
                    params_y, params_x, # list(alpha =, phi = , beta =, tau = ),
                    indicators_y = NULL, indicators_x = NULL, errors = c(.1, .03, .1),
                    innovations = c(.1, .03, .1),
                    longformat = T){
  
  o_bi <- occasions + burnin # add burnin
  
  # generate covarying innovations
  if(type != "T" | !is.list(innovations)){
    ifelse(!is.list(innovations), covs <- innovations, covs <- innovations[[1]]) # get vector indicating (co)variances
    covs <- append(covs, covs[2], after = 2)
    m <- matrix(covs, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))   # create covariance matrix
    z <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m))          # simulate innovations for each occasion
  }
  
  # covarying innovations MS-VAR and HMM models second regime
  if(type == "MS" | type == "HMM"){
    if(type == "MS") er <- innovations else er <- errors
    ifelse(!is.list(er), covs2 <- er, covs2 <- er[[2]])
    covs2 <- append(covs2, covs2[2], after = 2)
    m2 <- matrix(covs2, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))
    z2 <- as.data.frame(MASS::mvrnorm(o_bi, c(0, 0), Sigma = m2))
    z  <- list(z, z2)
  }
  
  # covarying innovations for TVAR model
  if(type == "T"){
    if(is.list(innovations)){
      v <- c(innovations[[1]][1], innovations[[1]][3], 
             innovations[[2]][1], innovations[[2]][3])
      m <- diag(v) # variances for two regimes
      v <- sqrt(v)
      c <- innovations[[1]][2] # correlation
      # this calculates covariances that gives 
      # all regime and variable combinations the same correlation
      m[1, 2:4] <- v[1] * v[-1] * c
      m[2, 3:4] <- v[2] * v[3:4] * c
      m[3, 4]   <- v[3] * v[4] * c
      m[lower.tri(m)] <- t(m)[lower.tri(m)]
      z_both <- MASS::mvrnorm(o_bi, rep(0, 4), m)
      z <- as.data.frame(z_both[, 1:2])
      z2 <- as.data.frame(z_both[, 3:4])
      names(z) <- names(z2) <- c("y", "x")
      z <- list(z, z2)
    }
  }

  # generate data for different models
  if(type == "VAR"){
    dat <- VAR1(o_bi, params_y, params_x, z)
  }
  if(type == "L"){
    if(is.null(indicators_y) | is.null(indicators_x)) stop("You have to specify indicators for x and y.")
    errors <- append(errors, errors[2], after = 2) # add covariance twice
    dat <- LVAR1(o_bi, params_y, params_x, indicators_y, indicators_x, errors, z)
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
  dat$t <- 1:occasions               # add t column for measurement occasion
  
  # longformat option for plotting
  if(longformat){
    if(type != "T") {
      partners <- c("y", "x")
      
      # if LVAR has multiple indicators
      if(type == "L" & (length(indicators_x[[1]]) > 1 | length(indicators_y[[1]]) > 1)){
        partners <- grep("_", names(dat), value = T)
      }
      
      dat <- reshape(dat, varying = partners, 
                     v.names = "value", timevar = "partner", 
                     idvar = "t", times = partners,
                     direction = "long", sep = "_")
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

## time-varying parameters
change_linear <- function(int, slope, t){
  n <- 1:t
  x <- int + slope * n
  return(x)
}

change_sine <- function(amplitude, freq, phase, deviation, t){
  n <- 1:t
  x <- amplitude  * sin(2 * pi * (freq/t) * (n + phase)) + deviation
  return(x)
}
