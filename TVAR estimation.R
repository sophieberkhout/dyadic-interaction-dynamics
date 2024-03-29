# TVAR estimation code adapted from Madhyastha et al (2011)
# enter df with column "y" as dependent and column "x" as threshold variable
# to estimate the TVAR model from our paper, use default
# to estimate the models from Madhyastha et al (2011) use
# for bilinear model: alpha = 1, phi = 1, beta = 2
# for ojive model:    alpha = 2, phi = 1, beta = 0
# for combi model:    alpha = 2, phi = 1, beta = 2
estTAR <- function(data, alpha = 2, phi = 2, beta = 2, minObs = 10) {

  dat <- data
  
  dat$yL    <- c(NA, dat[-nrow(dat), "y"])   # lag y variable
  dat$xL    <- c(NA, dat[-nrow(dat), "x"])   # lag x variable (thresholding variable)
  dat       <- na.omit(dat)        # remove first observations with NAs
  n         <- nrow(dat)           # total number of observations
  dat$dummy <- rep(1, n)           # dummy variable for the intercept
  
  dat <- dat[order(dat$xL), ]   # order the data based on x_{t-1}
  
  minObs <- round(n / minObs)   # minimum % of observations in regime 1
  maxObs <- n - minObs          # maximum % of observations in regime 1
  nObs   <- minObs:maxObs       # all possible number of observations in regime 1
  nIter  <- length(nObs)        # number of iterations
  
  tau <- dat$xL[nObs] # get all possible threshold values (based on x_t-1)
  
  sav <- vector(mode = "list", length = nIter) # empty list to store results
  ll  <- numeric(nIter) # empty vector to store log likelihood

  # n   <- nrow(dat) # total number of observations without NAs
  
  # create formula for the model 
  # (dependent on which parameter is regime-switching)
  dep <- "y"
  
  f     <- "~ 0"
  if(alpha == 1) {
    f_alpha <- "dummy"
    c_alpha <- "alpha"
  } else {
    f_alpha <- "dummy_1 + dummy_2"
    c_alpha <- c("alpha_1", "alpha_2")
  }
  if(phi == 1) {
    f_phi <- "yL" 
    c_phi <- "phi"
  } else {
    f_phi <- "yL_1 + yL_2"
    c_phi <- c("phi_1", "phi_2")
  }
  if(beta == 1) {
    f_beta <- "xL"
    c_beta <- "beta"
  } else {
    f_beta <- "xL_1 + xL_2"
    c_beta <- c("beta_1", "beta_2")
  }
  
  # paste formula together
  # check if xL should be in the formula
  if(beta != 0)
    f <- as.formula(paste(dep, f, "+", f_alpha, "+", f_phi, "+", f_beta)) else
      f <- as.formula(paste(dep, f, "+", f_alpha, "+", f_phi))

  for(i in 1:nIter) {
    
    # number of observations in regime 1 and regime 2
    n_1 <- nObs[i]
    n_2 <- n - n_1
    
    # which observations are in regimes
    reg_1 <- dat$xL <= tau[i]
    reg_2 <- dat$xL > tau[i]
    
    # dummy variable for intercept per regime
    dat$dummy_1 <- as.numeric(reg_1)
    dat$dummy_2 <- as.numeric(reg_2)
    
    # regime specific variable for lag y
    dat$yL_1 <- reg_1 * dat$yL
    dat$yL_2 <- reg_2 * dat$yL
    
    # regime specific variable for lag x
    dat$xL_1 <- reg_1 * dat$xL
    dat$xL_2 <- reg_2 * dat$xL
    
    # estimate model without intercept but with dummy variables above
    res <- lm(f, data = dat)
    
    # calculate sums of squares residuals per regime
    ssr_1 <- sum(resid(res)[1:n_1] ^ 2)
    ssr_2 <- sum(resid(res)[(n_1 + 1):n] ^ 2)
    
    # calculate log likelihood based on the regimes
    ll[i] <- n_1 * log(ssr_1 / n_1) + n_2 * log(ssr_2 / n_2)
    
    # save all results
    sav[[i]] <- res
  }
  
  # check which model fit best (lowest log likelihood)
  j <- which.min(ll)
  
  # extract estimated coefficients
  coefs <- sav[[j]]$coefficients
  if(beta == 0) {
    names(coefs) <- c(c_alpha, c_phi)
  } else {
    names(coefs) <- c(c_alpha, c_phi, c_beta)
  }
  
  # variable indicating which regime is active per measurement occasion
  reg <- as.numeric(data[, 2] < tau[j]) + 1
  
  # save predicted values
  pred <- numeric()
  pred[as.numeric(names(sav[[j]]$fitted.values))] <- sav[[j]]$fitted.values
  
  return(list(threshold = tau[j], coefficients = coefs, result = sav[[j]], 
              regime = reg, predicted = pred))
}

