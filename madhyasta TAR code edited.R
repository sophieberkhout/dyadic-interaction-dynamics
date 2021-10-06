# enter dat (df with 2 columns, second column is threshold var)
# To get the TVAR model as it is in our paper, see default
# To get the same models Madhyastha used enter
# for bilinear model: alpha = 1, phi = 1, beta = 2
# for ojive model:    alpha = 2, phi = 1, beta = 0
# for combi model:    alpha = 2, phi = 1, beta = 2
estTAR <- function(data, alpha = 2, phi = 2, beta = 2) {

  dat <- data
  
  nTotal <- nrow(dat)
  
  dat$alpha <- rep(1, nTotal)           # dummy variable for the intercept
  dat$phi   <- c(NA, dat[-nTotal, 1])   # lag y variable
  dat$beta  <- c(NA, dat[-nTotal, 2])   # lag x variable
  dat       <- na.omit(dat)             # remove first observations with NAs
  
  dat <- dat[order(dat$beta), ] # order the data based on x_t-1
  
  minObs <- round(nTotal / 10)   # minimum number of observations in regime 1
  maxObs <- nTotal - minObs      # maximum number of observations in regime 1
  nObs   <- minObs:maxObs   # all possible number of observations in regime 1
  nIter  <- length(nObs)    # number of iterations
  
  tau <- dat$beta[nObs] # get all possible threshold values (based on x_t-1)
  
  sav <- vector(mode = "list", length = nIter) # empty list to store results
  ll  <- numeric(nIter) # empty vector to store log likelihood

  n   <- nrow(dat) # total number of observations without NAs
  
  # create formula for the model (dependent on which parameter is regime-switching)
  dep <- colnames(dat)[1]
  f     <- "~ 0"
  if(alpha == 1) 
    f_alpha <- "alpha" else f_alpha <- "alpha + alpha_2"
  if(phi == 1)
    f_phi <- "phi" else f_phi <- "phi_1 + phi_2"
  if(beta == 1)
    f_beta <- "beta" else f_beta <- "beta_1 + beta_2"
  
  # check if beta should be in the formula
  if(beta != 0)
    f <- as.formula(paste(dep, f, "+", f_alpha, "+", f_phi, "+", f_beta)) else
      f <- as.formula(paste(dep, f, "+", f_alpha, "+", f_phi))

  for(i in 1:nIter) {
    
    # number of observations in regime 1 and regime 2
    n_1 <- nObs[i]
    n_2 <- n - n_1
    
    # which observations are in regimes
    reg_1 <- dat$beta <= tau[i]
    reg_2 <- dat$beta > tau[i]
    
    # dummy variable for intercept per regime
    dat$alpha_2 <- as.numeric(reg_2)
    
    # semi-dummy variable for phi per regime
    dat$phi_1 <- reg_1 * dat$phi
    dat$phi_2 <- reg_2 * dat$phi
    
    # semi-dummy variable for beta per regime
    dat$beta_1 <- reg_1 * dat$beta
    dat$beta_2 <- reg_2 * dat$beta
    
    # estimate model without intercept with dummy variables above
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
  
  reg <- as.numeric(data[, 2] < tau[j]) + 1
  
  pred <- numeric()
  pred[as.numeric(names(sav[[j]]$fitted.values))] <- sav[[j]]$fitted.values
  
  return(list(threshold = tau[j], result = sav[[j]], regime = reg, predicted = pred))
}

