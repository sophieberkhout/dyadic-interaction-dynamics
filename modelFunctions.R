VAR1 <- function(occasions, params_y, params_x, z){
  
  alpha_y <- params_y[[1]]
  phi_y   <- params_y[[2]]
  beta_y  <- params_y[[3]]
  
  alpha_x <- params_x[[1]]
  phi_x   <- params_x[[2]]
  beta_x  <- params_x[[3]]
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  for(t in 2:occasions){
    y[t] <- alpha_y + phi_y * y[t-1] + beta_y * x[t-1] + z$y[t]
    x[t] <- alpha_x + phi_x * x[t-1] + beta_x * y[t-1] + z$x[t]
  }
  
  dat <- data.frame(y = y, x = x)

  return(dat)
}

change <- function(param, t, burnin = NULL){
  p <- numeric()
  
  x       <- param[[1]]
  change  <- param[[2]]
  
  if(is.null(burnin)) burnin <- 0
  for(i in 1:(t + burnin)){
    if(change == "linear"){
      j <- i-(t/2 + burnin)
      p[i] <- (j * x/(t/2))
    } else if(change == "sine"){
      p[i] <- x * sin((6 * pi * (i - burnin))/t) 
    }
  }
  return(p)
}

TVVAR1 <- function(occasions, burnin, params_y, params_x, z){
  
  ifelse(length(params_y[[1]]) == 1, 
         alpha_y <- rep(params_y[[1]], occasions), 
         alpha_y <- change(param = params_y[[1]], t = (occasions-burnin), burnin = burnin)) 
  ifelse(length(params_y[[2]]) == 1, 
         phi_y   <- rep(params_y[[2]], occasions), 
         phi_y   <- change(params_y[[2]], (occasions-burnin), burnin))
  ifelse(length(params_y[[3]]) == 1, 
         beta_y  <- rep(params_y[[3]], occasions), 
         beta_y  <- change(params_y[[3]], (occasions-burnin), burnin))
  
  ifelse(length(params_x[[1]]) == 1, 
         alpha_x <- rep(params_x[[1]], occasions), 
         alpha_x <- change(params_x[[1]], (occasions-burnin), burnin))
  ifelse(length(params_x[[2]]) == 1, 
         phi_x   <- rep(params_x[[2]], occasions), 
         phi_x   <- change(params_x[[2]], (occasions-burnin), burnin)) 
  ifelse(length(params_x[[3]]) == 1, 
         beta_x  <- rep(params_x[[3]], occasions), 
         beta_x  <- change(params_x[[3]], (occasions-burnin), burnin))
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  for(t in 2:occasions){
    y[t] <- alpha_y[t] + phi_y[t] * y[t-1] + beta_y[t] * x[t-1] + z$y[t]
    x[t] <- alpha_x[t] + phi_x[t] * x[t-1] + beta_x[t] * y[t-1] + z$x[t]
  }
  
  params <- data.frame(alpha_y, phi_y, beta_y, alpha_x, phi_x, beta_x)
  dat <- data.frame(y = y, x = x)
  dat <- cbind(dat, params[which(lengths(c(params_y, params_x)) > 1)])
  return(dat)
}

TVAR1 <- function(occasions, 
                  params_y, params_x,
                  z, longformat){

  ifelse(length(params_y[[1]]) == 1,
         alpha_y <- rep(params_y[[1]], 2),
         alpha_y <- params_y[[1]])
  ifelse(length(params_y[[2]]) == 1,
         phi_y <- rep(params_y[[2]], 2),
         phi_y <- params_y[[2]])
  ifelse(length(params_y[[3]]) == 1,
         beta_y <- rep(params_y[[3]], 2),
         beta_y <- params_y[[3]])

  ifelse(length(params_x[[1]]) == 1,
         alpha_x <- rep(params_x[[1]], 2),
         alpha_x <- params_x[[1]])
  ifelse(length(params_x[[2]]) == 1,
         phi_x <- rep(params_x[[2]], 2),
         phi_x <- params_x[[2]])
  ifelse(length(params_x[[3]]) == 1,
         beta_x <- rep(params_x[[3]], 2),
         beta_x <- params_x[[3]])

  tau_y <- if(length(params_y) == 4) params_y[[4]] else NULL
  tau_x <- if(length(params_x) == 4) params_x[[4]] else NULL
  
  if(any(lengths(params_y) > 1) && is.null(tau_y)) stop("Multiple values for a y parameter are given, but no threshold is specified.")
  if(any(lengths(params_x) > 1) && is.null(tau_x)) stop("Multiple values for an x parameter are given, but no threshold is specified.")
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  influence_y <- rep(NA, occasions)
  influence_x <- rep(NA, occasions)
  
  regime_y <- rep(NA, occasions)
  regime_x <- rep(NA, occasions)

  for(t in 2:occasions){
    ifelse(is.null(tau_y) || x[t-1] <= tau_y, s_y <- 1, s_y <- 2)
    ifelse(is.null(tau_x) || y[t-1] <= tau_x, s_x <- 1, s_x <- 2)
    
    y[t] <- alpha_y[s_y] + phi_y[s_y] * y[t-1] + beta_y[s_y] * x[t-1] + z$y[t]
    x[t] <- alpha_x[s_x] + phi_x[s_x] * x[t-1] + beta_x[s_x] * y[t-1] + z$x[t]
    
    influence_x[t-1] <- beta_y[s_y] * x[t-1]
    influence_y[t-1] <- beta_x[s_x] * y[t-1]
    
    regime_y[t] <- s_y
    regime_x[t] <- s_x
  }
  dat <- data.frame(behavior_y = y, behavior_x = x, influence_y, influence_x, regime_y, regime_x)
  if(!longformat) names(dat)[1:2] <- c("y", "x") 
  return(dat)
}

HMM1 <- function(occasions,
                 params_y, params_x,
                 probs, z){

    ifelse(is.list(params_y),
         mu_y <- params_y[[1]],
         ifelse(length(params_y) == 1,
                mu_y <- rep(params_y, 2),
                mu_y <- params_y))
  
  ifelse(is.list(params_x),
         mu_x <- params_x[[1]],
         ifelse(length(params_x) == 1,
                mu_x <- rep(params_x, 2),
                mu_x <- params_x))

  y <- numeric(occasions)
  x <- numeric(occasions) 

  # transition matrix
  p11 <- probs[1]
  p22 <- probs[2]
  p12 <- 1 - p11  
  p21 <- 1 - p22
  P <- matrix(c(p11, p12, p21, p22), 2, 2)
  
  # probability s_{t=1} = 0
  ini_p <- 0.5
  s <- sample(1:2, 1, prob = c(ini_p, 1-ini_p))
  
  regime <- s
  
  for(t in 2:occasions){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    y[t] <- mu_y[s] + z[[s]][t, 1]
    x[t] <- mu_x[s] + z[[s]][t, 2]
    
    regime[t] <- s
  }
  
  dat <- data.frame(y, x, regime = as.factor(regime))
  return(dat)
}

MSVAR1 <- function(occasions,
                   params_y, params_x,
                   probs, z){
  
  ifelse(length(params_y[[1]]) == 1,
         alpha_y <- rep(params_y[[1]], 2),
         alpha_y <- params_y[[1]])
  ifelse(length(params_y[[2]]) == 1,
         phi_y <- rep(params_y[[2]], 2),
         phi_y <- params_y[[2]])
  ifelse(length(params_y[[3]]) == 1,
         beta_y <- rep(params_y[[3]], 2),
         beta_y <- params_y[[3]])
  
  ifelse(length(params_x[[1]]) == 1,
         alpha_x <- rep(params_x[[1]], 2),
         alpha_x <- params_x[[1]])
  ifelse(length(params_x[[2]]) == 1,
         phi_x <- rep(params_x[[2]], 2),
         phi_x <- params_x[[2]])
  ifelse(length(params_x[[3]]) == 1,
         beta_x <- rep(params_x[[3]], 2),
         beta_x <- params_x[[3]])
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  # transition matrix
  p11 <- probs[1]
  p22 <- probs[2]
  p12 <- 1 - p11  
  p21 <- 1 - p22
  P <- matrix(c(p11, p12, p21, p22), 2, 2)
  
  # probability s_{t=1} = 0
  ini_p <- 0.5
  s <- sample(1:2, 1, prob = c(ini_p, 1-ini_p))
  
  regime <- s

  for(t in 2:occasions){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    y[t] <- alpha_y[s] + phi_y[s] * y[t-1] + beta_y[s] * x[t-1] + z[[s]][t, "y"]
    x[t] <- alpha_x[s] + phi_x[s] * x[t-1] + beta_x[s] * y[t-1] + z[[s]][t, "x"]
    
    regime[t] <- s
  }
  
  dat <- data.frame(y, x, regime = as.factor(regime))
  return(dat)
}