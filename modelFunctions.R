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
  
  alpha_y <- ifelse(length(params_y[[1]]) == 1, 
                    rep(params_y[[1]], occasions), 
                    change(params_y[[1]]), (occasions-burnin), burnin) 
  phi_y   <- ifelse(length(params_y[[2]]) == 1, 
                    rep(params_y[[2]], occasions), 
                    change(params_y[[2]]), (occasions-burnin), burnin) 
  beta_y  <- ifelse(length(params_y[[3]]) == 1, 
                    rep(params_y[[3]], occasions), 
                    change(params_y[[3]]), (occasions-burnin), burnin)
  
  alpha_x <- ifelse(length(params_x[[1]]) == 1, 
                    rep(params_x[[1]], occasions), 
                    change(params_x[[1]]), (occasions-burnin), burnin) 
  phi_x   <- ifelse(length(params_x[[2]]) == 1, 
                    rep(params_x[[2]], occasions), 
                    change(params_x[[2]]), (occasions-burnin), burnin) 
  beta_x  <- ifelse(length(params_x[[3]]) == 1, 
                    rep(params_x[[3]], occasions), 
                    change(params_x[[3]]), (occasions-burnin), burnin) 
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  for(t in 2:occasions){
    y[t] <- alpha_y[t] + phi_y[t] * y[t-1] + beta_y[t] * x[t-1] + z$y[t]
    x[t] <- alpha_x[t] + phi_x[t] * x[t-1] + beta_x[t] * y[t-1] + z$x[t]
  }
  
  dat <- data.frame(y = y, x = x)
  return(dat)
}

TVAR1 <- function(occasions, 
                  params_y, params_x,
                  z){

  alpha_y <- params_y[[1]]
  phi_y   <- params_y[[2]]
  beta_y  <- params_y[[3]]
  
  alpha_x <- params_x[[1]]
  phi_x   <- params_x[[2]]
  beta_x  <- params_x[[3]]
  
  k_y <- if(length(params_y) == 4) params_y[[4]] else NULL
  k_x <- if(length(params_x) == 4) params_x[[4]] else NULL
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  I_x <- rep(NA, occasions)
  I_y <- rep(NA, occasions)
  
  for(t in 2:occasions){
    s_y <- ifelse(is.null(k_y) || hubby[t-1] <= k_y, 1, 2)
    s_x <- ifelse(is.null(k_x) || wifey[t-1] <= k_x, 1, 2)
    
    y[t] <- alpha_y[s_y] + phi_y[s_y] * y[t-1] + beta_y[s_y] * x[t-1] + z$y[t]
    x[t] <- alpha_x[s_x] + phi_x[s_x] * x[t-1] + beta_x[s_x] * y[t-1] + z$x[t]
    
    I_y[t-1] <- beta_y[s_y] * x[t-1]
    I_x[t-1] <- beta_x[s_x] * y[t-1]
    
  }
  dat <- data.frame(y, x, I_y, I_x)
  return(dat)
}

HMM1 <- function(occasions,
                 params_y, params_x,
                 probs, z){
  
  mu_y <- params_y
  mu_x <- params_x
  
  y <- numeric(occasions)
  x <- numeric(occasions) 
  
  # sigma1 <- matrix(c(.3, .1, .1, .3), 2, 2)
  # sigma2 <- matrix(c(.5, .3, .3, .5), 2, 2)
  # z1 <- MASS::mvrnorm(occasions, c(0, 0), Sigma = sigma1)
  # z2 <- MASS::mvrnorm(occasions, c(0, 0), Sigma = sigma2)
  # z <- list(z1, z2)
  
  # transition matrix
  p11 <- probs[1]
  p22 <- probs[2]
  p12 <- 1 - p11  
  p21 <- 1 - p22
  P <- matrix(c(p11, p12, p21, p22), 2, 2)
  
  # probability s_{t=1} = 0
  ini_p <- 0.5
  s <- sample(1:2, 1, prob = c(ini_p, 1-ini_p))
  
  save_s <- s
  
  for(t in 2:occasions){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    y[t] <- mu_y[s] + z[[s]][t, 1]
    x[t] <- mu_x[s] + z[[s]][t, 2]
    
    save_s[t] <- s
  }
  
  dat <- data.frame(y, x, s = save_s)
  return(dat)
}

MSVAR1 <- function(occasions,
                   params_y, params_x,
                   probs, z){
  
  alpha_y <- params_y[[1]]
  phi_y   <- params_y[[2]]
  beta_y  <- params_y[[3]]
  
  alpha_x <- params_x[[1]]
  phi_x   <- params_x[[2]]
  beta_x  <- params_x[[3]]
  
  # sigma1 <- matrix(c(.5, .3, .3, .5), 2, 2)
  # sigma2 <- matrix(c(.8, .3, .3, .8), 2, 2)
  # z1 <- MASS::mvrnorm(occasions, c(0, 0), Sigma = sigma1)
  # z2 <- MASS::mvrnorm(occasions, c(0, 0), Sigma = sigma2)
  # z <- list(z1, z2)
  
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
  
  save_s <- s

  for(t in 2:occasions){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    y[t] <- alpha_y[s] + phi_y[s] * y[t-1] + beta_y[s] * x[t-1] + z[[s]][t, "y"]
    x[t] <- alpha_x[s] + phi_x[s] * x[t-1] + beta_x[s] * y[t-1] + z[[s]][t, "x"]
    
    save_s[t] <- s
  }
  
  dat <- data.frame(y, x, s = save_s)
  return(dat)
}
