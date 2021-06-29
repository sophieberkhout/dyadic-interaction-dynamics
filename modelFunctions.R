symVAR1 <- function(t, burnin, 
                    alpha, phi, beta,
                    longformat = T){
  
  t_bi <- t + burnin
  
  y <- numeric(t_bi)
  x <- numeric(t_bi) 
  
  m <- matrix(c(.5, .3, .3, .5), 2, 2) # too high?
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  for(i in 2:t_bi){
    y[i] <- alpha$y + phi$y * y[i-1] + beta$y * x[i-1] + e[i, 1]
    x[i] <- alpha$x + phi$x * x[i-1] + beta$x * y[i-1] + e[i, 2]
    # y[i] <- y$alpha + y$phi * y[i-1] + y$beta * x[i-1] + e[i, 1]
    # x[i] <- u_x + phi_x * x[i-1] + beta_x * y[i-1] + e[i, 2]
    # y[i] <- u_y + phi_y * y[i-1] + beta_y * x[i-1] + e[i, 1]
    # x[i] <- u_x + phi_x * x[i-1] + beta_x * y[i-1] + e[i, 2]
  }
  
  y <- y[1:t + burnin]
  x <- x[1:t + burnin]
  
  if(longformat){
    Behavior <- c(y, x)
    Partner <- rep(c("y", "x"), each = t)
    t_total <- rep(1:t, 2)
    dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total)
  } else {
    dat <- data.frame(y = y, x = x)
  }
  return(dat)
}

# dat <- VAR1(t = 100, 
#             u_w = 0, u_h = 0, 
#             phi_w = 0.5, phi_h = 0.5, 
#             phi_wh = 0.2, phi_hw = 0.3)


I <- function(x, type, 
              k = NULL, parm = NULL){
  # if(type == "linear"){
  #   x <- phi_I * x
  # }
  if(type == "linear"){
    p <- parm
    # p <- parm * x
  }
  if(type == "bilinear"){
    # ifelse(x <= k, 
    #        x <- phi_In * x - phi_In * k, 
    #        x <- phi_Ip * x - phi_Ip * k)

    # ifelse(x <= k,
    #        p <- (parm[1] * (x - k))/x,
    #        p <- (parm[2] * (x - k))/x)
    # if(x == 0) p <- 0

    ifelse(x <= k,
           p <- parm[1] * x + k,
           p <- parm[2] * x + k)

  }
  if(type == "step"){
    ifelse(x <= k[1], 
           p <- parm[1],
           ifelse(length(k) == 1 | x <= k[2],
                  p <- parm[2],
                  p <- parm[3]))
  }
  # return(x)
  return(p)
}

TAR1 <- function(t, burnin, u_y, u_x, phi_x, phi_y, 
                 type_u, type_b, 
                 beta_y = NULL,
                 beta_x = NULL,
                 k_x = NULL, k_y = NULL){
  
  t_bi  <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi) 
  
  # e_w <- rnorm(t, sd = .1)
  # e_h <- rnorm(t, sd = .1)

  m <- matrix(c(.01, .005, .005, .01), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  I_x <- rep(NA, t_bi)
  I_y <- rep(NA, t_bi)
  
  for(i in 2:t_bi){
    # beta_y <- I(hubby[i-1], type_b[1], k_y, betas_y)
    # beta_x <- I(wifey[i-1], type_b[2], k_x, betas_x)
    # 
    # u_y <- I(hubby[i-1], type_u[1], k_y, us_y)
    # u_x <- I(wifey[i-1], type_u[2], k_x, us_x)
    # 
    # wifey[i] <- u_y + phi_x * wifey[i-1] + beta_y * hubby[i-1] + e[i, 1]
    # hubby[i] <- u_x + phi_y * hubby[i-1] + beta_x * wifey[i-1] + e[i, 2]
    # 
    # I_x[i-1] <- beta_y * hubby[i-1]
    # I_y[i-1] <- beta_x * wifey[i-1]  
    

    ifelse(is.null(k_y) || hubby[i-1] <= k_y, s_y <- 1, s_y <- 2)
    ifelse(is.null(k_x) || wifey[i-1] <= k_x, s_x <- 1, s_x <- 2)
    
    wifey[i] <- u_y[s_y] + phi_y * wifey[i-1] + beta_y[s_y] * hubby[i-1] + e[i, 1]
    hubby[i] <- u_x[s_x] + phi_x * hubby[i-1] + beta_x[s_x] * wifey[i-1] + e[i, 2]
    
    I_x[i-1] <- beta_y[s_y] * hubby[i-1]
    I_y[i-1] <- beta_x[s_x] * wifey[i-1]
    
  }
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  I <- c(I_y[1:t + burnin], I_x[1:t + burnin])
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total, Influence = I)
  return(dat)
}

# dat <- TAR1(t = 100, u_w = 0, u_h = 0, phi_w = 0.5, phi_h = 0.5, 
#             type_w = "bilinear", type_h = "step", 
#             phi_hw_p = 0.2, phi_hw_n = 0.4,
#             k_w = -0.1, k_h = 0,
#             d_h = 0.2)

# Plot influence figure
infl <- function(x, type, 
                 k = NULL, 
                 phi_Ip = NULL, phi_In = NULL, 
                 d = NULL){
  I <- numeric()
  for(i in 1:(length(x))){
  I[i] <- I_wh(x[i], type, k, phi_Ip, phi_In,  d)
  }
  return(I)
}

change <- function(x, t, burnin = NULL){
  p <- numeric()
  if(is.null(burnin)) burnin <- 0
  for(i in 1:(t + burnin)){
    if(x[1] == 1){
      j <- i-(t/2 + burnin)
      p[i] <- (j * (x[2])/(t/2))
    } else if(x[1] == 2){
      p[i] <- x[2]/2 * sin((6 * pi * (i - burnin))/t) + x[2]/2
    }
  }
  return(p)
}


TVAR1 <- function(t, burnin, tv_u_w, tv_u_h,
                  tv_phi_w, tv_phi_h,
                  tv_phi_wh, tv_phi_hw){
  
  t_bi <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi)
  
  # e_w <- rnorm(t, sd = .1)
  # e_h <- rnorm(t, sd = .1)
  
  ifelse(length(tv_u_w) == 1, 
         u_w <- rep(tv_u_w, t_bi), 
         u_w <- change(tv_u_w, t, burnin))
  ifelse(length(tv_u_h) == 1, 
         u_h <- rep(tv_u_h, t_bi), 
         u_h <- change(tv_u_h, t, burnin))
  
  ifelse(length(tv_phi_w) == 1, 
         phi_w <- rep(tv_phi_w, t_bi), 
         phi_w <- change(tv_phi_w, t, burnin))
  ifelse(length(tv_phi_h) == 1, 
         phi_h <- rep(tv_phi_h, t_bi),
         phi_h <- change(tv_phi_h, t, burnin))
  
  ifelse(length(tv_phi_wh) == 1, 
         phi_wh <- rep(tv_phi_wh, t_bi), 
         phi_wh <- change(tv_phi_wh, t, burnin))
  ifelse(length(tv_phi_hw) == 1, 
         phi_hw <- rep(tv_phi_hw, t_bi), 
         phi_hw <- change(tv_phi_hw, t, burnin))

  m <- matrix(c(.5, .3, .3, .5), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  for(i in 2:t_bi){
    wifey[i] <- u_w[i] + phi_w[i] * wifey[i-1] + phi_hw[i] * hubby[i-1] + e[i, 1]
    hubby[i] <- u_h[i] + phi_h[i] * hubby[i-1] + phi_wh[i] * wifey[i-1] + e[i, 2]
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total, u_y = rep(u_w[1:t + burnin], 2))
  return(dat)
}

# dat <- TVAR1(t = 100, 
#              tv_u_w = c(1, .1), tv_u_h = c(1, .1),
#              tv_phi_w = c(1, .6), tv_phi_h = c(1, .6),
#              tv_phi_wh = c(2, .3), tv_phi_hw = c(2, .4))



HMM <- function(t, burnin,
                m_x, m_y,
                p11, p22){
  
  t_bi  <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi) 
  
  sigma1 <- matrix(c(.5, .3, .3, .5), 2, 2)
  sigma2 <- matrix(c(.8, .3, .3, .8), 2, 2)
  e1 <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = sigma1)
  e2 <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = sigma2)
  e <- list(e1, e2)
  
  # transition matrix
  p12 <- 1 - p11  
  p21 <- 1 - p22
  P <- matrix(c(p11, p12, p21, p22), 2, 2)
  
  # probability s_{t=1} = 0
  z <- 0.5
  s <- sample(1:2, 1, prob = c(z, 1-z))
  
  save_s <- s
  
  for(i in 2:t_bi){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    wifey[i] <- m_y[s] + e[[s]][i, 1]
    hubby[i] <- m_x[s] + e[[s]][i, 2]
    
    save_s[i] <- s
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  save_s <- rep(save_s[1:t + burnin], 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total, s = save_s)
  return(dat)
}

# t <- HMM(100, 20, 
#          m_x = c(5, 10),
#          m_y = c(7, 15))
# plot.ts(t$Behavior[t$Partner == "x"])
# sum(t$s[t$Partner == "x"]-1) # how many times in regime 2
# sum(t$s[t$Partner == "y"]-1)
# var(t$Behavior[t$Partner == "y" & t$s == 2]) # variance partner=y s=2

MSVAR <- function(t, burnin,
                  c_x, c_y,
                  phi_x, phi_y,
                  beta_x, beta_y,
                  p11, p22){
  
  t_bi  <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi) 
  
  m <- matrix(c(.5, .3, .3, .5), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  # transition matrix
  p12 <- 1 - p11  
  p21 <- 1 - p22
  P <- matrix(c(p11, p12, p21, p22), 2, 2)
  
  # probability s_{t=1} = 0
  z <- 0.5
  s <- sample(1:2, 1, prob = c(z, 1-z))
  
  save_s <- s
  
  for(i in 2:t_bi){
    if(s == 1){
      new_s <- sample(1:2, 1, prob = P[, 1])
    } else if (s == 2){
      new_s <- sample(1:2, 1, prob = P[, 2])
    }
    
    s <- new_s
    
    wifey[i] <- c_y[s] + phi_y[s] * wifey[i-1] + beta_y[s] * hubby[i-1] + e[i, 1]
    hubby[i] <- c_x[s] + phi_x[s] * hubby[i-1] + beta_x[s] * wifey[i-1] + e[i, 2]
    
    save_s[i] <- s
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  save_s <- rep(save_s[1:t + burnin], 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total, s = save_s)
  return(dat)
}

# t <- MSVAR(t = 300, burnin = 20,
#             c_x = c(0, 0), c_y = c(0, 0),
#             phi_x = c(.5, .5), phi_y = c(.5, .5),
#             beta_x = c(0, 0), beta_y = c(.3, .8))
