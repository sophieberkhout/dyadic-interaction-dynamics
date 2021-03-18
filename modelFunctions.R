VAR1 <- function(t, burnin, u_w, u_h, phi_w, phi_h, phi_wh, phi_hw){
  
  t_bi <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi) 
  
  # e_w <- rnorm(t, sd = sqrt(.3)) # should these be correlated?
  # e_h <- rnorm(t, sd = sqrt(.3))
  
  m <- matrix(c(.5, .3, .3, .5), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  for(i in 2:t_bi){
    wifey[i] <- u_w + phi_w * wifey[i-1] + phi_hw * hubby[i-1] + e[i, 1]
    hubby[i] <- u_h + phi_h * hubby[i-1] + phi_wh * wifey[i-1] + e[i, 2]
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total)
  return(dat)
}

# dat <- VAR1(t = 100, 
#             u_w = 0, u_h = 0, 
#             phi_w = 0.5, phi_h = 0.5, 
#             phi_wh = 0.2, phi_hw = 0.3)

I_wh <- function(x, type, 
                 # phi_I = NULL, 
                 k = NULL, phi_Ip = NULL, phi_In = NULL, 
                 d = NULL){
  # if(type == "linear"){
  #   x <- phi_I * x
  # }
  if(type == "no") x <- phi_Ip * x
  if(type == "bilinear"){
    ifelse(x <= k, 
           x <- phi_In * x - phi_In * k, 
           x <- phi_Ip * x - phi_Ip * k)
  }
  if(type == "step"){
    ifelse(x <= k, 
           x <- d, 
           x <- 0) 
  }
  return(x)
}

TAR1 <- function(t, burnin, u_w, u_h, phi_w, phi_h, 
                 type_w, type_h, 
                 phi_wh_p = NULL, phi_wh_n = NULL,
                 phi_hw_p = NULL, phi_hw_n = NULL,
                 k_w = NULL, k_h = NULL,
                 d_w = NULL, d_h = NULL){
  
  t_bi  <- t + burnin
  
  wifey <- numeric(t_bi)
  hubby <- numeric(t_bi) 
  
  # e_w <- rnorm(t, sd = .1)
  # e_h <- rnorm(t, sd = .1)

  m <- matrix(c(.5, .3, .3, .5), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
    
  for(i in 2:t_bi){
    wifey[i] <- u_w + phi_w * wifey[i-1] + I_wh(hubby[i-1], type_w, k_w, phi_hw_p, phi_hw_n, d_w) + e[i, 1]
    hubby[i] <- u_h + phi_h * hubby[i-1] + I_wh(wifey[i-1], type_h, k_h, phi_wh_p, phi_wh_n, d_h) + e[i, 2]
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total)
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
      p[i] <- ((i - burnin) * (x[2]*2)/t) - x[2]
    } else if(x[1] == 2){
      p[i] <- x[2] * sin((2 * pi * (i - burnin))/t)
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
         u_w <- tv_u_w, u_w <- change(tv_u_w, i, t, burnin))
  ifelse(length(tv_u_h) == 1, 
         u_h <- tv_u_h, u_h <- change(tv_u_h, i, t, burnin))
  
  ifelse(length(tv_phi_w) == 1, 
         phi_w <- tv_phi_w, phi_w <- change(tv_phi_w, i, t, burnin))
  ifelse(length(tv_phi_h) == 1, 
         phi_h <- tv_phi_h, phi_h <- change(tv_phi_h, i, t, burnin))
  
  ifelse(length(tv_phi_wh) == 1, 
         phi_wh <- tv_phi_wh, phi_wh <- change(tv_phi_wh, i, t, burnin))
  ifelse(length(tv_phi_hw) == 1, 
         phi_hw <- tv_phi_hw, phi_hw <- change(tv_phi_hw, i, t, burnin))
  
  m <- matrix(c(.5, .3, .3, .5), 2, 2)
  e <- MASS::mvrnorm(t_bi, c(0, 0), Sigma = m)
  
  for(i in 2:t_bi){
    wifey[i] <- u_w + phi_w * wifey[i-1] + phi_hw * hubby[i-1] + e[i, 1]
    hubby[i] <- u_h + phi_h * hubby[i-1] + phi_wh * wifey[i-1] + e[i, 2]
  }
  
  Behavior <- c(wifey[1:t + burnin], hubby[1:t + burnin])
  Partner <- rep(c("y", "x"), each = t)
  t_total <- rep(1:t, 2)
  dat <- data.frame(Partner = Partner, Behavior = Behavior, t = t_total)
  return(dat)
}

# dat <- TVAR1(t = 100, 
#              tv_u_w = c(1, .1), tv_u_h = c(1, .1),
#              tv_phi_w = c(1, .6), tv_phi_h = c(1, .6),
#              tv_phi_wh = c(2, .3), tv_phi_hw = c(2, .4))
