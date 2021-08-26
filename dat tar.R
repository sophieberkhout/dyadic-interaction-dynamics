source("simVARS.R")
source("plotFunctions.R")
# source("myTheme.R")
# library("ggplot2")

set.seed(1)
dat.a <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = c(0, 5),
                                 phi = 0.5,
                                 beta = 0.2,
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.5,
                                 beta = 0))

set.seed(1)
dat.b <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = 0,
                                 phi = 0.5,
                                 beta = c(0.2, 0.6),
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.5,
                                 beta = 0))

myTS(dat.a)
mySSP(dat.a, type = "carryover")
# mySSP(dat.a, type = "spillover", partner = "y")
mySSP(dat.a, type = "spillover_threshold", partner = "y", tau = 0)

myTS(dat.b)
myTS(dat.b, partner = "y", regime = T)
mySSP(dat.b, type = "carryover")
# mySSP(dat.b, type = "spillover", partner = "y")
mySSP(dat.b, type = "spillover_threshold", partner = "y", tau = 0)
myInf(dat.b, partner = "x", tau = 0)


# 
# set.seed(1)
# dat <- TAR1(t = 3000, burnin = 20,
#             us_y = c(-10, 10), us_x = 0, 
#             phi_w = 0.5, phi_h = 0.5,
#             type_u = c("step", "linear"), type_b = c("linear", "linear"),
#             betas_y = 0.2, betas_x = 0,
#             k_w = -0.1)
# 
# set.seed(1)
# t <- 3000
# k_y <- -0.1
# dat <- TAR1(t = t, burnin = 20,
#             us_y = 0, us_x = 0, 
#             phi_w = 0.5, phi_h = 0.5,
#             type_u = c("linear", "linear"), type_b = c("bilinear", "linear"),
#             betas_y = c(0.9, 0), betas_x = 0,
#             k_y = -0.1)


tarplots <- function(dat, t, k_y){
  dat$lag1 <- c(NA, dat$Behavior[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  datI <- data.frame(y = dat$Behavior[1:t], x = dat$lag1[(t+1):(t*2)],
                     g = dat$lag1[(t+1):(t*2)] <= k_y)
  
  p_so <- ggplot(datI, aes(x = x, y = y, colour = g)) + geom_point(size = 2) +
    scale_color_manual(values = c("gray50", "gray75")) +
    labs(x = expression(italic("x"[t-1])), y = expression(italic("y"[t]))) +
    geom_smooth(data = datI[datI$g == T, ], method = "lm", se = F, 
                xseq = seq(min(datI$x, na.rm = T), k_y, 0.01)) +
    geom_smooth(data = datI[datI$g == F, ], method = "lm", se = F, 
                xseq = seq(k_y, max(datI$x, na.rm = T), 0.01)) +
    geom_vline(xintercept = k_y)
  p_so <- myTheme(p_so, x = datI$x, y = datI$y, legend.position = "none")
  
  p_ar <- ggplot(dat, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
    scale_color_manual(values = c("grey", "black")) +
    labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
    geom_smooth(method = "lm", se = F, fullrange = T)
  p_ar <- myTheme(p_ar, x = dat$lag1, y = dat$Behavior)
  
  p_ts <- ggplot(dat, aes(x = t, y = Behavior, color = Partner)) + 
    geom_line() +
    labs(x = expression(italic("t")), y = "Behavior") +
    scale_color_manual(values = c("grey", "black"))
  p_ts <- myTheme(p_ts, x = dat$t, y = dat$Behavior)
  
  p_inf <- ggplot(dat[dat$Partner == "x", ], aes(x = Behavior, y = Influence)) + geom_point(size = 2) +
    geom_vline(xintercept = k_y) + geom_hline(yintercept = 0) + 
    labs(x = expression(italic("x"[t-1])), y = expression(paste(beta[y], italic("x"[t-1]))))
  p_inf <- myTheme(p_inf, 
                   x = dat[dat$Partner == "x", "Behavior"], 
                   y =  dat[dat$Partner == "x", "Behavior"])
  
  plots <- list(spillover = p_so, 
                autoregressive = p_ar, 
                timeseries = p_ts,
                influence = p_inf)
  return(plots)
}


# plot(dat[dat$Partner == "x", "Behavior"], dat[dat$Partner == "x", "Influence"])



