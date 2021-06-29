###### TAR
source("modelFunctions.R")
source("myTheme.R")
library("ggplot2")
set.seed(1)
dat1 <- TAR1(t = 300, burnin = 20,
             # linear means no threshold
             # bilinear means two spillover parameter values and one threshold
             # step means two or three values and one or two thresholds
             type_u = c("linear", "linear"),   # intercept y and x both linear
             type_b = c("bilinear", "linear"), # spillover x to  y bilinear, and y to x linear          
             us_y = 0,                         # intercept y = 0
             us_x = 0,                         # intercept x = 0
             phi_y = 0.5,                      # autoregression y = 0.5
             phi_x = 0.5,                      # autoregression x = 0.5
             betas_y = c(0.2, 0.6),            # spillover x to y is 0.2 or 0.6 depending on threshold
             betas_x = 0,                      # spillover y to x is 0
             k_y = 0)                          # threshold y is 0

daty <- dat1[dat1$Partner == "y", ]
daty$lag1 <- c(NA, daty$Behavior[-nrow(daty)])

datyl <- daty[1:30, ]
p_tsl <- ggplot(datyl, aes(x = t, y = Behavior)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") 
p_tsl <- myTheme(p_tsl, x = datyl$t, y = datyl$Behavior)
p_tsl

datyr <- daty[271:300, ]
p_tsr <- ggplot(datyr, aes(x = t, y = Behavior)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") 
p_tsr <- myTheme(p_tsr, x = datyr$t, y = datyr$Behavior)
p_tsr

p_arl <- ggplot(datyl, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
  geom_path() +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) 
p_arl <- myTheme(p_arl, x = datyl$lag1, y = datyl$Behavior)
p_arl

p_arr <- ggplot(datyr, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
  geom_path() +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) 
p_arr <- myTheme(p_arr, x = datyr$lag1, y = datyr$Behavior)
p_arr


######### TVVAR
### sine
set.seed(1)
tvdat <- TVAR1(t = 300, burnin = 20,
               tv_u_w = c(2, 2), tv_u_h = 0,
               tv_phi_w = .5, tv_phi_h = .5,
               tv_phi_wh = .2, tv_phi_hw = .2)
#### line
set.seed(1)
tvdat <- TVAR1(t = 300, burnin = 20,
               tv_u_w = c(1, 2), tv_u_h = 0,
               tv_phi_w = .5, tv_phi_h = .5,
               tv_phi_wh = .2, tv_phi_hw = .2)

tvdaty <- tvdat[tvdat$Partner == "y", ]
tvdaty$lag1 <- c(NA, tvdaty$Behavior[-nrow(tvdaty)])

tvdatyl <- tvdaty[138:162, ]
tvdatyl <- tvdaty[1:25, ]
tvp_tsl <- ggplot(tvdatyl, aes(x = t, y = Behavior)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") 
tvp_tsl <- myTheme(tvp_tsl, x = tvdatyl$t, y = c(tvdatyl$Behavior, tvdatyl$Behavior))
tvp_tsl

tvdatyr <- tvdaty[276:300, ]
tvdatyr <- tvdaty[26:50, ]
tvp_tsr <- ggplot(tvdatyr, aes(x = t, y = Behavior)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") 
tvp_tsr <- myTheme(tvp_tsr, x = tvdatyr$t, y = c(tvdatyl$Behavior, tvdatyr$Behavior))
tvp_tsr

tvp_arl <- ggplot(tvdatyl, aes(x = lag1, y = Behavior, colour = t)) + geom_point(size = 2) +
  geom_path() +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  scale_colour_gradient(low = "grey90", high = "black")
tvp_arl <- myTheme(tvp_arl, x = c(tvdatyl$lag1, tvdatyr$lag1), y = c(tvdatyl$Behavior, tvdatyr$Behavior),
                   legend.position = c(.15, .7))
tvp_arl

tvp_arr <- ggplot(tvdatyr, aes(x = lag1, y = Behavior, colour = t)) + geom_point(size = 2) +
  geom_path() +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  scale_colour_gradient(low = "grey90", high = "black")
tvp_arr <- myTheme(tvp_arr, x = c(tvdatyl$lag1, tvdatyr$lag1), y = c(tvdatyl$Behavior, tvdatyr$Behavior),
                   legend.position = c(.15, .7))
tvp_arr

ggsave("Plots/TV-VARs_z1.pdf", tvp_arl , device = "pdf", width = 5, height = 3)
ggsave("Plots/TV-VARs_z2.pdf", tvp_arr , device = "pdf", width = 5, height = 3)

