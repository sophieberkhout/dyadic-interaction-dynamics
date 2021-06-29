library(ggplot2)
library(tidyverse)
source("modelFunctions.R")
source("myTheme.R")

set.seed(1)
dat.msvar <- MSVAR(t = 300, burnin = 20,
            c_x = c(0, 3), c_y = c(0, 3),
            phi_x = c(.5, .5), phi_y = c(.5, .5),
            beta_x = c(0, 0), beta_y = c(.1, .3),
            p11 = 0.9, p22 = 0.6)


##### VAR #####
# time-series
p_ts <- ggplot(dat.msvar, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
p_ts <- myTheme(p_ts, c(dat$t, dat.msvar$t), c(dat$Behavior, dat.msvar$Behavior))
ggsave("MSVAR_ts.pdf", p_ts , device = "pdf", width = 5, height = 3)

# scatter plot autoregression
dat.msvar$lag1 <- c(NA, dat.msvar$Behavior[-nrow(dat.msvar)])
dat.msvar$lag1[max(dat.msvar$t)+1] <- NA

p_ar <-  ggplot(dat.msvar, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  geom_smooth(method = "lm", se = F, fullrange = T)
p_ar <- myTheme(p_ar, x = c(dat$Behavior, dat.msvar$Behavior), y = c(dat$lag1, dat.msvar$lag1))
ggsave("MSVAR_ss.pdf", p_ar, device = "pdf", width = 5, height = 3)

# scatter plot spill-over h-w
p_sohw <- ggplot(dat.msvar, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
  scale_color_manual(values = c("grey", "black"), 
                     labels = c(expression(paste("x"[t], " vs ", "y"[t-1])), expression(paste("y"[t], " vs ", "x"[t-1])))) +
  labs(x = expression(italic(t-1)), y = expression(italic(t)), color = "legend") +
  geom_smooth(method = "lm", se = F, fullrange = T) 
p_sohw <- myTheme(p_sohw, x = c(dat$Behavior, dat.msvar$Behavior), y = c(dat$lag1, dat.msvar$lag1))

# dat.msvarso.8 <- dat.msvara.frame(Behavior = dat.msvar.8[dat.msvar.8$Partner == "y", "Behavior"], 
#                       lag1 = dat.msvar.8[dat.msvar.8$Partner == "x", "lag1"])
# 
# phigh_sohw <- ggplot(dat.msvarso.8, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
#   labs(x = expression(italic("x"[t-1])), y = expression(italic("y"[t]))) +
#   geom_smooth(method = "lm", se = F, col = "black", fullrange = T) 
# phigh_sohw <- myTheme(phigh_sohw, x = dat.msvarso.8$lag1, y = dat.msvarso.8$Behavior)
ggsave("MSVAR_sohw.pdf", p_sohw, device = "pdf", width = 5, height = 3)

# ccf
cc.msvar <- ccf(dat.msvar[dat.msvar$Partner == "y", "Behavior"], 
            dat.msvar[dat.msvar$Partner == "x", "Behavior"])
cc.msvar <- data.frame(Lag = cc.msvar$lag, CCF = cc.msvar$acf)
cc.msvar <- cc.msvar[which(cc.msvar$Lag == -10):which(cc.msvar$Lag == 10), ]
p_cc.msvar <- ggplot(cc.msvar, aes(x = Lag, y = CCF)) + 
  geom_linerange(dat = cc.msvar, aes(ymin = 0, ymax = CCF))
p_cc.msvar <- myTheme(p_cc.msvar, x = c(cc$Lag, cc.msvar$Lag), y = c(cc$CCF, cc.msvar$CCF, 0))
ggsave("MSVAR_ccf.pdf", p_cc.msvar, device = "pdf", width = 5, height = 3)

