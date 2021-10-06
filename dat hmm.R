# library(ggplot2)
# library(tidyverse)
# source("modelFunctions.R")
# source("myTheme.R")
source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.hmm <- simVARS(occasions = 300, burnin = 20,
                   type = "HMM", probs = c(.9, .6),
                   params_y = list(mu = c(0, 3)),
                   params_x = list(mu = c(0, 3)))

set.seed(1)
params_y <- c(0, 3)
params_x <- c(0, 3)
probs <- c(.9, .6)
datHMM <- simVARS(occasions = 100, burnin = 20,
                  type = "HMM", probs = probs,
                  params_y = params_y,
                  params_x = params_x,
                  innovations = innovations,
                  longformat = F
)

myTS(dat.hmm, regime = T, partner = "y")


myTS(dat.hmm, regime = T)
mySSP(dat.hmm, type = "carryover")
mySSP(dat.hmm, type = "spillover")
myCCF(dat.hmm)

set.seed(1)
dat <- HMM(300, 20,
           m_x = c(0, 3), m_y = c(0, 3),
           p11 = 0.9, p22 = 0.6)


##### VAR #####
# time-series
p_ts <- ggplot(dat, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
p_ts <- myTheme(p_ts, c(dat$t, dat.msvar$t), c(dat$Behavior, dat.msvar$Behavior))
ggsave("HMM_ts.pdf", p_ts , device = "pdf", width = 5, height = 3)

# scatter plot autoregression
dat$lag1 <- c(NA, dat$Behavior[-nrow(dat)])
dat$lag1[max(dat$t)+1] <- NA

p_ar <-  ggplot(dat, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  geom_smooth(method = "lm", se = F, fullrange = T)
p_ar <- myTheme(p_ar, x = c(dat$Behavior, dat.msvar$Behavior), y = c(dat$lag1, dat.msvar$lag1))
ggsave("HMM_ss.pdf", p_ar, device = "pdf", width = 5, height = 3)

# scatter plot spill-over h-w
p_sohw <- ggplot(dat, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
  scale_color_manual(values = c("grey", "black"), 
                     labels = c(expression(paste("x"[t], " vs ", "y"[t-1])), expression(paste("y"[t], " vs ", "x"[t-1])))) +
  labs(x = expression(italic(t-1)), y = expression(italic(t)), color = "legend") +
  geom_smooth(method = "lm", se = F, fullrange = T) 
p_sohw <- myTheme(p_sohw, x = c(dat$Behavior, dat.msvar$Behavior), y = c(dat$lag1, dat.msvar$lag1))

# datso.8 <- data.frame(Behavior = dat.8[dat.8$Partner == "y", "Behavior"], 
#                       lag1 = dat.8[dat.8$Partner == "x", "lag1"])
# 
# phigh_sohw <- ggplot(datso.8, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
#   labs(x = expression(italic("x"[t-1])), y = expression(italic("y"[t]))) +
#   geom_smooth(method = "lm", se = F, col = "black", fullrange = T) 
# phigh_sohw <- myTheme(phigh_sohw, x = datso.8$lag1, y = datso.8$Behavior)
ggsave("HMM_sohw.pdf", p_sohw, device = "pdf", width = 5, height = 3)

# ccf
cc <- ccf(dat[dat$Partner == "y", "Behavior"], 
            dat[dat$Partner == "x", "Behavior"])
cc <- data.frame(Lag = cc$lag, CCF = cc$acf)
cc <- cc[which(cc$Lag == -10):which(cc$Lag == 10), ]
p_ccf <- ggplot(cc, aes(x = Lag, y = CCF)) + 
  geom_linerange(dat = cc, aes(ymin = 0, ymax = CCF))
p_ccf <- myTheme(p_ccf, x = c(cc$Lag, cc.msvar$Lag), y = c(cc$CCF, cc.msvar$CCF, 0))
ggsave("HMM_ccf.pdf", p_ccf, device = "pdf", width = 5, height = 3)


cc <- ccf(dat[dat$Partner == "x", "Behavior"], 
          dat[dat$Partner == "y", "Behavior"])