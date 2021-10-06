source("symVARS.R")
source("plotFunctions.R")
# source("myTheme.R")

## NEW

set.seed(1)
alpha <- change_sine(amplitude = 2, freq = 3, phase = 1, deviation = 0, 10000)
datTVVAR <- simVARS(occasions = 10000, burnin = 20,
                    type = "TV",
                    params_y = list(alpha = alpha,
                                    phi = .5,
                                    beta = .2),
                    params_x = list(alpha = 0,
                                    phi = .5,
                                    beta = 0)
)


## OLD
set.seed(1)
dat.s <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = list(2, "sine"),
                                 phi = .5,
                                 beta = .2),
                 params_x = list(alpha = 0,
                                 phi = .5,
                                 beta = 0))

set.seed(1)
dat.l <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = list(2, "linear"),
                                 phi = .5,
                                 beta = .2),
                 params_x = list(alpha = 0,
                                 phi = .5,
                                 beta = 0))

lim <- c(min(c(dat.l$behavior, dat.s$behavior)), max(c(dat.l$behavior, dat.s$behavior)))
myTS(dat.l, ylim = lim)
mySSP(dat.l, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.l, ylim = c(0, 1))


myTS(dat.s, ylim = lim)
mySSP(dat.s, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.s, ylim = c(0, 1))
myTSsimple(dat.s$t, dat.s$alpha_y)

dat.s <- TVAR1(t = 300, burnin = 20,
             tv_u_w = c(2, 2), tv_u_h = 0,
             tv_phi_w = .5, tv_phi_h = .5,
             tv_phi_wh = .2, tv_phi_hw = .2)
set.seed(1)
dat.l <- TVAR1(t = 300, burnin = 20,
               tv_u_w = c(1, 2), tv_u_h = 0,
               tv_phi_w = .5, tv_phi_h = .5,
               tv_phi_wh = .2, tv_phi_hw = .2)




##### TV-VAR pars
phi_w1 <- change(c(1, 3), 300)

# phi_w1 <- numeric()
# for(i in 1:300){
#   phi_w1[i] <- change(c(1, 0), i, 300)
#   phi_w1[i] <- phi_w1[i] - 0.6
# }

phi_w2 <- change(c(2, .6), 300)

phi_1 <- data.frame(phi = phi_w1, t = 1:300)
phi_2 <- data.frame(phi = phi_w2, t = 1:300)

p <- ggplot(dat = phi_2, aes(x = t, y = phi)) + geom_line() +
  labs(x = expression(italic("t")), y = "Parameter") 

p <- myTheme(p, x= 1:300, y = phi_w2)
p
ggsave("TV-VAR sine.pdf", p, device = "pdf", width = 5, height = 3)

p <- ggplot(dat = phi_1, aes(x = t, y = phi)) + geom_line() +
  labs(x = expression(italic("t")), y = "Parameter") 

p <- myTheme(p, x= 1:100, y = phi_w1)
p


pl_ts <- ggplot(dat.l, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
pl_ts <- myTheme(pl_ts, dat.l$t, dat.l$Behavior)
ggsave("Plots/TV-VARl_ts.pdf", pl_ts , device = "pdf", width = 5, height = 3)

dat.l$lag1 <- c(NA, dat.l$Behavior[-nrow(dat.l)])
dat.l$lag1[max(dat.l$t)+1] <- NA

tvar.l <-  ggplot(dat.l, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  geom_smooth(method = "lm", se = F, fullrange = T)
tvar.l <- myTheme(tvar.l, x = dat.l$Behavior, y = dat.l$lag1)
ggsave("Plots/TV-VARl_ar.pdf", tvar.l, device = "pdf", width = 5, height = 3)

datso.l <- data.frame(Behavior = dat.l[dat.l$Partner == "y", "Behavior"], 
                      lag1 = dat.l[dat.l$Partner == "x", "lag1"])

tvso.l <- ggplot(datso.l, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
  labs(x = expression(italic("x"[t-1])), y = expression(italic("y"[t]))) +
  geom_smooth(method = "lm", se = F, col = "black", fullrange = T) 
tvso.l <- myTheme(tvso.l, x = datso.l$lag1, y = datso.l$Behavior)
ggsave("Plots/TV-VARl_so.pdf", tvso.l, device = "pdf", width = 5, height = 3)



ps_ts <- ggplot(dat.s, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
ps_ts <- myTheme(ps_ts, dat.s$t, dat.l$Behavior)
ggsave("Plots/TV-VARs_ts.pdf", ps_ts , device = "pdf", width = 5, height = 3)

dat.s$lag1 <- c(NA, dat.s$Behavior[-nrow(dat.s)])
dat.s$lag1[max(dat.s$t)+1] <- NA

tvar.s <-  ggplot(dat.s, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  geom_smooth(method = "lm", se = F, fullrange = T)
tvar.s <- myTheme(tvar.s, x = dat.l$Behavior, y = dat.l$lag1)
ggsave("Plots/TV-VARs_ar.pdf", tvar.s, device = "pdf", width = 5, height = 3)


datso.s <- data.frame(Behavior = dat.s[dat.s$Partner == "y", "Behavior"], 
                      lag1 = dat.s[dat.s$Partner == "x", "lag1"])

tvso.s <- ggplot(datso.s, aes(x = lag1, y = Behavior)) + geom_point(size = 2) +
  labs(x = expression(italic("x"[t-1])), y = expression(italic("y"[t]))) +
  geom_smooth(method = "lm", se = F, col = "black", fullrange = T) 
tvso.s <- myTheme(tvso.s, x = datso.l$lag1, y = datso.l$Behavior)
ggsave("Plots/TV-VARs_so.pdf", tvso.s, device = "pdf", width = 5, height = 3)




# zoomed
ps_ts1 <- ggplot(dat.s[60:90,], aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
ps_ts1 <- myTheme(ps_ts1, dat.s$t[60:90], dat.s$Behavior)

ps_ts2 <- ggplot(dat.s[210:240,], aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
ps_ts2 <- myTheme(ps_ts2, dat.s$t[210:240], dat.s$Behavior)












# scatter plot autoregression
dat.s$lag1 <- c(NA, dat.s$Behavior[-nrow(dat.s)])
dat.s$lag1[max(dat.s$t)+1] <- NA

# scatter plot spill-over h-w
ps_so <- ggplot(dat.s, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("y/x"[t-1])), y = expression(italic("x/y"[t])), color = "legend") +
  geom_smooth(se = F, fullrange = T)
ps_so <- myTheme(ps_so, x = dat.s$Behavior, y = dat.s$lag1)
ggsave("TV-VARs_so.pdf", ps_so, device = "pdf", width = 5, height = 3)

# ccf
cc.8 <- ccf(dat[dat$Partner == "y", "Behavior"], 
            dat[dat$Partner == "x", "Behavior"])
cc.8 <- data.frame(Lag = cc.8$lag, CCF = cc.8$acf)
cc.8 <- cc.8[which(cc.8$Lag == -10):which(cc.8$Lag == 10), ]
phigh_ccf <- ggplot(cc.8, aes(x = Lag, y = CCF)) + 
  geom_linerange(dat = cc.8, aes(ymin = 0, ymax = CCF))
phigh_ccf <- myTheme(phigh_ccf, x = cc.8$Lag, y = c(cc.8$CCF, cc.2$CCF, 0))
ggsave("VARhigh_ccf.pdf", phigh_ccf, device = "pdf", width = 5, height = 3)


pl_ts <- ggplot(dat.l, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
pl_ts <- myTheme(pl_ts, dat.s$t, dat.s$Behavior)
ggsave("TV-VARl_ts.pdf", pl_ts , device = "pdf", width = 5, height = 3)

# scatter plot spill-over h-w
dat.l$lag1 <- c(NA, dat.l$Behavior[-nrow(dat.l)])
dat.l$lag1[max(dat.l$t)+1] <- NA

pl_so <- ggplot(dat.l, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("y/x"[t-1])), y = expression(italic("x/y"[t])), color = "legend") +
  geom_smooth(se = F, fullrange = T)
pl_so <- myTheme(pl_so, x = dat.s$Behavior, y = dat.s$lag1)
ggsave("TV-VARl_so.pdf", pl_so, device = "pdf", width = 5, height = 3)
