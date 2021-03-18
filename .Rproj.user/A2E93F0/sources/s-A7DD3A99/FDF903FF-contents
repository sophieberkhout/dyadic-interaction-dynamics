set.seed(1)
dat.s <- TVAR1(t = 300,
             tv_u_w = 0, tv_u_h = 0,
             tv_phi_w = .5, tv_phi_h = .5,
             tv_phi_wh = .5, tv_phi_hw = c(2, .6))
set.seed(1)
dat.l <- TVAR1(t = 300,
               tv_u_w = 0, tv_u_h = 0,
               tv_phi_w = .5, tv_phi_h = .5,
               tv_phi_wh = .5, tv_phi_hw = c(1, .6))

ps_ts <- ggplot(dat, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
ps_ts <- myTheme(ps_ts, dat.s$t, dat.s$Behavior)
ggsave("TV-VARs_ts.pdf", ps_ts , device = "pdf", width = 5, height = 3)

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
